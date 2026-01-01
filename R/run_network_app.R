run_network_app <- function(){
# app.R — OSINT Friend Networks (v9.7)
# Changes from v9.6:
# - Step 0 loader now accepts MULTIPLE .rds files and merges them into the current session.
# - Merge rules:
#     * saved_networks: appended; auto-rename on key collision with file tag.
#     * poi_urls: first non-empty wins.
#     * vetting: precedence reject > accept > uncertain > NA.
#     * meta_df: merged by (name, profile_link, context, is_poi); non-empty values preferred; first finite lat/lng kept.
#     * geocode_cache: union of keys; keeps existing entries.
# - Step 2 vetting modal intact (View Profile / Accept / Reject / Uncertain).
# - Enrichment table still uses Scroller with paging=TRUE and hidden pagination UI.

options(shiny.maxRequestSize = 1024^3)
options(shiny.launch.browser = TRUE)

# ---- Packages: install if missing, then load quietly ----
required_pkgs <- c(
  "shiny","shinyjs","bslib","rvest","dplyr","stringr","readr","DT","purrr","htmltools",
  "tidyr","igraph","visNetwork","RColorBrewer","shinycssloaders","writexl","leaflet",
  "leaflet.extras","httr","jsonlite"
)

install_if_missing <- function(pkgs) {
  missing <- pkgs[!vapply(pkgs, requireNamespace, FUN.VALUE = logical(1), quietly = TRUE)]
  if (length(missing) > 0) {
    install.packages(missing, dependencies = TRUE)
  }
}

install_if_missing(required_pkgs)

suppressPackageStartupMessages({
  lapply(required_pkgs, function(p) library(p, character.only = TRUE))
})


app_version <- "9.8"

# ---------- Helpers ----------
esc <- function(x){
  if (is.null(x)) return("")
  x <- tryCatch(as.character(x), error = function(e) "")
  x[is.na(x)] <- ""
  htmltools::htmlEscape(x)
}

normalize_fb_url <- function(url){
  url <- as.character(url)
  if (!length(url)) return(character())
  url <- ifelse(startsWith(url, "/"), paste0("https://www.facebook.com", url), url)
  is_profilephp <- grepl("profile\\.php", url, ignore.case = TRUE)
  url <- ifelse(is_profilephp, url, sub("[#?].*$", "", url))
  url <- ifelse(is_profilephp, url, sub("/+$$", "/", paste0(url, "/")))
  url
}

looks_like_profile <- function(url){
  url <- as.character(url)
  grepl("facebook\\.com/(profile\\.php\\?id=\\d+/?$|[A-Za-z0-9._-]+/?$)", url) &
    !grepl("facebook\\.com/(help|policy|privacy|settings|login\\.php|watch|marketplace|groups|events|pages|gaming|ads|reel|stories|bookmark|search)", url)
}

auto_detect_poi <- function(html_path){
  title_txt <- tryCatch({
    doc <- read_html(html_path, options = c("RECOVER","NOERROR","NOWARNING"))
    html_text2(html_element(doc, "title"))
  }, error = function(e) NA_character_)
  if (is.na(title_txt) || !nzchar(title_txt)) return(NA_character_)
  pats <- c("(?i)friends\\s*[·|]\\s*([^|·\\-]+)",
            "(?i)friends\\s+of\\s+([^|·\\-]+)",
            "(?i)amis\\s*[·|]\\s*([^|·\\-]+)",
            "(?i)amigos\\s*[·|]\\s*([^|·\\-]+)")
  for (p in pats){
    m <- stringr::str_match(title_txt, p)
    if (!all(is.na(m)) && nzchar(trimws(m[,2]))) return(trimws(m[,2]))
  }
  NA_character_
}

extract_friends_from_html <- function(html_path){
  doc <- read_html(html_path, options = c("RECOVER","NOERROR","NOWARNING"))
  name_links <- html_elements(
    doc,
    xpath = "//span[contains(@class,'xjp7ctv')]//a[@role='link' and @tabindex='0' and contains(@href,'facebook.com')]"
  )
  if (!length(name_links)){
    return(tibble(name = character(), profile_url = character(), context = character()))
  }
  nm_vec <- html_text2(name_links)
  hrefs  <- normalize_fb_url(html_attr(name_links, "href"))
  clean_ctx <- function(x){
    if (is.null(x) || length(x) == 0) return(NA_character_)
    x <- x[1]; if (is.na(x)) return(NA_character_)
    x <- gsub("\\s+", " ", as.character(x)); x <- trimws(x)
    if (!nzchar(x)) return(NA_character_)
    drops <- c("Follow","Friends","Friend","Mutual friends","Add Friend","Message","Options","More")
    if (tolower(x) %in% tolower(drops)) return(NA_character_)
    nc <- nchar(x); if (!is.na(nc) && nc > 160) x <- substr(x, 1, 160)
    x
  }
  contexts <- purrr::map_chr(seq_along(name_links), function(i){
    a <- name_links[[i]]
    ctx1 <- tryCatch(
      html_element(a, xpath = "./ancestor::span[contains(@class,'xjp7ctv')]/following-sibling::div[1]//div[@dir='auto'][1]"),
      error = function(e) NULL
    )
    txt <- clean_ctx(if (!is.null(ctx1)) html_text2(ctx1) else NA_character_)
    if (is.na(txt)){
      ctx2 <- tryCatch(
        html_element(a, xpath = "./ancestor::div[1]/following-sibling::div[1]//div[@dir='auto'][1]"),
        error = function(e) NULL
      )
      txt <- clean_ctx(if (!is.null(ctx2)) html_text2(ctx2) else NA_character_)
    }
    if (is.na(txt)){
      near <- tryCatch(html_element(a, xpath = "./ancestor::div[1]/following-sibling::div[1]"), error = function(e) NULL)
      if (!is.null(near)){
        raw <- tryCatch(html_text2(near), error = function(e) "")
        parts <- strsplit(raw, "\\r?\\n|\\s*·\\s*|\\x{2022}|\\|{1,2}", perl = TRUE)[[1]]
        parts <- trimws(parts); parts <- parts[nchar(parts) > 1]
        drops <- tolower(c("follow","add friend","friends","mutual friends","message"))
        nm <- trimws(nm_vec[i])
        parts <- parts[!(tolower(parts) %in% drops) & tolower(parts) != tolower(nm)]
        if (length(parts)) txt <- clean_ctx(parts[1])
      }
    }
    txt
  })
  tibble(
    name = as.character(nm_vec),
    profile_url = as.character(hrefs),
    context = as.character(contexts)
  ) %>%
    filter(nchar(trimws(name)) > 0, looks_like_profile(profile_url)) %>%
    group_by(profile_url) %>% slice(1) %>% ungroup() %>%
    arrange(tolower(name))
}

poi_from_filename <- function(filename){
  if (is.null(filename) || !nzchar(filename)) return(NA_character_)
  base <- tools::file_path_sans_ext(filename)
  base <- sub("\\s*[-_]\\s*Facebook\\s*$", "", base, ignore.case = TRUE)
  trimws(base)
}

# Simple OSM geocoder (first result)
geocode_osm <- function(query){
  if (is.null(query) || !nzchar(query)) return(NULL)
  r <- tryCatch({
    httr::GET(
      url = "https://nominatim.openstreetmap.org/search",
      query = list(q = query, format = "json", addressdetails = 0, limit = 1),
      httr::user_agent("OSINT-Friend-Networks/1.0 (Shiny)")
    )
  }, error = function(e) NULL)
  if (is.null(r) || httr::http_error(r)) return(NULL)
  txt <- httr::content(r, as = "text", encoding = "UTF-8")
  js  <- tryCatch(jsonlite::fromJSON(txt), error = function(e) NULL)
  if (is.null(js) || NROW(js) < 1) return(NULL)
  list(
    lat = suppressWarnings(as.numeric(js$lat[1])),
    lon = suppressWarnings(as.numeric(js$lon[1])),
    display = js$display_name[1]
  )
}

`%||%` <- function(x, y) if (is.null(x) || (length(x) == 1 && is.na(x))) y else x

# ---------- Merge helpers for multi-session import ----------
coalesce_chr <- function(...) {
  vals <- as.character(c(...))
  vals <- vals[!is.na(vals) & nzchar(vals)]
  if (length(vals)) vals[1] else ""
}
coalesce_num <- function(...) {
  vals <- suppressWarnings(as.numeric(c(...)))
  vals <- vals[is.finite(vals)]
  if (length(vals)) vals[1] else NA_real_
}

std_meta <- function(df){
  if (is.null(df) || !is.data.frame(df) || !nrow(df)) {
    return(tibble(
      name=character(), profile_link=character(), context=character(),
      phone=character(), email=character(), species=character(),
      location=character(), lat=numeric(), lng=numeric(), is_poi=logical()
    ))
  }
  need <- c("name","profile_link","context","phone","email","species","location","lat","lng","is_poi")
  for (nm in need) if (!nm %in% names(df)) df[[nm]] <- NA
  df[need]
}

merge_meta <- function(a, b){
  bind_rows(std_meta(a), std_meta(b)) %>%
    group_by(name, profile_link, context, is_poi) %>%
    summarise(
      phone    = coalesce_chr(phone),
      email    = coalesce_chr(email),
      species  = coalesce_chr(species),
      location = coalesce_chr(location),
      lat      = coalesce_num(lat),
      lng      = coalesce_num(lng),
      .groups  = "drop"
    )
}

merge_vetting <- function(a, b){
  # precedence: reject > accept > uncertain > NA
  prec <- c(reject = 3, accept = 2, uncertain = 1)
  ids <- union(names(a), names(b))
  out <- setNames(rep(NA_character_, length(ids)), ids)
  for (id in ids){
    cand <- na.omit(c(a[id], b[id]))
    if (!length(cand)) next
    scores <- prec[cand]; scores[is.na(scores)] <- -Inf
    out[id] <- cand[ which.max(scores) ]
  }
  out
}

merge_poi_urls <- function(a, b){
  keys <- union(names(a), names(b))
  out <- list()
  for (k in keys){
    va <- a[[k]] %||% ""
    vb <- b[[k]] %||% ""
    out[[k]] <- if (nzchar(va)) va else vb
  }
  out
}

append_list_unique <- function(store, add, tag = NULL){
  if (!length(add)) return(store)
  for (nm in names(add)){
    key <- nm
    if (key %in% names(store)){
      base <- if (nzchar(tag)) paste0(nm, " — from ", tag) else paste0(nm, " (import)")
      k2 <- base; i <- 2
      while (k2 %in% names(store)) { k2 <- paste0(base, " (", i, ")"); i <- i + 1 }
      key <- k2
    }
    store[[key]] <- add[[nm]]
  }
  store
}

# ---------- Theme ----------
theme <- bs_theme(version = 5, bootswatch = "flatly", base_font = font_google("Inter"))

# ---------- UI ----------
ui <- page_fluid(
  theme = theme,
  useShinyjs(),
  tags$head(
    tags$style(HTML(
      "\n      /* Make the enrichment table body scroll inside the panel */\n      #meta_table .dataTables_scrollBody {\n        max-height: calc(72vh) !important;\n      }\n      "
    ))
  ),
  titlePanel("OSINT Friend Networks"),
  tabsetPanel(
    id = "tabs",
    tabPanel(
      title = "Step 0. Quick Start",
      fluidRow(
        column(8,
               h4("How to use"),
               tags$ol(
                 tags$li("Open a target's Facebook profile → Friends, scroll to the bottom to load all."),
                 tags$li("Browser: ", tags$b("File → Save Page As… → Webpage, Complete"), "."),
                 tags$li("Go to ", tags$b("Step 1"), " and upload the saved HTML. Verify auto-filled POI name. (Optional: paste the POI's profile URL.)"),
                 tags$li("Repeat Step 1 for as many POIs as you need."),
                 tags$li("Go to ", tags$b("Step 2"), " to build the network and vet nodes."),
                 tags$li("Go to ", tags$b("Step 3"), " to enrich POIs/Confirmed/Uncertain and map flows."),
                 tags$li("Export Maltego edges in ", tags$b("Step 4"), ".")
               ),
               tags$hr(),
               h4("Load saved sessions (multi-file)"),
               fileInput("upload_session", NULL, accept = ".rds",
                         buttonLabel = "Choose .rds…", placeholder = "No files selected",
                         multiple = TRUE),   # <-- allow multiple
               actionButton("load_session_btn", "Load & Merge", class = "btn btn-outline-primary"),
               tags$hr(),
               tags$small("Note: Session files are local .rds snapshots (no cloud storage).")
        )
      )
    ),
    tabPanel(
      title = "Step 1. Parse Friend Networks",
      tags$p("Open the person's profile → Friends, scroll to the bottom, then ",
             tags$strong("File → Save Page As… → Webpage, Complete"), "."),
      tags$hr(),
      fluidRow(
        column(5,
               h4("Upload saved Friends HTML"),
               fileInput("html_file", NULL, accept = c(".html",".htm"),
                         buttonLabel = "Choose HTML…", placeholder = "No file selected"),
               textInput("poi_name", "POI (Person of Interest) name", placeholder = "e.g., John Doe"),
               textInput("poi_url",  "POI Facebook URL (optional)", placeholder = "https://www.facebook.com/username"),
               actionButton("parse_btn", "Parse Friends", class = "btn btn-primary"),
               tags$span(style="margin-left:10px"),
               actionButton("save_btn", "Save POI network", class = "btn btn-success"),
               br(), br(),
               selectInput("saved_choice", "View a saved network", choices = character(0)),
               actionButton("load_saved", "Load selected", class = "btn btn-outline-secondary"),
               br(), br(),
               downloadButton("download_csv", "Download current table as CSV")
        ),
        column(7,
               h4("Parsed Friends"),
               DTOutput("friends_table"),
               tags$hr(),
               tags$small("Follow applicable laws and platform ToS. Geocoding uses OpenStreetMap Nominatim.")
        )
      )
    ),
    tabPanel(
      title = "Step 2. Build Network",
      fluidRow(
        column(3,
               h4("Saved POIs"),
               uiOutput("saved_picker"),
               actionLink("select_all", "Select all"), span(" | "), actionLink("clear_all", "Clear"),
               tags$hr(),
               sliderInput("threshold", "Friend appears in at least N POIs", min = 1, max = 2, value = 1, step = 1),
               selectInput("community_algo", "Community Detection",
                           choices = c("Walktrap","Louvain","Infomap","Edge betweenness"), selected = "Walktrap"),
               checkboxInput("highlight_owners", "Use diamond shape for POIs", TRUE),
               selectizeInput("highlight_ids", "Highlight nodes", choices = NULL, multiple = TRUE),
               selectizeInput("path_ids", "Shortest path (choose two nodes)", choices = NULL, multiple = TRUE, options = list(maxItems = 2)),
               br(),
               actionButton("apply_vetting", "Update Network", class = "btn btn-warning"),
               tags$span(style="margin-left:8px; color:#777;", "(removes rejected nodes)"),
               br(), br(),
               downloadButton("download_report", "Download XLSX report")
        ),
        column(9,
               h4("Network"),
               withSpinner(visNetworkOutput("network", height = "70vh"), type = 5, color = "#98A5A6")
        )
      )
    ),
    tabPanel(
      title = "Step 3. Enrich & Map",
      tabsetPanel(
        id = "enrich_tabs",
        tabPanel(
          title = "Table",
          fluidRow(
            column(4,
                   h4("Edit selected profile"),
                   uiOutput("meta_selected_name"),
                   tags$hr(),
                   textInput("meta_phone", "Phone"),
                   textInput("meta_email", "Email"),
                   textInput("meta_species", "Species Advertised"),
                   tags$div(style = "margin-top:8px;"),
                   strong("Location"),
                   div(style = "font-size: 0.9em; color:#666; margin-bottom:6px;",
                       "Click the map to drop a pin or use the search below."),
                   div(
                     class = "d-flex", style = "gap:6px; margin-bottom:6px;",
                     textInput("meta_search", NULL, placeholder = "Search address/place...", width = "100%"),
                     actionButton("meta_do_search", "Search")
                   ),
                   leafletOutput("meta_map", height = 280),
                   div(style="margin-top:6px;", uiOutput("loc_selected_label")),
                   br(),
                   actionButton("meta_save", "Save metadata", class = "btn btn-success"),
                   tags$span(style="margin-left:8px; color:#777;", "Updates the table")
            ),
            column(8,
                   h4("Profiles to enrich (POIs, Confirmed, Uncertain)"),
                   DTOutput("meta_table"),
                   br(),
                   downloadButton("meta_download_xlsx", "Download Enriched Table (XLSX)")
            )
          )
        ),
        tabPanel(
          title = "Flows Map (POI → Profile)",
          fluidRow(
            column(3,
                   h4("Legend"),
                   tags$ul(
                     tags$li(tags$span(style="color:#2b8cbe;font-weight:bold;", "Blue"), ": Confirmed"),
                     tags$li(tags$span(style="color:#a6bddb;font-weight:bold;", "Light blue"), ": Uncertain"),
                     tags$li(tags$span(style="color:#bdbdbd;font-weight:bold;", "Grey"), ": Unvetted (not mapped)"),
                     tags$li(tags$span(style="color:white;font-weight:bold;", "White"), ": POI location")
                   ),
                   checkboxInput("flows_show_points", "Show markers", TRUE),
                   checkboxInput("flows_fit_bounds", "Auto-fit to flows", TRUE),
                   br(),
                   actionButton("flows_update", "Update Map", class = "btn btn-secondary")
            ),
            column(9,
                   leafletOutput("flows_map", height = "72vh")
            )
          )
        )
      )
    ),
    tabPanel(
      title = "Step 4. Export to Maltego",
      fluidRow(
        column(5,
               h4("Export CSV for Maltego"),
               tags$p("Use in Maltego: ", tags$strong("Import Graph → 3rd Party Table"),
                      ". Map ", tags$code("Source/Target"), " to Person/Alias; map URL/context to properties."),
               checkboxInput("export_only_confirmed", "Only export Confirmed nodes (always keep POIs)", FALSE),
               checkboxInput("export_exclude_rejected", "Exclude Rejected nodes", TRUE),
               br(),
               downloadButton("download_maltego_csv", "Download Maltego CSV"),
               tags$hr(),
               h4("Save Session"),
               tags$p("Save a snapshot of your work to reload later (Step 0)."),
               downloadButton("download_session", "Save Session (.rds)")
        ),
        column(7,
               tags$h5("Columns"),
               tags$pre("Source, SourceURL, SourceContext, SourceIsPOI, SourceVetted,\nTarget, TargetURL, TargetContext, TargetIsPOI, TargetVetted, LinkLabel")
        )
      )
    )
  )
)

# ---------- Server ----------
server <- function(input, output, session){
  # ---------------- State ----------------
  parsed <- reactiveVal(tibble(POI = character(), name = character(), context = character(), profile_url = character()))
  saved_networks <- reactiveVal(list())
  
  vetting <- reactiveVal(setNames(character(0), character(0)))  # id -> "accept"|"reject"|"uncertain"
  apply_filter <- reactiveVal(FALSE)
  current_node <- reactiveVal(NULL)
  poi_urls <- reactiveVal(list())  # optional: POI name -> URL
  
  meta_df <- reactiveVal(tibble(
    name = character(), profile_link = character(), context = character(),
    phone = character(), email = character(), species = character(),
    location = character(), lat = numeric(), lng = numeric(),
    is_poi = logical()
  ))
  meta_selected_id <- reactiveVal(NULL)
  loc_value  <- reactiveVal("")
  loc_coords <- reactiveVal(c(NA_real_, NA_real_))
  geocode_cache <- reactiveVal(list())
  
  # Separate reactive for displayed enrichment table (avoid re-render on save)
  meta_table_data <- reactiveVal(tibble(
    POI=character(), Name=character(), URL=character(), Context=character(),
    Phone=character(), Email=character(), Species=character(), Location=character()
  ))
  
  # ---------------- Step 0: Load & MERGE Sessions (.rds) ----------------
  observeEvent(input$load_session_btn, {
    req(input$upload_session)
    files <- input$upload_session
    paths <- files$datapath
    fnames <- files$name
    
    cur_sn   <- saved_networks()
    cur_pu   <- poi_urls()
    cur_vt   <- vetting()
    cur_md   <- meta_df()
    cur_gcx  <- geocode_cache()
    
    withProgress(message = "Loading sessions…", value = 0, {
      n <- length(paths)
      for (i in seq_along(paths)){
        incProgress(1/n, detail = fnames[i])
        st <- tryCatch(readRDS(paths[i]), error = function(e) NULL)
        if (is.null(st) || !is.list(st)) {
          showNotification(paste("Skipped:", fnames[i], "— not a valid .rds"), type = "warning")
          next
        }
        sn  <- st$saved_networks; if (!is.list(sn)) sn <- list()
        pu  <- st$poi_urls %||% list()
        vt  <- st$vetting %||% setNames(character(0), character(0))
        md  <- st$meta_df
        gcx <- st$geocode_cache %||% list()
        
        tag <- tools::file_path_sans_ext(basename(fnames[i]))
        
        cur_sn  <- append_list_unique(cur_sn, sn, tag = tag)
        cur_pu  <- merge_poi_urls(cur_pu, pu)
        cur_vt  <- merge_vetting(cur_vt, vt)
        cur_md  <- merge_meta(cur_md, md)
        
        all_keys <- union(names(cur_gcx), names(gcx))
        merged_gcx <- cur_gcx
        for (k in all_keys) {
          if (!is.null(cur_gcx[[k]])) next
          merged_gcx[[k]] <- gcx[[k]]
        }
        cur_gcx <- merged_gcx
      }
    })
    
    saved_networks(cur_sn)
    poi_urls(cur_pu)
    vetting(cur_vt)
    meta_df(cur_md)
    geocode_cache(cur_gcx)
    
    parsed(tibble(POI = character(), name = character(), context = character(), profile_url = character()))
    reset("html_file"); updateTextInput(session, "poi_url", value = "")
    
    updateSelectInput(session, "saved_choice", choices = names(cur_sn), selected = if (length(cur_sn)) names(cur_sn)[1] else "")
    updateCheckboxGroupInput(session, "include_networks", choices = names(cur_sn), selected = names(cur_sn))
    
    md <- cur_md
    if (nrow(md)) {
      out <- md %>% arrange(desc(is_poi), name) %>%
        transmute(
          POI = ifelse(is_poi, "Yes", ""),
          Name = name,
          URL  = profile_link,
          Context = context,
          Phone = phone,
          Email = email,
          Species = species,
          Location = location
        )
      meta_table_data(out)
    } else {
      meta_table_data(meta_table_data()[0,])
    }
    
    showNotification(paste0("Loaded & merged ", length(paths), " session file", ifelse(length(paths)>1,"s",""), "."), type = "message", duration = 6)
  }, ignoreInit = TRUE)
  
  # ---------------- Step 1: upload -> suggest POI ----------------
  observeEvent(input$html_file, {
    req(input$html_file)
    fname_poi <- poi_from_filename(input$html_file$name)
    if (!is.na(fname_poi) && nzchar(fname_poi)){
      updateTextInput(session, "poi_name", value = fname_poi)
      showNotification(paste("POI from file name:", fname_poi), type = "message", duration = 3)
    }
    auto <- auto_detect_poi(input$html_file$datapath)
    if (!is.na(auto) && nzchar(auto) && !identical(auto, fname_poi)){
      showNotification(paste("Page title suggests:", auto, "— using file name by default (edit if needed)."),
                       type = "message", duration = 5)
    }
  }, ignoreInit = TRUE)
  
  observeEvent(input$parse_btn, {
    req(input$html_file)
    poi <- trimws(input$poi_name %||% "")
    if (!nzchar(poi)) { showNotification("Please enter a POI name before parsing.", type = "warning"); return(NULL) }
    tryCatch({
      res <- extract_friends_from_html(input$html_file$datapath)
      if (nrow(res) == 0){
        parsed(tibble(POI = character(), name = character(), context = character(), profile_url = character()))
        showNotification("No friend entries detected. Ensure you fully scrolled and saved 'Webpage, Complete'.", type = "warning")
      } else {
        res <- res %>% mutate(POI = poi) %>% relocate(POI, name, context, profile_url)
        parsed(res)
        showNotification(paste("Parsed", nrow(res), "friends for POI:", poi), type = "message")
      }
    }, error = function(e){
      showNotification(paste("Parsing failed:", conditionMessage(e)), type = "error")
    })
  })
  
  observeEvent(input$save_btn, {
    dat <- parsed()
    shiny::validate(shiny::need(nrow(dat) > 0, "No parsed data to save. Parse a file first."))
    poi <- unique(dat$POI)
    key <- paste0(poi, " — ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
    store <- saved_networks(); store[[key]] <- dat; saved_networks(store)
    
    purls <- poi_urls()
    if (nzchar(input$poi_url)) purls[[poi]] <- input$poi_url
    poi_urls(purls)
    
    updateSelectInput(session, "saved_choice", choices = names(store), selected = key)
    reset("html_file"); updateTextInput(session, "poi_url", value = "")
    parsed(tibble(POI = character(), name = character(), context = character(), profile_url = character()))
    showNotification(paste("Saved network:", key, "— upload the next POI or go to Step 2 when ready."), type = "message")
  })
  
  observeEvent(input$load_saved, {
    choice <- input$saved_choice; store <- saved_networks()
    if (nzchar(choice) && choice %in% names(store)) { parsed(store[[choice]]); showNotification(paste("Loaded:", choice), type = "message") }
    else { showNotification("Select a saved network first.", type = "warning") }
  })
  
  output$friends_table <- renderDT({
    dat <- parsed()
    if (nrow(dat) == 0) return(DT::datatable(tibble(), options = list(dom = 't')))
    DT::datatable(
      dat, rownames = FALSE, extensions = c('Buttons'),
      options = list(pageLength = 25, scrollX = TRUE, dom = 'Bfrtip',
                     buttons = list(list(extend = "csv", text = "Download CSV",
                                         title = paste0("facebook_friends_", format(Sys.time(), "%Y%m%d_%H%M%S"))))),
      escape = FALSE
    )
  })
  
  output$download_csv <- downloadHandler(
    filename = function() paste0("facebook_friends_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv"),
    content = function(file){
      dat <- parsed()
      shiny::validate(shiny::need(nrow(dat) > 0, "No data to export. Parse a file first."))
      write_csv(dat, file)
    }
  )
  
  # ---------------- Step 2: saved picker ----------------
  output$saved_picker <- renderUI({
    store <- saved_networks(); keys <- names(store)
    if (!length(keys)) tags$em("No saved POI networks yet. Use Step 1 to add some.")
    else checkboxGroupInput("include_networks", NULL, choices = keys, selected = keys)
  })
  observeEvent(input$select_all, { store <- saved_networks(); updateCheckboxGroupInput(session, "include_networks", selected = names(store)) })
  observeEvent(input$clear_all, { updateCheckboxGroupInput(session, "include_networks", selected = character(0)) })
  
  data_from_saved <- reactive({
    store <- saved_networks(); sel <- input$include_networks
    shiny::validate(
      shiny::need(length(store) > 0, "No saved networks found."),
      shiny::need(length(sel)  > 0, "Select at least one saved network.")
    )
    bind_rows(store[sel]) %>%
      rename(account_owner = POI, profile_name = name, profile_link = profile_url) %>%
      mutate(account_owner = as.character(account_owner), profile_name = as.character(profile_name))
  })
  
  observe({
    store <- saved_networks()
    if (!length(store)) return()
    df <- tryCatch(data_from_saved(), error = function(e) NULL)
    if (!is.null(df) && nrow(df)){
      nPOI <- length(unique(df$account_owner))
      updateSliderInput(session, "threshold", min = 1, max = max(2, nPOI), value = min(2, nPOI), step = 1)
    }
  })
  
  first_non_na <- function(x){ x <- x[!is.na(x) & nzchar(x)]; if (length(x)) x[[1]] else NA_character_ }
  
  network_data <- reactive({
    df <- data_from_saved()
    shiny::validate(shiny::need(nrow(df) > 0, "No rows in selected networks."))
    
    vstat <- vetting()
    if (isTRUE(apply_filter())){
      rejected <- names(vstat)[!is.na(vstat) & vstat == "reject"]
      if (length(rejected)) df <- df %>% filter(!(profile_name %in% rejected))
    }
    
    bipartite_edges <- df %>% select(account_owner, profile_name, profile_link, context)
    
    friend_counts <- bipartite_edges %>%
      distinct(account_owner, profile_name, profile_link, context) %>%
      group_by(profile_name, profile_link) %>% summarise(count = n(), .groups = "drop")
    
    filtered_friends <- friend_counts %>% filter(count >= input$threshold)
    shiny::validate(shiny::need(nrow(filtered_friends) > 0, "No friends meet the selected threshold."))
    
    filtered_edges <- bipartite_edges %>%
      semi_join(filtered_friends, by = c("profile_name","profile_link")) %>%
      distinct(account_owner, profile_name, .keep_all = TRUE)
    
    g <- graph_from_data_frame(filtered_edges, directed = FALSE)
    
    comm <- switch(input$community_algo,
                   "Walktrap"=cluster_walktrap(g),
                   "Louvain"=cluster_louvain(g),
                   "Infomap"=cluster_infomap(g),
                   "Edge betweenness"=cluster_edge_betweenness(g),
                   cluster_walktrap(g))
    V(g)$community <- membership(comm)
    
    nodes <- data.frame(
      id = V(g)$name,
      label = V(g)$name,
      community = as.factor(V(g)$community),
      stringsAsFactors = FALSE
    )
    
    friend_info <- filtered_edges %>%
      group_by(profile_name) %>%
      summarise(profile_link = first_non_na(profile_link),
                context = first_non_na(context),
                .groups = "drop")
    nodes <- left_join(nodes, friend_info, by = c("id" = "profile_name"))
    nodes$context      <- as.character(nodes$context)
    nodes$profile_link <- as.character(nodes$profile_link)
    
    owners <- unique(filtered_edges$account_owner)
    nodes$is_poi <- nodes$id %in% owners
    purls <- poi_urls()
    if (length(purls)){
      idx <- which(nodes$is_poi)
      if (length(idx)){
        for (i in idx){
          nm <- nodes$id[i]
          if (nzchar(purls[[nm]] %||% "")) nodes$profile_link[i] <- purls[[nm]]
        }
      }
    }
    
    nodes$title <- ifelse(nodes$is_poi, "<strong>POI</strong>",
                          ifelse(!is.na(nodes$context) & nzchar(nodes$context), esc(nodes$context), ""))
    
    deg <- degree(g); minD <- min(deg); maxD <- max(deg)
    nodes$size <- if (maxD == minD) rep(28, length(deg)) else 12 + (deg-minD)/(maxD-minD) * (46-12)
    nodes$shape <- ifelse(nodes$is_poi & isTRUE(input$highlight_owners), "diamond", "dot")
    
    v <- vetting(); nodes$vstat <- as.character(v[as.character(nodes$id)])
    
    edges <- as.data.frame(get.edgelist(g)); colnames(edges) <- c("from","to"); edges$width <- 1
    list(nodes = nodes, edges = edges, g = g, owners = owners)
  })
  
  # Keep selectors synced
  observe({
    nd <- tryCatch(network_data(), error = function(e) NULL)
    if (!is.null(nd)){
      ch <- nd$nodes$id
      updateSelectizeInput(session, "highlight_ids", choices = ch, server = TRUE)
      updateSelectizeInput(session, "path_ids",      choices = ch, server = TRUE)
    }
  })
  observe({ if (!is.null(input$path_ids) && length(input$path_ids) > 0) { updateSelectizeInput(session, "highlight_ids", selected = character(0)); disable("highlight_ids") } else { enable("highlight_ids") } })
  observe({ if (!is.null(input$highlight_ids) && length(input$highlight_ids) > 0) { updateSelectizeInput(session, "path_ids", selected = character(0)); disable("path_ids") } else { enable("path_ids") } })
  
  observeEvent(input$apply_vetting, {
    apply_filter(TRUE)
    showNotification("Network updated: rejected nodes removed.", type = "message")
  })
  
  # Colors
  COL_GREY_BG <- "#bdbdbd"
  COL_GREY_BR <- "#636363"
  COL_POI_BG  <- "#ffffff"
  COL_POI_BR  <- "#000000"
  COL_ACC_BG  <- "#2b8cbe"
  COL_ACC_BR  <- "#084081"
  COL_UNC_BG  <- "#a6bddb"
  COL_UNC_BR  <- "#2b8cbe"
  COL_REJ_BG  <- "#e34a33"
  COL_REJ_BR  <- "#990000"
  
  output$network <- renderVisNetwork({
    nd <- network_data()
    nodes <- nd$nodes; edges <- nd$edges; g <- nd$g
    
    nodes$color.background <- ifelse(nodes$is_poi, COL_POI_BG, COL_GREY_BG)
    nodes$color.border     <- ifelse(nodes$is_poi, COL_POI_BR, COL_GREY_BR)
    edges$color <- "#9e9e9e"
    
    if (!is.null(input$highlight_ids) && length(input$highlight_ids) > 0){
      highlight_set <- unique(unlist(lapply(input$highlight_ids, function(id){
        nbrs <- neighbors(g, id, mode = "all"); c(id, igraph::as_ids(nbrs))
      })))
      nodes$color.background[!(nodes$id %in% highlight_set)] <- ifelse(nodes$is_poi[!(nodes$id %in% highlight_set)], COL_POI_BG, "#d9d9d9")
      nodes$color.border[!(nodes$id %in% highlight_set)]     <- ifelse(nodes$is_poi[!(nodes$id %in% highlight_set)], COL_POI_BR, "#bdbdbd")
    }
    
    if (!is.null(input$path_ids) && length(input$path_ids) == 2){
      sp <- igraph::shortest_paths(g, from = input$path_ids[1], to = input$path_ids[2], output = "both")
      sp_nodes <- igraph::as_ids(sp$vpath[[1]]); sp_edge_ids <- unlist(sp$epath[[1]])
      nodes$color.background[nodes$id %in% sp_nodes] <- "#ffd54f"
      nodes$color.border[nodes$id %in% sp_nodes]     <- "#ff6f00"
      if (length(sp_edge_ids) > 0){ edges$color[sp_edge_ids] <- "#ff6f00"; edges$width[sp_edge_ids] <- 3 }
      non_sp_edges <- setdiff(seq_len(nrow(edges)), sp_edge_ids); edges$color[non_sp_edges] <- "#d0d0d0"
    }
    
    v <- nodes$vstat
    nodes$color.background[!is.na(v) & v == "accept"]    <- COL_ACC_BG
    nodes$color.border[!is.na(v) & v == "accept"]        <- COL_ACC_BR
    nodes$color.background[!is.na(v) & v == "uncertain"] <- COL_UNC_BG
    nodes$color.border[!is.na(v) & v == "uncertain"]     <- COL_UNC_BR
    nodes$color.background[!is.na(v) & v == "reject"]    <- COL_REJ_BG
    nodes$color.border[!is.na(v) & v == "reject"]        <- COL_REJ_BR
    
    visNetwork(nodes, edges) %>%
      visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
      visPhysics(solver = "forceAtlas2Based", stabilization = TRUE) %>%
      visEvents(
        select = htmlwidgets::JS(
          "function(params){ if(params && params.nodes && params.nodes.length){ Shiny.setInputValue('node_click', params.nodes[0], {priority: 'event'}); } }"
        )
      )
  })
  
  # --- Step 2: Node modal + vetting actions ---
  observeEvent(input$node_click, {
    req(input$node_click)
    nd <- tryCatch(network_data(), error = function(e) NULL); req(!is.null(nd))
    node <- nd$nodes[nd$nodes$id == input$node_click, , drop = FALSE]; req(nrow(node) == 1)
    
    nm     <- node$label %||% node$id
    ctx    <- node$context %||% NA_character_
    link   <- node$profile_link %||% NA_character_
    is_poi <- isTRUE(node$is_poi)
    
    current_node(as.character(node$id))
    js_url <- gsub("'", "\\'", link, fixed = TRUE)
    
    showModal(
      modalDialog(
        easyClose = TRUE, footer = NULL,
        div(
          h4(esc(nm), style = "margin-top:0;"),
          if (!is.na(ctx) && nzchar(ctx)) tags$p(tags$em(esc(ctx))),
          if (is_poi) tags$p(tags$small(tags$b("POI (central node)"))),
          tags$hr(),
          div(
            class = "btn-toolbar", style = "gap:8px; display:flex; flex-wrap:wrap;",
            if (!is.na(link) && nzchar(link)) tags$button(
              type = "button", class = "btn btn-primary",
              onclick = sprintf("window.open('%s','_blank','noopener');", js_url), "View Profile"
            ),
            if (!isTRUE(is_poi)) actionButton("confirm_node",   label = tagList(icon("thumbs-up"), "Accept"),   class = "btn btn-primary"),
            if (!isTRUE(is_poi)) actionButton("reject_node",    label = tagList(icon("times"),     "Reject"),    class = "btn btn-danger"),
            if (!isTRUE(is_poi)) actionButton("uncertain_node", label = tagList(icon("question"),  "Uncertain"), class = "btn btn-info")
          )
        )
      )
    )
  }, ignoreInit = TRUE)
  
  observeEvent(input$confirm_node, {
    id <- current_node(); req(id)
    v <- vetting(); v[id] <- "accept"; vetting(v)
    removeModal()
  }, ignoreInit = TRUE)
  
  observeEvent(input$reject_node, {
    id <- current_node(); req(id)
    v <- vetting(); v[id] <- "reject"; vetting(v)
    removeModal()
  }, ignoreInit = TRUE)
  
  observeEvent(input$uncertain_node, {
    id <- current_node(); req(id)
    v <- vetting(); v[id] <- "uncertain"; vetting(v)
    removeModal()
  }, ignoreInit = TRUE)
  # --- End Step 2 modal block ---
  
  # ---------------- Step 3: Enrichment (Table) ----------------
  observeEvent(list(network_data(), vetting()), {
    nd <- tryCatch(network_data(), error = function(e) NULL)
    if (is.null(nd)) {
      meta_df(tibble(name=character(), profile_link=character(), context=character(),
                     phone=character(), email=character(), species=character(),
                     location=character(), lat=numeric(), lng=numeric(), is_poi=logical()))
      meta_table_data(meta_table_data()[0,])
      return()
    }
    nodes <- nd$nodes
    to_edit <- nodes$is_poi | (!is.na(nodes$vstat) & nodes$vstat %in% c("accept","uncertain"))
    base <- nodes[to_edit, c("id","profile_link","context","is_poi")]
    names(base) <- c("name","profile_link","context","is_poi")
    base$name <- as.character(base$name); base$profile_link <- as.character(base$profile_link); base$context <- as.character(base$context)
    
    cur <- meta_df()
    merged <- base %>%
      left_join(cur, by = c("name","profile_link","context","is_poi")) %>%
      mutate(
        phone   = as.character(phone %||% ""),
        email   = as.character(email %||% ""),
        species = as.character(species %||% ""),
        location= as.character(location %||% ""),
        lat     = suppressWarnings(as.numeric(lat)),
        lng     = suppressWarnings(as.numeric(lng))
      ) %>%
      arrange(desc(is_poi), name)
    meta_df(merged)
    
    out <- merged %>% transmute(
      POI = ifelse(is_poi, "Yes", ""),
      Name = name,
      URL = profile_link,
      Context = context,
      Phone = phone,
      Email = email,
      Species = species,
      Location = location
    )
    meta_table_data(out)
  }, ignoreInit = FALSE)
  
  output$meta_table <- renderDT({
    out <- meta_table_data()
    if (!nrow(out)) out <- meta_table_data()[0,]
    DT::datatable(
      out,
      selection = "single",
      rownames  = FALSE,
      extensions = c('Scroller'),
      options = list(
        scrollY = 400,
        scroller = TRUE,
        deferRender = TRUE,
        paging = TRUE,
        stateSave = TRUE,
        dom = "fti",
        scrollX = TRUE
      )
    )
  }, server = TRUE)
  
  meta_proxy <- dataTableProxy("meta_table")
  
  observeEvent(input$meta_table_rows_selected, {
    s <- input$meta_table_rows_selected
    out <- meta_table_data()
    df  <- meta_df()
    if (length(s) != 1 || !nrow(out) || !nrow(df)) return()
    key_name <- out$Name[s]; key_url <- out$URL[s]
    row <- df %>% filter(name == key_name, profile_link == key_url) %>% slice(1)
    if (!nrow(row)) return()
    meta_selected_id(row$name)
    updateTextInput(session, "meta_phone",   value = row$phone %||% "")
    updateTextInput(session, "meta_email",   value = row$email %||% "")
    updateTextInput(session, "meta_species", value = row$species %||% "")
    loc_value(row$location %||% "")
    loc_coords(c(row$lat %||% NA_real_, row$lng %||% NA_real_))
  }, ignoreInit = TRUE)
  
  output$meta_selected_name <- renderUI({
    id <- meta_selected_id()
    if (is.null(id) || !nzchar(id)) return(tags$em("Select a row to edit."))
    tags$div(tags$strong("Selected:"), " ", esc(id))
  })
  
  # Map for editing a single record
  output$meta_map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = 10, lat = 20, zoom = 2)
  })
  
  observe({
    coords <- loc_coords()
    proxy <- leafletProxy("meta_map")
    proxy %>% clearMarkers()
    if (all(is.finite(coords))){
      proxy %>% addMarkers(lng = coords[2], lat = coords[1])
      proxy %>% setView(lng = coords[2], lat = coords[1], zoom = 8)
    }
  })
  
  observeEvent(input$meta_map_click, {
    click <- input$meta_map_click
    if (is.null(click)) return()
    lat <- click$lat; lng <- click$lng
    loc_coords(c(lat, lng))
    loc_value(sprintf("%.5f, %.5f", lat, lng))
  })
  
  observeEvent(input$meta_do_search, {
    shinyjs::disable("meta_do_search")
    on.exit(shinyjs::enable("meta_do_search"), add = TRUE)
    q <- trimws(input$meta_search %||% "")
    if (!nzchar(q)) return()
    cache <- geocode_cache()
    key <- tolower(q)
    res <- cache[[key]]
    if (is.null(res)) {
      res <- geocode_osm(q)
      cache[[key]] <- res
      geocode_cache(cache)
    }
    if (is.null(res) || !is.finite(res$lat) || !is.finite(res$lon)){
      showNotification("No results for that place. Try a more specific query.", type = "warning")
      return()
    }
    loc_coords(c(res$lat, res$lon))
    loc_value(res$display %||% sprintf("%.5f, %.5f", res$lat, res$lon))
  }, ignoreInit = TRUE)
  
  output$loc_selected_label <- renderUI({
    val <- loc_value()
    if (!is.null(val) && nzchar(val)) HTML(paste0("<strong>Selected:</strong> ", esc(val)))
    else HTML("<em>No location selected.</em>")
  })
  
  observeEvent(input$meta_save, {
    id <- meta_selected_id(); req(id)
    df <- meta_df(); req(nrow(df))
    
    s <- input$meta_table_rows_selected
    out_disp <- meta_table_data()
    if (length(s) != 1 || !nrow(out_disp)) { showNotification("Select a row first.", type="warning"); return() }
    key_name <- out_disp$Name[s]; key_url <- out_disp$URL[s]
    idx <- which(df$name == key_name & df$profile_link == key_url)
    if (!length(idx)) { showNotification("Row not found.", type="error"); return() }
    
    coords <- loc_coords()
    df$phone[idx]    <- input$meta_phone %||% ""
    df$email[idx]    <- input$meta_email %||% ""
    df$species[idx]  <- input$meta_species %||% ""
    df$location[idx] <- loc_value() %||% ""
    df$lat[idx]      <- coords[1]; df$lng[idx] <- coords[2]
    meta_df(df)
    
    out_new <- df %>% arrange(desc(is_poi), name) %>% transmute(
      POI = ifelse(is_poi, "Yes", ""),
      Name = name,
      URL = profile_link,
      Context = context,
      Phone = phone,
      Email = email,
      Species = species,
      Location = location
    )
    replaceData(meta_proxy, out_new, resetPaging = FALSE, rownames = FALSE)
    selectRows(meta_proxy, s)
    
    showNotification("Saved.", type = "message")
  })
  
  output$meta_download_xlsx <- downloadHandler(
    filename = function() paste0("enriched_profiles_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".xlsx"),
    content = function(file){
      df <- meta_df()
      if (!nrow(df)) write_xlsx(tibble(Note="No rows"), path = file) else write_xlsx(df, path = file)
    }
  )
  
  # ---------------- Step 3: Flows Map (POI → Confirmed/Uncertain) ----------------
  output$flows_map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = 10, lat = 20, zoom = 2)
  })
  
  compute_flows <- function(){
    nd <- tryCatch(network_data(), error = function(e) NULL)
    dfm <- meta_df()
    if (is.null(nd) || !nrow(dfm)) return(tibble())
    
    v <- vetting()
    nodes <- nd$nodes
    if (!"is_poi" %in% names(nodes)) nodes$is_poi <- nodes$id %in% nd$owners
    node_info <- nodes %>% transmute(id, is_poi = is_poi, status = as.character(v[as.character(id)]))
    
    coords <- dfm %>% select(name, lat, lng) %>% mutate(lat = as.numeric(lat), lng = as.numeric(lng))
    
    owners <- nd$owners
    edges_oriented <- nd$edges %>%
      mutate(Source = ifelse(from %in% owners, from, to), Target = ifelse(from %in% owners, to, from)) %>%
      distinct(Source, Target)
    
    src_info <- node_info %>% transmute(Source = id, Source_is_poi = is_poi, Source_status = status)
    tgt_info <- node_info %>% transmute(Target = id, Target_is_poi = is_poi, Target_status = status)
    
    ed <- edges_oriented %>% left_join(src_info, by = "Source") %>% left_join(tgt_info, by = "Target")
    ed <- ed %>% filter(Source_is_poi) %>% filter(!is.na(Target_status) & Target_status %in% c("accept","uncertain"))
    
    ed <- ed %>%
      left_join(coords %>% rename(Source = name, sLat = lat, sLng = lng), by = "Source") %>%
      left_join(coords %>% rename(Target = name, tLat = lat, tLng = lng), by = "Target")
    
    ed %>% filter(is.finite(sLat), is.finite(sLng), is.finite(tLat), is.finite(tLng))
  }
  
  flows_data <- eventReactive(input$flows_update, compute_flows(), ignoreInit = TRUE)
  
  observeEvent(flows_data(), {
    fd <- flows_data()
    proxy <- leafletProxy("flows_map")
    proxy %>% clearMarkers() %>% clearShapes()
    
    if (!nrow(fd)){
      showNotification("No flows to draw. Ensure POIs and Confirmed/Uncertain nodes have locations saved.", type = "warning")
      return()
    }
    
    for (i in seq_len(nrow(fd))){
      col <- if (fd$Target_status[i] == "accept") COL_ACC_BG else COL_UNC_BG
      dash <- if (fd$Target_status[i] == "uncertain") "10,10" else NULL
      proxy %>% addPolylines(lng = c(fd$sLng[i], fd$tLng[i]), lat = c(fd$sLat[i], fd$tLat[i]),
                             weight = 3, color = col, dashArray = dash, opacity = 0.9)
    }
    
    if (isTRUE(input$flows_show_points)){
      poi_pts <- fd %>% distinct(Source, sLat, sLng)
      if (nrow(poi_pts)){
        proxy %>% addCircleMarkers(data = poi_pts, lng = ~sLng, lat = ~sLat,
                                   radius = 5, fillOpacity = 1, color = "#424242",
                                   fillColor = "#ffffff", weight = 2, label = ~Source)
      }
      tgt_pts <- fd %>% distinct(Target, tLat, tLng, Target_status)
      if (nrow(tgt_pts)){
        proxy %>% addCircleMarkers(data = tgt_pts, lng = ~tLng, lat = ~tLat,
                                   radius = 4, fillOpacity = 1,
                                   color = ifelse(tgt_pts$Target_status=="accept", COL_ACC_BR, COL_UNC_BR),
                                   fillColor = ifelse(tgt_pts$Target_status=="accept", COL_ACC_BG, COL_UNC_BG),
                                   weight = 2, label = ~Target)
      }
    }
    
    if (isTRUE(input$flows_fit_bounds)){
      all_lng <- c(fd$sLng, fd$tLng); all_lat <- c(fd$sLat, fd$tLat)
      proxy %>% fitBounds(lng1 = min(all_lng), lat1 = min(all_lat), lng2 = max(all_lng), lat2 = max(all_lat))
    }
  })
  
  # ---------------- Step 4: Maltego CSV ----------------
  output$download_maltego_csv <- downloadHandler(
    filename = function() paste0("maltego_edges_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv"),
    content = function(file){
      nd <- network_data()
      nodes <- nd$nodes; edges <- nd$edges; owners <- nd$owners
      vvec <- vetting()
      node_ids <- as.character(nodes$id)
      keep <- setNames(rep(TRUE, length(node_ids)), node_ids)
      if (isTRUE(input$export_only_confirmed)){
        keep[] <- FALSE
        conf_ids <- names(vvec)[!is.na(vvec) & vvec == "accept"]
        keep[conf_ids] <- TRUE; keep[owners] <- TRUE
      } else if (isTRUE(input$export_exclude_rejected)){
        rej_ids <- names(vvec)[!is.na(vvec) & vvec == "reject"]
        keep[rej_ids] <- FALSE
      }
      kept_ids <- names(keep)[keep]
      nodes2 <- nodes %>% dplyr::filter(id %in% kept_ids)
      edges2 <- edges %>% dplyr::filter(from %in% kept_ids & to %in% kept_ids) %>% dplyr::distinct(from, to)
      meta <- nodes2 %>% dplyr::transmute(
        id,
        NodeURL     = profile_link %||% NA_character_,
        NodeContext = context %||% NA_character_,
        NodeIsPOI   = id %in% owners,
        NodeVetted  = as.character(vvec[as.character(id)])
      )
      out_edges <- edges2 %>%
        dplyr::mutate(Source = ifelse(from %in% owners, from, to),
                      Target = ifelse(from %in% owners, to, from)) %>%
        dplyr::distinct(Source, Target)
      out <- out_edges %>%
        dplyr::left_join(meta, by = c("Source" = "id")) %>%
        dplyr::rename(SourceURL = NodeURL, SourceContext = NodeContext,
                      SourceIsPOI = NodeIsPOI, SourceVetted = NodeVetted) %>%
        dplyr::left_join(meta, by = c("Target" = "id")) %>%
        dplyr::rename(TargetURL = NodeURL, TargetContext = NodeContext,
                      TargetIsPOI = NodeIsPOI, TargetVetted = NodeVetted) %>%
        dplyr::mutate(LinkLabel = "friend_of") %>%
        dplyr::select(Source, SourceURL, SourceContext, SourceIsPOI, SourceVetted,
                      Target, TargetURL, TargetContext, TargetIsPOI, TargetVetted, LinkLabel)
      readr::write_excel_csv(out, file)
    }
  )
  
  # ---------------- Step 4: Save Session (.rds) ----------------
  output$download_session <- downloadHandler(
    filename = function() paste0("ofn_session_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".rds"),
    content = function(file){
      state <- list(
        app_version    = app_version,
        saved_networks = saved_networks(),
        poi_urls       = poi_urls(),
        vetting        = vetting(),
        meta_df        = meta_df(),
        geocode_cache  = geocode_cache(),
        saved_at       = Sys.time()
      )
      saveRDS(state, file)
    }
  )
}

shinyApp(ui, server)

}

