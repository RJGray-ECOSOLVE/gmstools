run_network_app <- function(){
  # app.R — OSINT Friend Networks (v8.2)
# Tabs: (1) Parse & Save  (2) Build Network (Vetting)
#       (3) Enrich Confirmed Profiles (with map search + click-to-pin)
#       (4) Export to Maltego
# - FIX: remove searchOSMOptions (not available in all builds)
# - NEW: text search + geocode via OSM Nominatim; map click still supported
# - POI auto-filled from uploaded file name

options(shiny.maxRequestSize = 1024^3)
options(shiny.launch.browser = TRUE)

library(shiny)
library(shinyjs)
library(bslib)
library(rvest)
library(dplyr)
library(stringr)
library(readr)
library(DT)
library(purrr)
library(htmltools)
library(tidyr)
library(igraph)
library(visNetwork)
library(RColorBrewer)
library(shinycssloaders)
library(writexl)
library(leaflet)
library(leaflet.extras)   # still used for providers, but not searchOSMOptions
library(httr)             # for geocoding
library(jsonlite)         # for geocoding

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
      httr::user_agent("OSINT-Friend-Networks/1.0 (Shiny) https://nominatim.openstreetmap.org")
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

# ---------- Theme ----------
theme <- bs_theme(version = 5, bootswatch = "flatly", base_font = font_google("Inter"))

# ---------- UI ----------
ui <- page_fluid(
  theme = theme,
  useShinyjs(),
  titlePanel("OSINT Friend Networks"),
  tabsetPanel(
    id = "tabs",
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
               tags$small("This app structures data from a file you saved while viewing content you could already access. ",
                          "Follow applicable laws and platform ToS.")
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
               checkboxInput("highlight_owners", "Style POI (central) nodes", TRUE),
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
      title = "Step 3. Enrich Confirmed Profiles",
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
               h4("Profiles to enrich (POIs and Confirmed)"),
               DTOutput("meta_table"),
               br(),
               downloadButton("meta_download_xlsx", "Download Enriched Table (XLSX)")
        )
      )
    ),
    tabPanel(
      title = "Step 4. Export to Maltego",
      fluidRow(
        column(5,
               h4("Export CSV for Maltego"),
               tags$p("Use in Maltego: ", tags$strong("Import Graph → 3rd Party Table"), ". Map ",
                      tags$code("Source/Target"), " to Person/Alias; map URL/context to properties."),
               checkboxInput("export_only_confirmed", "Only export Confirmed nodes (always keep POIs)", FALSE),
               checkboxInput("export_exclude_rejected", "Exclude Rejected nodes", TRUE),
               br(),
               downloadButton("download_maltego_csv", "Download Maltego CSV")
        ),
        column(7,
               tags$h5("Columns"),
               tags$pre("Source, SourceURL, SourceContext, SourceIsPOI, SourceVetted,\nTarget, TargetURL, TargetContext, TargetIsPOI, TargetVetted,\nLinkLabel")
        )
      )
    )
  )
)

# ---------- Server ----------
server <- function(input, output, session){
  parsed <- reactiveVal(tibble(POI = character(), name = character(), context = character(), profile_url = character()))
  saved_networks <- reactiveVal(list())
  
  vetting <- reactiveVal(setNames(logical(0), character(0)))
  apply_filter <- reactiveVal(FALSE)
  current_node <- reactiveVal(NULL)
  
  meta_df <- reactiveVal(tibble(
    name = character(), profile_link = character(), context = character(),
    phone = character(), email = character(), species = character(),
    location = character(), lat = numeric(), lng = numeric()
  ))
  meta_selected_id <- reactiveVal(NULL)
  loc_value  <- reactiveVal("")
  loc_coords <- reactiveVal(c(NA_real_, NA_real_))
  
  # Step 1: upload -> suggest POI
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
    updateSelectInput(session, "saved_choice", choices = names(store), selected = key)
    reset("html_file"); parsed(tibble(POI = character(), name = character(), context = character(), profile_url = character()))
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
  
  # Step 2: saved picker
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
    
    v <- vetting()
    if (isTRUE(apply_filter())){
      rejected <- names(v)[!is.na(v) & v == FALSE]
      if (length(rejected)) df <- df %>% filter(!(profile_name %in% rejected))
    }
    
    bipartite_edges <- df %>% select(account_owner, profile_name, profile_link, context)
    
    friend_counts <- bipartite_edges %>%
      distinct(account_owner, profile_name, profile_link, context) %>%
      group_by(profile_name, profile_link) %>%
      summarise(count = n(), .groups = "drop")
    
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
    
    owners <- unique(filtered_edges$account_owner); nodes$is_poi <- nodes$id %in% owners
    nodes$title <- ifelse(nodes$is_poi, "<strong>POI</strong>",
                          ifelse(!is.na(nodes$context) & nzchar(nodes$context), esc(nodes$context), ""))
    
    deg <- degree(g); minD <- min(deg); maxD <- max(deg)
    nodes$size <- if (maxD == minD) rep(28, length(deg)) else 12 + (deg-minD)/(maxD-minD) * (46-12)
    
    nodes$shape <- ifelse(nodes$is_poi & isTRUE(input$highlight_owners), "diamond", "dot")
    nodes$color.background <- ifelse(nodes$is_poi & isTRUE(input$highlight_owners), "#ffffff", NA)
    nodes$color.border <- ifelse(nodes$is_poi & isTRUE(input$highlight_owners), "#000000", NA)
    
    nodes$vetted <- as.logical(v[as.character(nodes$id)])
    
    edges <- as.data.frame(get.edgelist(g)); colnames(edges) <- c("from","to"); edges$width <- 1
    list(nodes = nodes, edges = edges, g = g, owners = owners)
  })
  
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
  
  output$network <- renderVisNetwork({
    nd <- network_data()
    nodes <- nd$nodes; edges <- nd$edges; g <- nd$g
    
    groups <- sort(unique(nodes$community))
    pal <- brewer.pal(min(length(groups), 8), "Spectral")
    if (length(groups) > length(pal)) pal <- rep(pal, length.out = length(groups))
    color_map <- setNames(pal, groups)
    nodes$color.background <- ifelse(is.na(nodes$color.background), color_map[as.character(nodes$community)], nodes$color.background)
    nodes$color.border <- ifelse(is.na(nodes$color.border), "black", nodes$color.border)
    edges$color <- "gray"
    
    if (!is.null(input$highlight_ids) && length(input$highlight_ids) > 0){
      highlight_set <- unique(unlist(lapply(input$highlight_ids, function(id){
        nbrs <- neighbors(g, id, mode = "all"); c(id, igraph::as_ids(nbrs))
      })))
      nodes$color.background[!(nodes$id %in% highlight_set)] <- "#d9d9d9"
      nodes$color.border[!(nodes$id %in% highlight_set)] <- "#bdbdbd"
    }
    
    if (!is.null(input$path_ids) && length(input$path_ids) == 2){
      sp <- igraph::shortest_paths(g, from = input$path_ids[1], to = input$path_ids[2], output = "both")
      sp_nodes <- igraph::as_ids(sp$vpath[[1]]); sp_edge_ids <- unlist(sp$epath[[1]])
      nodes$color.background[nodes$id %in% sp_nodes] <- "#ffb74d"; nodes$color.border[nodes$id %in% sp_nodes] <- "#e65100"
      if (length(sp_edge_ids) > 0){ edges$color[sp_edge_ids] <- "#e65100"; edges$width[sp_edge_ids] <- 3 }
      non_sp_edges <- setdiff(seq_len(nrow(edges)), sp_edge_ids); edges$color[non_sp_edges] <- "#d0d0d0"
    }
    
    nodes$color.background[!is.na(nodes$vetted) & nodes$vetted == TRUE]  <- "#2ecc71"
    nodes$color.border[!is.na(nodes$vetted) & nodes$vetted == TRUE]      <- "#1b5e20"
    nodes$color.background[!is.na(nodes$vetted) & nodes$vetted == FALSE] <- "#e74c3c"
    nodes$color.border[!is.na(nodes$vetted) & nodes$vetted == FALSE]     <- "#b71c1c"
    
    visNetwork(nodes, edges) %>%
      visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
      visPhysics(solver = "forceAtlas2Based", stabilization = TRUE) %>%
      visEvents(
        select = htmlwidgets::JS(
          "function(params){ if(params && params.nodes && params.nodes.length){ Shiny.setInputValue('node_click', params.nodes[0], {priority: 'event'}); } }"
        )
      )
  })
  
  observeEvent(input$node_click, {
    req(input$node_click)
    nd <- tryCatch(network_data(), error = function(e) NULL); req(!is.null(nd))
    node <- nd$nodes[nd$nodes$id == input$node_click, , drop = FALSE]; req(nrow(node) == 1)
    
    nm    <- node$label %||% node$id
    ctx   <- node$context %||% NA_character_
    link  <- node$profile_link %||% NA_character_
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
            if (!is.na(link) && nzchar(link)) tags$button(type = "button", class = "btn btn-primary",
                                                          onclick = sprintf("window.open('%s','_blank','noopener');", js_url), "View Profile"),
            if (!isTRUE(is_poi)) actionButton("confirm_node", label = tagList(icon("check"), "Confirm"), class = "btn btn-success"),
            if (!isTRUE(is_poi)) actionButton("reject_node",  label = tagList(icon("times"),  "Reject"),  class = "btn btn-danger")
          )
        )
      )
    )
  })
  observeEvent(input$confirm_node, { id <- current_node(); req(id); v <- vetting(); v[id] <- TRUE; vetting(v); removeModal() })
  observeEvent(input$reject_node,  { id <- current_node(); req(id); v <- vetting(); v[id] <- FALSE; vetting(v); removeModal() })
  
  output$download_report <- downloadHandler(
    filename = function() paste0("network_report_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".xlsx"),
    content = function(file){
      nd <- network_data(); g <- nd$g; df <- data_from_saved()
      deg <- degree(g); bet <- betweenness(g); clo <- closeness(g); eig <- eigen_centrality(g)$vector; comm <- V(g)$community
      node_info <- data.frame(Node = names(deg), Degree = as.numeric(deg), Betweenness = as.numeric(bet),
                              Closeness = as.numeric(clo), Eigenvector = as.numeric(eig),
                              Community = as.character(comm), stringsAsFactors = FALSE)
      profile_info <- df %>% distinct(profile_name, profile_link, context)
      report <- left_join(node_info, profile_info, by = c("Node" = "profile_name"))
      owners <- unique(df$account_owner); report$POI <- report$Node %in% owners
      v <- vetting(); report$Vetted <- as.logical(v[as.character(report$Node)])
      write_xlsx(report, path = file)
    }
  )
  
  # Step 3: Enrichment
  observeEvent(list(network_data(), vetting()), {
    nd <- tryCatch(network_data(), error = function(e) NULL)
    if (is.null(nd)) return()
    v <- vetting()
    nodes <- nd$nodes
    to_edit <- nodes$is_poi | (!is.na(nodes$vetted) & nodes$vetted == TRUE)
    base <- nodes[to_edit, c("id","profile_link","context")]
    names(base) <- c("name","profile_link","context")
    base$name <- as.character(base$name); base$profile_link <- as.character(base$profile_link); base$context <- as.character(base$context)
    cur <- meta_df()
    merged <- base %>%
      left_join(cur, by = c("name","profile_link","context")) %>%
      mutate(
        phone   = as.character(phone %||% ""),
        email   = as.character(email %||% ""),
        species = as.character(species %||% ""),
        location= as.character(location %||% ""),
        lat     = suppressWarnings(as.numeric(lat)),
        lng     = suppressWarnings(as.numeric(lng))
      )
    meta_df(merged)
  }, ignoreInit = FALSE)
  
  output$meta_table <- renderDT({
    df <- meta_df()
    if (!nrow(df)) return(DT::datatable(tibble(), options = list(dom='t')))
    out <- df %>% transmute(Name = name, URL = profile_link, Context = context,
                            Phone = phone, Email = email, Species = species, Location = location)
    DT::datatable(out, selection = "single", rownames = FALSE,
                  options = list(pageLength = 15, scrollX = TRUE, dom = "tpi"))
  })
  
  observeEvent(input$meta_table_rows_selected, {
    s <- input$meta_table_rows_selected
    df <- meta_df()
    if (length(s) != 1 || !nrow(df)) return()
    row <- df[s, ]
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
  
  # Map: initial render (no search widget from leaflet.extras to avoid missing function)
  output$meta_map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = 10, lat = 20, zoom = 2)
  })
  
  # Draw marker when coords change
  observe({
    coords <- loc_coords()
    proxy <- leafletProxy("meta_map")
    proxy %>% clearMarkers()
    if (all(is.finite(coords))){
      proxy %>% addMarkers(lng = coords[2], lat = coords[1])
      proxy %>% setView(lng = coords[2], lat = coords[1], zoom = 8)
    }
  })
  
  # Click map to set coordinates (Location string shows "lat, lon")
  observeEvent(input$meta_map_click, {
    click <- input$meta_map_click
    if (is.null(click)) return()
    lat <- click$lat; lng <- click$lng
    loc_coords(c(lat, lng))
    loc_value(sprintf("%.5f, %.5f", lat, lng))
  })
  
  # Search box -> geocode via OSM
  observeEvent(input$meta_do_search, {
    q <- input$meta_search
    if (!nzchar(q)) return()
    res <- geocode_osm(q)
    if (is.null(res) || !is.finite(res$lat) || !is.finite(res$lon)){
      showNotification("No results for that place. Try a more specific query.", type = "warning")
      return()
    }
    loc_coords(c(res$lat, res$lon))
    loc_value(res$display %||% sprintf("%.5f, %.5f", res$lat, res$lon))
  })
  
  output$loc_selected_label <- renderUI({
    val <- loc_value()
    if (!is.null(val) && nzchar(val)) HTML(paste0("<strong>Selected:</strong> ", esc(val)))
    else HTML("<em>No location selected.</em>")
  })
  
  observeEvent(input$meta_save, {
    id <- meta_selected_id(); req(id)
    df <- meta_df(); req(nrow(df))
    idx <- which(df$name == id); if (!length(idx)) return()
    coords <- loc_coords()
    df$phone[idx]    <- input$meta_phone %||% ""
    df$email[idx]    <- input$meta_email %||% ""
    df$species[idx]  <- input$meta_species %||% ""
    df$location[idx] <- loc_value() %||% ""
    df$lat[idx]      <- coords[1]; df$lng[idx] <- coords[2]
    meta_df(df)
    showNotification("Saved.", type = "message")
  })
  
  output$meta_download_xlsx <- downloadHandler(
    filename = function() paste0("enriched_profiles_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".xlsx"),
    content = function(file){
      df <- meta_df()
      if (!nrow(df)) write_xlsx(tibble(Note="No rows"), path = file) else write_xlsx(df, path = file)
    }
  )
  
  # Step 4: Maltego CSV
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
        conf_ids <- names(vvec)[!is.na(vvec) & vvec == TRUE]
        keep[conf_ids] <- TRUE; keep[owners] <- TRUE
      } else if (isTRUE(input$export_exclude_rejected)){
        rej_ids <- names(vvec)[!is.na(vvec) & vvec == FALSE]
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
        NodeVetted  = as.logical(vvec[as.character(id)])
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
}

shinyApp(ui, server)
}


