run_network_app <- function(){# app.R — Two-tab OSINT Friend Networks (v5, modal opens system browser)
  # - Hover shows CONTEXT; clicking a node shows a modal with "View Profile" button
  # - Button uses JS window.open(...) so it opens your default browser (not a blank R window)
  # - Stays on Step 1 after saving a POI (no auto-switch)
  
  options(shiny.maxRequestSize = 1024^3)   # 1 GB upload cap for the parser
  options(shiny.launch.browser = TRUE)     # open the app in your system browser
  
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
  
  # ------------------ Helpers: Parser ------------------
  
  # Vectorised normaliser for Facebook profile URLs
  normalize_fb_url <- function(url) {
    url <- as.character(url)
    if (!length(url)) return(character())
    # prepend domain when path-only
    url <- ifelse(startsWith(url, "/"), paste0("https://www.facebook.com", url), url)
    # keep query for profile.php but drop fragments for others
    is_profilephp <- grepl("profile\\.php", url, ignore.case = TRUE)
    url <- ifelse(is_profilephp, url, sub("[#?].*$", "", url))
    # avoid forcing trailing slash on profile.php?id=...
    url <- ifelse(is_profilephp, url, sub("/+$$", "/", paste0(url, "/")))
    url
  }
  
  # URL looks like an actual profile (slug or profile.php?id=...)
  looks_like_profile <- function(url) {
    url <- as.character(url)
    grepl("facebook\\.com/(profile\\.php\\?id=\\d+/?$|[A-Za-z0-9._-]+/?$)", url) &
      !grepl("facebook\\.com/(help|policy|privacy|settings|login\\.php|watch|marketplace|groups|events|pages|gaming|ads|reel|stories|bookmark|search)", url)
  }
  
  # Auto-detect POI from <title>
  auto_detect_poi <- function(html_path) {
    title_txt <- tryCatch({
      doc <- read_html(html_path, options = c("RECOVER","NOERROR","NOWARNING"))
      html_text2(html_element(doc, "title"))
    }, error = function(e) NA_character_)
    if (is.na(title_txt) || !nzchar(title_txt)) return(NA_character_)
    pats <- c(
      "(?i)friends\\s*[·|]\\s*([^|·\\-]+)",
      "(?i)friends\\s+of\\s+([^|·\\-]+)",
      "(?i)amis\\s*[·|]\\s*([^|·\\-]+)",
      "(?i)amigos\\s*[·|]\\s*([^|·\\-]+)"
    )
    for (p in pats) {
      m <- stringr::str_match(title_txt, p)
      if (!all(is.na(m)) && nzchar(trimws(m[,2]))) return(trimws(m[,2]))
    }
    NA_character_
  }
  
  # Extract context line under each friend name in a saved Friends page HTML
  extract_friends_from_html <- function(html_path) {
    doc <- read_html(html_path, options = c("RECOVER","NOERROR","NOWARNING"))
    
    # Focusable name links within the name span
    name_links <- html_elements(
      doc,
      xpath = "//span[contains(@class,'xjp7ctv')]//a[@role='link' and @tabindex='0' and contains(@href,'facebook.com')]"
    )
    if (!length(name_links)) {
      return(tibble(name = character(), profile_url = character(), context = character()))
    }
    
    nm_vec <- html_text2(name_links)
    hrefs <- normalize_fb_url(html_attr(name_links, "href"))
    
    # helper: tidy candidate context text (safe to call on NA/empty)
    clean_ctx <- function(x) {
      if (is.null(x) || length(x) == 0) return(NA_character_)
      x <- x[1]
      if (is.na(x)) return(NA_character_)
      x <- as.character(x)
      x <- gsub("\\s+", " ", x)
      x <- trimws(x)
      if (!nzchar(x)) return(NA_character_)
      drops <- c("Follow","Friends","Friend","Mutual friends","Add Friend","Message","Options","More")
      if (tolower(x) %in% tolower(drops)) return(NA_character_)
      nc <- nchar(x)
      if (!is.na(nc) && nc > 160) x <- substr(x, 1, 160)
      x
    }
    
    contexts <- purrr::map_chr(seq_along(name_links), function(i) {
      a <- name_links[[i]]
      
      # 1) Preferred: first text block after the name span
      ctx1 <- tryCatch(
        html_element(a, xpath = "./ancestor::span[contains(@class,'xjp7ctv')]/following-sibling::div[1]//div[@dir='auto'][1]"),
        error = function(e) NULL
      )
      txt <- clean_ctx(if (!is.null(ctx1)) html_text2(ctx1) else NA_character_)
      
      # 2) Fallback: first text block after the immediate wrapping div
      if (is.na(txt)) {
        ctx2 <- tryCatch(
          html_element(a, xpath = "./ancestor::div[1]/following-sibling::div[1]//div[@dir='auto'][1]"),
          error = function(e) NULL
        )
        txt <- clean_ctx(if (!is.null(ctx2)) html_text2(ctx2) else NA_character_)
      }
      
      # 3) Fallback: parse nearby block and pick a short meaningful line (not the name)
      if (is.na(txt)) {
        near <- tryCatch(html_element(a, xpath = "./ancestor::div[1]/following-sibling::div[1]"), error = function(e) NULL)
        if (!is.null(near)) {
          raw <- tryCatch(html_text2(near), error = function(e) "")
          parts <- strsplit(raw, "\\r?\\n|\\s*·\\s*|\\x{2022}|\\|{1,2}", perl = TRUE)[[1]]
          parts <- trimws(parts)
          parts <- parts[nchar(parts) > 1]
          # drop UI words and the NAME itself
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
  
  # ------------------ Theme ------------------
  
  theme <- bs_theme(version = 5, bootswatch = "flatly", base_font = font_google("Inter"))
  
  # ------------------ UI ------------------
  
  ui <- page_fluid(
    theme = theme,
    useShinyjs(),
    titlePanel("OSINT Friend Networks"),
    tabsetPanel(
      id = "tabs",
      tabPanel(
        title = "Step 1. Parse Friend Networks",
        tags$p("Open the person's profile → Friends, scroll to the bottom to load all entries, ",
               "then in your browser: ", tags$strong("File → Save Page As… → Webpage, HTML Only"), "."),
        tags$hr(),
        fluidRow(
          column(5,
                 h4("Upload saved Friends HTML"),
                 fileInput("html_file", NULL, accept = c(".html",".htm"), buttonLabel = "Choose HTML…", placeholder = "No file selected"),
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
          column(4,
                 h4("Select saved POI networks"),
                 uiOutput("saved_picker"),
                 actionLink("select_all", "Select all"), span(" | "), actionLink("clear_all", "Clear"),
                 tags$hr(),
                 sliderInput("threshold", "Friend appears in at least N POIs", min = 1, max = 2, value = 1, step = 1),
                 selectInput("community_algo", "Community Detection", choices = c("Walktrap","Louvain","Infomap","Edge betweenness"), selected = "Walktrap"),
                 checkboxInput("highlight_owners", "Style POI (central) nodes", TRUE),
                 selectizeInput("highlight_ids", "Highlight nodes", choices = NULL, multiple = TRUE),
                 selectizeInput("path_ids", "Shortest path (choose two nodes)", choices = NULL, multiple = TRUE, options = list(maxItems = 2)),
                 br(), downloadButton("download_report", "Download XLSX report")
          ),
          column(8,
                 h4("Network"),
                 withSpinner(visNetworkOutput("network", height = "650px"), type = 5, color = "#98A5A6")
          )
        )
      )
    )
  )
  
  # ------------------ Server ------------------
  
  server <- function(input, output, session) {
    parsed <- reactiveVal(tibble(POI = character(), name = character(), context = character(), profile_url = character()))
    saved_networks <- reactiveVal(list())
    
    observeEvent(input$html_file, {
      req(input$html_file)
      auto <- auto_detect_poi(input$html_file$datapath)
      if (!is.na(auto) && nzchar(auto)) {
        updateTextInput(session, "poi_name", value = auto)
        showNotification(paste("Detected POI:", auto), type = "message")
      }
    }, ignoreInit = TRUE)
    
    observeEvent(input$parse_btn, {
      req(input$html_file)
      poi <- trimws(input$poi_name %||% "")
      if (!nzchar(poi)) { showNotification("Please enter a POI name before parsing.", type = "warning"); return(NULL) }
      tryCatch({
        res <- extract_friends_from_html(input$html_file$datapath)
        if (nrow(res) == 0) {
          parsed(tibble(POI = character(), name = character(), context = character(), profile_url = character()))
          showNotification("No friend entries detected. Ensure you fully scrolled before saving and used 'HTML Only'.", type = "warning")
        } else {
          res <- res %>% mutate(POI = poi) %>% relocate(POI, name, context, profile_url)
          parsed(res)
          showNotification(paste("Parsed", nrow(res), "friends for POI:", poi), type = "message")
        }
      }, error = function(e) { showNotification(paste("Parsing failed:", conditionMessage(e)), type = "error") })
    })
    
    observeEvent(input$save_btn, {
      dat <- parsed(); validate(need(nrow(dat) > 0, "No parsed data to save. Parse a file first."))
      poi <- unique(dat$POI)
      key <- paste0(poi, " — ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
      store <- saved_networks(); store[[key]] <- dat; saved_networks(store)
      updateSelectInput(session, "saved_choice", choices = names(store), selected = key)
      reset("html_file"); parsed(tibble(POI = character(), name = character(), context = character(), profile_url = character()))
      showNotification(paste("Saved network:", key, "— you can upload the next POI or switch to Step 2 when ready."), type = "message")
      # No auto-switch to Step 2
    })
    
    observeEvent(input$load_saved, {
      choice <- input$saved_choice; store <- saved_networks()
      if (nzchar(choice) && choice %in% names(store)) { parsed(store[[choice]]); showNotification(paste("Loaded saved network:", choice), type = "message") }
      else { showNotification("Select a saved network first.", type = "warning") }
    })
    
    output$friends_table <- renderDT({
      dat <- parsed(); if (nrow(dat) == 0) return(DT::datatable(tibble(), options = list(dom = 't')))
      DT::datatable(dat, rownames = FALSE, extensions = c('Buttons'),
                    options = list(pageLength = 25, scrollX = TRUE, dom = 'Bfrtip',
                                   buttons = list(list(extend = "csv", text = "Download CSV",
                                                       title = paste0("facebook_friends_", format(Sys.time(), "%Y%m%d_%H%M%S")) ))),
                    escape = FALSE)
    })
    
    output$download_csv <- downloadHandler(
      filename = function() paste0("facebook_friends_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv"),
      content = function(file) { dat <- parsed(); validate(need(nrow(dat) > 0, "No data to export. Parse a file first.")); write_csv(dat, file) }
    )
    
    output$saved_picker <- renderUI({
      store <- saved_networks(); keys <- names(store)
      if (!length(keys)) tags$em("No saved POI networks yet. Use Step 1 to add some.")
      else checkboxGroupInput("include_networks", NULL, choices = keys, selected = keys)
    })
    observeEvent(input$select_all, { store <- saved_networks(); updateCheckboxGroupInput(session, "include_networks", selected = names(store)) })
    observeEvent(input$clear_all, { updateCheckboxGroupInput(session, "include_networks", selected = character(0)) })
    
    data_from_saved <- reactive({
      store <- saved_networks(); sel <- input$include_networks
      validate(need(length(store) > 0, "No saved networks found."),
               need(length(sel) > 0, "Select at least one saved network."))
      bind_rows(store[sel]) %>%
        rename(account_owner = POI, profile_name = name, profile_link = profile_url) %>%
        mutate(account_owner = as.character(account_owner),
               profile_name  = as.character(profile_name))
    })
    
    observe({
      df <- tryCatch(data_from_saved(), error = function(e) NULL)
      if (!is.null(df) && nrow(df)) {
        nPOI <- length(unique(df$account_owner))
        updateSliderInput(session, "threshold", min = 1, max = max(2, nPOI), value = min(2, nPOI), step = 1)
      }
    })
    
    first_non_na <- function(x){ x <- x[!is.na(x) & nzchar(x)]; if (length(x)) x[[1]] else NA_character_ }
    
    network_data <- reactive({
      df <- data_from_saved(); validate(need(nrow(df) > 0, "No rows in selected networks."))
      bipartite_edges <- df %>% select(account_owner, profile_name, profile_link, context)
      friend_counts <- bipartite_edges %>%
        distinct(account_owner, profile_name, profile_link, context) %>%
        group_by(profile_name, profile_link) %>%
        summarise(count = n(), .groups = "drop")
      filtered_friends <- friend_counts %>% filter(count >= input$threshold)
      validate(need(nrow(filtered_friends) > 0, "No friends meet the selected threshold. Try lowering it or selecting more POIs."))
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
      owners <- unique(filtered_edges$account_owner); nodes$is_poi <- nodes$id %in% owners
      
      # Tooltip on hover: show context for friends, POI tag for POIs
      nodes$title <- ifelse(
        nodes$is_poi,
        "<strong>POI</strong>",
        ifelse(!is.na(nodes$context) & nzchar(nodes$context), htmlEscape(nodes$context), "")
      )
      
      # Node sizing by degree
      deg <- degree(g); minD <- min(deg); maxD <- max(deg)
      nodes$size <- if (maxD == minD) rep(28, length(deg)) else 12 + (deg-minD)/(maxD-minD) * (46-12)
      
      # Distinguish POIs visually
      if (isTRUE(input$highlight_owners)) {
        nodes$shape <- ifelse(nodes$is_poi, "diamond", "dot")
        nodes$color.background <- ifelse(nodes$is_poi, "#ffffff", NA)
        nodes$color.border <- ifelse(nodes$is_poi, "#000000", NA)
      } else {
        nodes$shape <- "dot"
      }
      
      edges <- as.data.frame(get.edgelist(g)); colnames(edges) <- c("from","to"); edges$width <- 1
      list(nodes = nodes, edges = edges, g = g, owners = owners)
    })
    
    observe({
      nd <- tryCatch(network_data(), error = function(e) NULL)
      if (!is.null(nd)) {
        ch <- nd$nodes$id
        updateSelectizeInput(session, "highlight_ids", choices = ch, server = TRUE)
        updateSelectizeInput(session, "path_ids",      choices = ch, server = TRUE)
      }
    })
    observe({ if (!is.null(input$path_ids) && length(input$path_ids) > 0) { updateSelectizeInput(session, "highlight_ids", selected = character(0)); disable("highlight_ids") } else { enable("highlight_ids") } })
    observe({ if (!is.null(input$highlight_ids) && length(input$highlight_ids) > 0) { updateSelectizeInput(session, "path_ids", selected = character(0)); disable("path_ids") } else { enable("path_ids") } })
    
    output$network <- renderVisNetwork({
      nd <- network_data(); nodes <- nd$nodes; edges <- nd$edges; g <- nd$g
      groups <- sort(unique(nodes$community))
      pal <- brewer.pal(min(length(groups), 8), "Spectral")
      if (length(groups) > length(pal)) pal <- rep(pal, length.out = length(groups))
      color_map <- setNames(pal, groups)
      nodes$color.background <- ifelse(is.na(nodes$color.background), color_map[as.character(nodes$community)], nodes$color.background)
      nodes$color.border <- ifelse(is.na(nodes$color.border), "black", nodes$color.border)
      edges$color <- "gray"
      
      if (!is.null(input$highlight_ids) && length(input$highlight_ids) > 0) {
        highlight_set <- unique(unlist(lapply(input$highlight_ids, function(id){
          nbrs <- neighbors(g, id, mode = "all"); c(id, as_ids(nbrs))
        })))
        nodes$color.background[!(nodes$id %in% highlight_set)] <- "#d9d9d9"
        nodes$color.border[!(nodes$id %in% highlight_set)] <- "#bdbdbd"
      }
      if (!is.null(input$path_ids) && length(input$path_ids) == 2) {
        sp <- shortest_paths(g, from = input$path_ids[1], to = input$path_ids[2], output = "both")
        sp_nodes <- as_ids(sp$vpath[[1]]); sp_edge_ids <- unlist(sp$epath[[1]])
        nodes$color.background[nodes$id %in% sp_nodes] <- "#ffb74d"; nodes$color.border[nodes$id %in% sp_nodes] <- "#e65100"
        if (length(sp_edge_ids) > 0) { edges$color[sp_edge_ids] <- "#e65100"; edges$width[sp_edge_ids] <- 3 }
        non_sp_edges <- setdiff(seq_len(nrow(edges)), sp_edge_ids); edges$color[non_sp_edges] <- "#d0d0d0"
      }
      
      visNetwork(nodes, edges) %>%
        visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
        visPhysics(solver = "forceAtlas2Based", stabilization = TRUE) %>%
        # Send clicked/selected node id back to Shiny for modal
        visEvents(
          select = htmlwidgets::JS(
            "function(params){",
            "  if(params && params.nodes && params.nodes.length){",
            "    Shiny.setInputValue('node_click', params.nodes[0], {priority: 'event'});",
            "  }",
            "}"
          )
        )
    })
    
    # Show modal with name, context, and JS-based "View Profile" button (system browser)
    observeEvent(input$node_click, {
      req(input$node_click)
      nd <- tryCatch(network_data(), error = function(e) NULL); req(!is.null(nd))
      node <- nd$nodes[nd$nodes$id == input$node_click, , drop = FALSE]; req(nrow(node) == 1)
      
      nm    <- node$label %||% node$id
      ctx   <- node$context %||% NA_character_
      link  <- node$profile_link %||% NA_character_
      is_poi <- isTRUE(node$is_poi)
      
      # escape single quotes for JS literal
      js_url <- gsub("'", "\\\\'", link, fixed = TRUE)
      
      showModal(
        modalDialog(
          easyClose = TRUE, footer = NULL,
          div(
            h4(htmltools::htmlEscape(nm), style = "margin-top:0;"),
            if (!is.na(ctx) && nzchar(ctx)) tags$p(tags$em(htmltools::htmlEscape(ctx))),
            if (is_poi) tags$p(tags$small(tags$b("POI (central node)"))),
            tags$hr(),
            if (!is.na(link) && nzchar(link)) {
              # Use JS to open in system/default browser and avoid R's internal viewer
              tags$button(
                type = "button", class = "btn btn-primary",
                onclick = sprintf("window.open('%s','_blank','noopener');", js_url),
                "View Profile"
              )
            } else {
              tags$em("No profile URL available for this node.")
            }
          )
        )
      )
    })
    
    output$download_report <- downloadHandler(
      filename = function() paste0("network_report_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".xlsx"),
      content = function(file) {
        nd <- network_data(); g <- nd$g; df <- data_from_saved()
        deg <- degree(g); bet <- betweenness(g); clo <- closeness(g); eig <- eigen_centrality(g)$vector; comm <- V(g)$community
        node_info <- data.frame(Node = names(deg), Degree = as.numeric(deg), Betweenness = as.numeric(bet),
                                Closeness = as.numeric(clo), Eigenvector = as.numeric(eig),
                                Community = as.character(comm), stringsAsFactors = FALSE)
        profile_info <- df %>% distinct(profile_name, profile_link, context)
        report <- left_join(node_info, profile_info, by = c("Node" = "profile_name"))
        owners <- unique(df$account_owner); report$POI <- report$Node %in% owners
        write_xlsx(report, path = file)
      }
    )
  }
  
  # %||% fallback
  `%||%` <- function(x, y) if (is.null(x) || (length(x) == 1 && is.na(x))) y else x
  
  shinyApp(ui, server)
}
