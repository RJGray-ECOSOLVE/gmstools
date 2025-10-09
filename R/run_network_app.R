run_network_app <- function(){
  # app.R — Two-tab OSINT Friend Networks: (1) Parse & Save, (2) Build Network
  options(shiny.maxRequestSize = 1024^3)  # 1 GB upload cap for the parser
  
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
  
  normalize_fb_url <- function(url) {
    url <- ifelse(startsWith(url, "/"),
                  paste0("https://www.facebook.com", url),
                  url)
    url <- sub("[#?].*$", "", url)
    url <- sub("/+$", "/", paste0(url, "/"))
    url
  }
  
  looks_like_profile <- function(url) {
    grepl("facebook\\.com/(profile\\.php\\?id=\\d+/?$|[A-Za-z0-9._-]+/?$)", url) &
      !grepl("facebook\\.com/(help|policy|privacy|settings|login\\.php|watch|marketplace|groups|events|pages|gaming|ads|reel|stories|bookmark|search)", url)
  }
  
  is_obvious_nav_text <- function(x) {
    tolower(trimws(x)) %in% c("friends","mutual friends","about","timeline","photos",
                              "videos","more","find friends","add friend","message","follow")
  }
  
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
  
  extract_context_for_node <- function(node, name) {
    cont <- tryCatch({
      html_element(node, xpath = "./ancestor::*[self::div or self::li][1]")
    }, error = function(e) NULL)
    if (is.null(cont)) cont <- tryCatch(html_parent(node), error = function(e) NULL)
    if (is.null(cont)) return(NA_character_)
    txt <- tryCatch(html_text2(cont), error = function(e) "")
    if (!nzchar(txt)) return(NA_character_)
    txt <- gsub("\\s+", " ", txt)
    if (nzchar(name)) {
      safe_name <- stringr::str_replace_all(name, "([.^$|()*+?{}\\[\\]\\\\])", "\\\\\\1")
      txt <- gsub(paste0("\\b", safe_name, "\\b"), "", txt)
      txt <- str_squish(txt)
    }
    parts <- unlist(strsplit(txt, "(?:\\s{2,}|\\s·\\s|\\s\\|\\s|\\n| – )"))
    parts <- trimws(parts)
    parts <- parts[nchar(parts) > 0]
    drops <- c("Add Friend","Add friend","Message","Follow","Options","More","Search",
               "Friends","Mutual friends","Friend","Following")
    parts <- parts[!parts %in% drops]
    parts <- parts[!grepl("mutual friends|^\\d+\\+?$", parts, ignore.case = TRUE)]
    priors <- grep("(?i)works at|lives in|from |studied at|education|college|university| at |\\bin\\b", parts, perl = TRUE)
    chosen <- if (length(priors)) parts[priors[1]] else if (length(parts)) parts[1] else NA_character_
    if (is.na(chosen) || !nzchar(chosen)) return(NA_character_)
    substr(chosen, 1, 160)
  }
  
  extract_friends_from_html <- function(html_path) {
    doc <- read_html(html_path, options = c("RECOVER","NOERROR","NOWARNING"))
    a_nodes <- html_elements(doc, "a")
    if (length(a_nodes) == 0) {
      return(tibble(name = character(), profile_url = character(), context = character()))
    }
    href <- tryCatch(html_attr(a_nodes, "href"), error = function(e) rep(NA_character_, length(a_nodes)))
    text <- tryCatch(html_text2(a_nodes),      error = function(e) html_text(a_nodes))
    idx  <- seq_along(a_nodes)
    
    raw <- tibble(
      idx  = idx,
      text = as.character(text),
      href = as.character(href)
    ) %>%
      filter(!is.na(href), nzchar(href)) %>%
      mutate(href = normalize_fb_url(href))
    
    candidates <- raw %>%
      filter(grepl("facebook\\.com", href)) %>%
      filter(!grepl("sharer\\.php|photo\\.php|video\\.php|permalink|privacy|help|settings|login\\.php", href)) %>%
      filter(!grepl("watch|marketplace|groups|events|gaming|ads|reel|stories|bookmark|people/|pages/", href)) %>%
      filter(!grepl("friends(_mutual)?|sk=friends|friend_requests", href)) %>%
      filter(looks_like_profile(href)) %>%
      mutate(text = trimws(text)) %>%
      filter(nchar(text) > 1, !is_obvious_nav_text(text))
    
    if (nrow(candidates) == 0) {
      return(tibble(name = character(), profile_url = character(), context = character()))
    }
    
    contexts <- purrr::map2_chr(candidates$idx, candidates$text, function(i, nm) {
      node <- a_nodes[[i]]
      if (inherits(node, "xml_node")) extract_context_for_node(node, nm) else NA_character_
    })
    
    out <- candidates %>%
      transmute(name = text, profile_url = href, context = contexts) %>%
      group_by(profile_url) %>% slice(1) %>% ungroup() %>%
      filter(nchar(trimws(name)) > 0) %>%
      arrange(tolower(name))
    
    out
  }
  
  # ------------------ Theme ------------------
  
  theme <- bs_theme(
    version   = 5,
    bootswatch = "flatly",
    base_font  = font_google("Inter")
  )
  
  # ------------------ UI ------------------
  
  ui <- page_fluid(
    theme = theme,
    useShinyjs(),
    titlePanel("OSINT Friend Networks"),
    tabsetPanel(
      id = "tabs",
      # --------- TAB 1: Parse & Save ----------
      tabPanel(
        title = "Step 1. Parse Friend Networks",
        tags$p(
          "Open the person's profile → Friends, scroll to the bottom to load all entries, ",
          "then in your browser: ", tags$strong("File → Save Page As… → Webpage, Complete"), "."
        ),
        tags$hr(),
        fluidRow(
          column(
            5,
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
          column(
            7,
            h4("Parsed Friends"),
            DTOutput("friends_table"),
            tags$hr(),
            tags$small(
              "This app structures data from a file you saved while viewing content you could already access. ",
              "Follow applicable laws and platform ToS."
            )
          )
        )
      ),
      # --------- TAB 2: Build Network ----------
      tabPanel(
        title = "Step 2. Build Network",
        fluidRow(
          column(
            4,
            h4("Select saved POI networks"),
            uiOutput("saved_picker"),
            actionLink("select_all", "Select all"),
            span(" | "),
            actionLink("clear_all", "Clear"),
            tags$hr(),
            sliderInput("threshold", "Friend appears in at least N POIs",
                        min = 1, max = 2, value = 1, step = 1),
            selectInput("community_algo", "Community Detection",
                        choices = c("Walktrap", "Louvain", "Infomap", "Edge betweenness"),
                        selected = "Walktrap"),
            checkboxInput("highlight_owners", "Style POI (central) nodes", TRUE),
            selectizeInput("highlight_ids", "Highlight nodes", choices = NULL, multiple = TRUE),
            selectizeInput("path_ids", "Shortest path (choose two nodes)",
                           choices = NULL, multiple = TRUE, options = list(maxItems = 2)),
            br(),
            downloadButton("download_report", "Download XLSX report")
          ),
          column(
            8,
            h4("Network"),
            withSpinner(visNetworkOutput("network", height = "650px"), type = 5, color = "#98A5A6")
          )
        )
      )
    )
  )
  
  # ------------------ Server ------------------
  
  server <- function(input, output, session) {
    
    # ---- State shared across tabs ----
    parsed <- reactiveVal(tibble(POI = character(), name = character(), context = character(), profile_url = character()))
    saved_networks <- reactiveVal(list())  # named list: key -> tibble(POI, name, context, profile_url)
    
    # ---------- TAB 1 logic: Parse & Save ----------
    
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
      if (!nzchar(poi)) {
        showNotification("Please enter a POI name before parsing.", type = "warning")
        return(NULL)
      }
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
      }, error = function(e) {
        showNotification(paste("Parsing failed:", conditionMessage(e)), type = "error")
      })
    })
    
    observeEvent(input$save_btn, {
      dat <- parsed()
      validate(need(nrow(dat) > 0, "No parsed data to save. Parse a file first."))
      poi <- unique(dat$POI)
      key <- paste0(poi, " — ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
      store <- saved_networks()
      store[[key]] <- dat
      saved_networks(store)
      updateSelectInput(session, "saved_choice", choices = names(store), selected = key)
      # Reset upload + clear preview
      reset("html_file")
      parsed(tibble(POI = character(), name = character(), context = character(), profile_url = character()))
      showNotification(paste("Saved network:", key), type = "message")
      # Switch the user to the Build tab (optional)
      updateTabsetPanel(session, "tabs", selected = "Step 2. Build Network")
    })
    
    observeEvent(input$load_saved, {
      choice <- input$saved_choice
      store <- saved_networks()
      if (nzchar(choice) && choice %in% names(store)) {
        parsed(store[[choice]])
        showNotification(paste("Loaded saved network:", choice), type = "message")
      } else {
        showNotification("Select a saved network first.", type = "warning")
      }
    })
    
    output$friends_table <- renderDT({
      dat <- parsed()
      if (nrow(dat) == 0) return(DT::datatable(tibble(), options = list(dom = 't')))
      DT::datatable(
        dat,
        rownames = FALSE,
        extensions = c('Buttons'),
        options = list(
          pageLength = 25,
          scrollX = TRUE,
          dom = 'Bfrtip',
          buttons = list(
            list(extend = "csv", text = "Download CSV",
                 title = paste0("facebook_friends_", format(Sys.time(), "%Y%m%d_%H%M%S")))
          )
        ),
        escape = FALSE
      )
    })
    
    output$download_csv <- downloadHandler(
      filename = function() paste0("facebook_friends_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv"),
      content = function(file) {
        dat <- parsed()
        validate(need(nrow(dat) > 0, "No data to export. Parse a file first."))
        write_csv(dat, file)
      }
    )
    
    # ---------- TAB 2 logic: Build Network ----------
    
    # Picker for which saved networks to include
    output$saved_picker <- renderUI({
      store <- saved_networks()
      keys <- names(store)
      if (!length(keys)) {
        tags$em("No saved POI networks yet. Use Step 1 to add some.")
      } else {
        checkboxGroupInput("include_networks", NULL, choices = keys, selected = keys)
      }
    })
    
    observeEvent(input$select_all, {
      store <- saved_networks()
      updateCheckboxGroupInput(session, "include_networks", selected = names(store))
    })
    observeEvent(input$clear_all, {
      updateCheckboxGroupInput(session, "include_networks", selected = character(0))
    })
    
    # Combined data frame from selected saved networks
    data_from_saved <- reactive({
      store <- saved_networks()
      sel <- input$include_networks
      validate(need(length(store) > 0, "No saved networks found."),
               need(length(sel) > 0, "Select at least one saved network."))
      df <- bind_rows(store[sel])
      # Rename to the schema expected by the graph builder
      df %>%
        rename(account_owner = POI,
               profile_name  = name,
               profile_link  = profile_url) %>%
        mutate(account_owner = as.character(account_owner),
               profile_name  = as.character(profile_name))
    })
    
    # Adapt threshold max to number of distinct POIs selected
    observe({
      df <- tryCatch(data_from_saved(), error = function(e) NULL)
      if (!is.null(df) && nrow(df)) {
        nPOI <- length(unique(df$account_owner))
        updateSliderInput(session, "threshold",
                          min = 1, max = max(2, nPOI), value = min(2, nPOI), step = 1)
      }
    })
    
    # Build the network graph object from saved data
    network_data <- reactive({
      df <- data_from_saved()
      validate(need(nrow(df) > 0, "No rows in selected networks."))
      
      # bipartite edges: account_owner (POI) -- profile_name (Friend)
      bipartite_edges <- df %>% select(account_owner, profile_name, profile_link, context)
      
      # Count in how many *distinct POIs* each friend appears
      friend_counts <- bipartite_edges %>%
        distinct(account_owner, profile_name, profile_link, context) %>%
        group_by(profile_name, profile_link) %>%
        summarise(count = n(), .groups = "drop")
      
      filtered_friends <- friend_counts %>% filter(count >= input$threshold)
      validate(need(nrow(filtered_friends) > 0,
                    "No friends meet the selected threshold. Try lowering it or selecting more POIs."))
      
      filtered_edges <- bipartite_edges %>%
        semi_join(filtered_friends, by = c("profile_name","profile_link")) %>%
        distinct(account_owner, profile_name, .keep_all = TRUE)
      
      # Build graph
      g <- graph_from_data_frame(filtered_edges, directed = FALSE)
      
      # Community detection
      algo <- input$community_algo
      comm <- switch(algo,
                     "Walktrap"         = cluster_walktrap(g),
                     "Louvain"          = cluster_louvain(g),
                     "Infomap"          = cluster_infomap(g),
                     "Edge betweenness" = cluster_edge_betweenness(g),
                     cluster_walktrap(g)
      )
      V(g)$community <- membership(comm)
      
      # Nodes table
      nodes <- data.frame(
        id = V(g)$name,
        label = V(g)$name,
        community = as.factor(V(g)$community),
        stringsAsFactors = FALSE
      )
      
      # Attach friend metadata (context/link) to matching friend nodes
      friend_info <- filtered_edges %>%
        group_by(profile_name) %>%
        summarise(
          profile_link = first(na.omit(profile_link)),
          context = first(na.omit(context)),
          .groups = "drop"
        )
      nodes <- left_join(nodes, friend_info, by = c("id" = "profile_name"))
      
      # Identify POIs (account_owner nodes)
      owners <- unique(filtered_edges$account_owner)
      nodes$is_poi <- nodes$id %in% owners
      
      # Titles for hover
      nodes$title <- ifelse(
        nodes$is_poi,
        paste0("<strong>POI</strong>"),
        ifelse(!is.na(nodes$context),
               paste0("<strong>Context:</strong> ", htmlEscape(nodes$context),
                      ifelse(!is.na(nodes$profile_link),
                             paste0("<br><strong>Link:</strong> ", htmlEscape(nodes$profile_link)), "")),
               ifelse(!is.na(nodes$profile_link),
                      paste0("<strong>Link:</strong> ", htmlEscape(nodes$profile_link)), "")
        )
      )
      
      # Node sizing by degree
      deg <- degree(g)
      minD <- min(deg); maxD <- max(deg)
      size <- if (maxD == minD) rep(28, length(deg)) else 12 + (deg - minD) / (maxD - minD) * (46 - 12)
      nodes$size <- size
      
      # Distinguish POIs visually
      if (isTRUE(input$highlight_owners)) {
        nodes$shape <- ifelse(nodes$is_poi, "diamond", "dot")
        nodes$color.background <- ifelse(nodes$is_poi, "#ffffff", NA)
        nodes$color.border <- ifelse(nodes$is_poi, "#000000", NA)
      } else {
        nodes$shape <- "dot"
      }
      
      # Edge table
      edges <- as.data.frame(get.edgelist(g))
      colnames(edges) <- c("from", "to")
      edges$width <- 1
      
      list(nodes = nodes, edges = edges, g = g, owners = owners)
    })
    
    # Keep the highlight/path pickers synced with current graph
    observe({
      nd <- tryCatch(network_data(), error = function(e) NULL)
      if (!is.null(nd)) {
        choices <- nd$nodes$id
        updateSelectizeInput(session, "highlight_ids", choices = choices, server = TRUE)
        updateSelectizeInput(session, "path_ids",      choices = choices, server = TRUE)
      }
    })
    
    # Mutually-exclusive highlight vs path (UX)
    observe({
      if (!is.null(input$path_ids) && length(input$path_ids) > 0) {
        updateSelectizeInput(session, "highlight_ids", selected = character(0))
        disable("highlight_ids")
      } else {
        enable("highlight_ids")
      }
    })
    observe({
      if (!is.null(input$highlight_ids) && length(input$highlight_ids) > 0) {
        updateSelectizeInput(session, "path_ids", selected = character(0))
        disable("path_ids")
      } else {
        enable("path_ids")
      }
    })
    
    # Render network
    output$network <- renderVisNetwork({
      nd <- network_data()
      nodes <- nd$nodes
      edges <- nd$edges
      g     <- nd$g
      
      # Color by community
      groups <- sort(unique(nodes$community))
      pal <- brewer.pal(min(length(groups), 8), "Spectral")
      if (length(groups) > length(pal)) pal <- rep(pal, length.out = length(groups))
      color_map <- setNames(pal, groups)
      nodes$color.background <- ifelse(is.na(nodes$color.background),
                                       color_map[as.character(nodes$community)],
                                       nodes$color.background)
      nodes$color.border <- ifelse(is.na(nodes$color.border), "black", nodes$color.border)
      edges$color <- "gray"
      
      # Optional “highlight nodes”
      if (!is.null(input$highlight_ids) && length(input$highlight_ids) > 0) {
        highlight_set <- unique(unlist(lapply(input$highlight_ids, function(id) {
          nbrs <- neighbors(g, id, mode = "all")
          c(id, as_ids(nbrs))
        })))
        nodes$color.background[!(nodes$id %in% highlight_set)] <- "#d9d9d9"
        nodes$color.border[!(nodes$id %in% highlight_set)] <- "#bdbdbd"
      }
      
      # Optional “shortest path” view
      if (!is.null(input$path_ids) && length(input$path_ids) == 2) {
        sp <- shortest_paths(g, from = input$path_ids[1], to = input$path_ids[2], output = "both")
        sp_nodes <- as_ids(sp$vpath[[1]])
        sp_edge_ids <- unlist(sp$epath[[1]])
        nodes$color.background[nodes$id %in% sp_nodes] <- "#ffb74d"
        nodes$color.border[nodes$id %in% sp_nodes] <- "#e65100"
        
        if (length(sp_edge_ids) > 0) {
          edges$color[sp_edge_ids] <- "#e65100"
          edges$width[sp_edge_ids] <- 3
        }
        # dim non-path edges
        non_sp_edges <- setdiff(seq_len(nrow(edges)), sp_edge_ids)
        edges$color[non_sp_edges] <- "#d0d0d0"
      }
      
      visNetwork(nodes, edges) %>%
        visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
        visPhysics(solver = "forceAtlas2Based", stabilization = TRUE)
    })
    
    # Download report (centralities + metadata)
    output$download_report <- downloadHandler(
      filename = function() paste0("network_report_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".xlsx"),
      content = function(file) {
        nd <- network_data()
        g  <- nd$g
        df <- data_from_saved()
        
        deg <- degree(g)
        bet <- betweenness(g)
        clo <- closeness(g)
        eig <- eigen_centrality(g)$vector
        comm <- V(g)$community
        
        node_info <- data.frame(
          Node        = names(deg),
          Degree      = as.numeric(deg),
          Betweenness = as.numeric(bet),
          Closeness   = as.numeric(clo),
          Eigenvector = as.numeric(eig),
          Community   = as.character(comm),
          stringsAsFactors = FALSE
        )
        
        # Attach friend link/context when available
        profile_info <- df %>%
          distinct(profile_name, profile_link, context)
        report <- left_join(node_info, profile_info, by = c("Node" = "profile_name"))
        
        # POI flag
        owners <- unique(df$account_owner)
        report$POI <- report$Node %in% owners
        
        write_xlsx(report, path = file)
      }
    )
  }
  
  # Safe fallback for %||% without rlang
  `%||%` <- function(x, y) if (is.null(x) || (length(x) == 1 && is.na(x))) y else x
  
  shinyApp(ui, server)
  
}

run_network_app()
