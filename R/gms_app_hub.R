gms_app_hub <- function(launch.browser = TRUE, port = NULL) {
  
  required_pkgs <- c("shiny", "bslib", "shinyjs", "httpuv", "processx")
  install_if_missing <- function(pkgs) {
    missing <- pkgs[!vapply(pkgs, requireNamespace, FUN.VALUE = logical(1), quietly = TRUE)]
    if (length(missing) > 0) install.packages(missing, dependencies = TRUE)
  }
  install_if_missing(required_pkgs)
  
  suppressPackageStartupMessages({
    lapply(required_pkgs, function(p) library(p, character.only = TRUE))
  })
  
  `%||%` <- function(x, y) if (is.null(x)) y else x
  
  # ---- App registry (edit labels/icons as you like) ----
  # key must be unique and stable
  app_registry <- tibble::tibble(
    key   = c("data_model", "search_router", "intel_dashboard", "network_v2"),
    title = c("Data Model Builder", "GMS Search Router", "Intel Dashboard", "OSINT Friend Networks"),
    desc  = c(
      "Build/export field templates and data dictionaries.",
      "Launch native searches + Google dork packs across platforms.",
      "Flexdashboard for IWT intelligence analytics.",
      "Parse FB friends HTML, build networks, vet nodes, export to Maltego."
    ),
    icon  = c("database", "search", "chart-line", "project-diagram")
  )
  
  # ---- Child process management ----
  proc_state <- new.env(parent = emptyenv())
  proc_state$procs <- list()   # key -> processx::process
  proc_state$ports <- list()   # key -> port
  
  is_running <- function(key) {
    p <- proc_state$procs[[key]]
    !is.null(p) && p$is_alive()
  }
  
  stop_child <- function(key) {
    p <- proc_state$procs[[key]]
    if (!is.null(p) && p$is_alive()) {
      try(p$kill(), silent = TRUE)
    }
    proc_state$procs[[key]] <- NULL
    proc_state$ports[[key]] <- NULL
    invisible(TRUE)
  }
  
  # Start selected app in a background R process on a random free port
  start_child <- function(key) {
    if (is_running(key)) return(invisible(TRUE))
    
    child_port <- httpuv::randomPort()
    proc_state$ports[[key]] <- child_port
    
    # Build an expression that runs *inside* a fresh R session
    # IMPORTANT: gmstools must be installed for this to work (or adjust to source your functions)
    run_expr <- switch(
      key,
      "data_model" = sprintf("shiny::runApp(gmstools::data_model_app(), port=%d, host='127.0.0.1', launch.browser=FALSE)", child_port),
      "search_router" = sprintf("shiny::runApp(gmstools::gms_search_router(), port=%d, host='127.0.0.1', launch.browser=FALSE)", child_port),
      "intel_dashboard" = sprintf("gmstools::run_intel_dashboard(launch.browser=FALSE, port=%d)", child_port),
      "network_v2" = sprintf("shiny::runApp(gmstools::run_network_appV2(), port=%d, host='127.0.0.1', launch.browser=FALSE)", child_port),
      stop("Unknown app key: ", key, call. = FALSE)
    )
    
    # Launch background R
    # Use Rscript so it works cross-platform
    cmd <- c(
      "-e",
      paste0(
        "options(shiny.launch.browser=FALSE);",
        "suppressPackageStartupMessages(library(gmstools));",
        run_expr
      )
    )
    
    p <- processx::process$new(
      command = file.path(R.home("bin"), "Rscript"),
      args    = cmd,
      stdout  = "|",
      stderr  = "|",
      cleanup = TRUE
    )
    
    proc_state$procs[[key]] <- p
    invisible(TRUE)
  }
  
  child_url <- function(key) {
    prt <- proc_state$ports[[key]]
    if (is.null(prt)) return(NULL)
    paste0("http://127.0.0.1:", prt)
  }
  
  # ---- UI bits ----
  theme <- bslib::bs_theme(
    version = 5,
    bootswatch = "darkly",
    base_font = bslib::font_google("Inter")
  )
  
  card_button <- function(key, title, desc, icon) {
    tags$div(
      class = "hub-card",
      tags$button(
        type = "button",
        class = "hub-card-btn",
        onclick = sprintf("Shiny.setInputValue('launch_app', '%s', {priority:'event'});", key),
        tags$div(class = "hub-icon", shiny::icon(icon)),
        tags$div(
          class = "hub-card-text",
          tags$div(class = "hub-title", title),
          tags$div(class = "hub-desc", desc)
        )
      )
    )
  }
  
  ui <- bslib::page_fillable(
    theme = theme,
    shinyjs::useShinyjs(),
    tags$head(
      tags$style(HTML("
        :root{
          --hub-accent: #22d3ee;
          --hub-accent2:#06b6d4;
          --hub-bg: #0b1220;
          --hub-card: rgba(255,255,255,0.06);
          --hub-card2: rgba(255,255,255,0.10);
          --hub-border: rgba(255,255,255,0.12);
          --hub-text: rgba(255,255,255,0.92);
          --hub-muted: rgba(255,255,255,0.65);
        }

        body { background: radial-gradient(1200px 800px at 10% 0%, rgba(34,211,238,0.10), transparent 60%),
                           radial-gradient(1000px 700px at 90% 20%, rgba(34,211,238,0.08), transparent 55%),
                           var(--hub-bg);
               color: var(--hub-text);
        }

        .hub-shell{ padding: 26px; height: 100vh; box-sizing: border-box; }
        .hub-header{
          display:flex; align-items:center; justify-content:space-between; gap:16px; margin-bottom: 18px;
        }
        .hub-brand{
          display:flex; align-items:center; gap:12px;
        }
        .hub-logo{
          width:42px; height:42px; border-radius: 14px;
          background: linear-gradient(135deg, rgba(34,211,238,0.25), rgba(34,211,238,0.06));
          border: 1px solid rgba(34,211,238,0.22);
          box-shadow: 0 10px 30px rgba(0,0,0,0.35);
          display:flex; align-items:center; justify-content:center;
        }
        .hub-logo i{ color: var(--hub-accent); font-size: 18px; }

        .hub-titlebar .h1{
          font-size: 20px; margin:0; font-weight: 700;
        }
        .hub-sub{
          margin:0; color: var(--hub-muted); font-size: 13px;
        }

        .hub-top-actions{
          display:flex; gap:10px; align-items:center;
        }

        .hub-grid{
    display:grid;
    grid-template-columns: repeat(2, minmax(0, 1fr));
    gap: 14px;
    margin-top: 14px;
  }

  @media (max-width: 900px){
    .hub-grid{
      grid-template-columns: 1fr;
    }
  }

        .hub-card{
          background: linear-gradient(180deg, var(--hub-card), rgba(0,0,0,0.18));
          border: 1px solid var(--hub-border);
          border-radius: 18px;
          overflow:hidden;
          box-shadow: 0 18px 40px rgba(0,0,0,0.35);
        }

        .hub-card-btn{
          width: 100%;
          text-align: left;
          background: transparent;
          border: 0;
          padding: 16px;
          color: var(--hub-text);
          display:flex;
          gap: 14px;
          align-items:flex-start;
        }

        .hub-card-btn:hover{
          background: linear-gradient(180deg, var(--hub-card2), rgba(0,0,0,0.10));
          cursor: pointer;
        }

        .hub-icon{
          width: 44px; height: 44px; border-radius: 14px;
          background: rgba(34,211,238,0.10);
          border: 1px solid rgba(34,211,238,0.18);
          display:flex; align-items:center; justify-content:center;
          flex: 0 0 44px;
        }
        .hub-icon i{ color: var(--hub-accent); font-size: 18px; }

        .hub-title{ font-weight: 700; font-size: 15px; margin-bottom: 4px; }
        .hub-desc{ color: var(--hub-muted); font-size: 12.5px; line-height: 1.35; }

        .hub-view{
          height: calc(100vh - 90px);
          border-radius: 18px;
          overflow: hidden;
          border: 1px solid rgba(255,255,255,0.10);
          background: rgba(0,0,0,0.22);
          box-shadow: 0 18px 40px rgba(0,0,0,0.35);
        }

        .hub-iframe{
          width: 100%;
          height: 100%;
          border: 0;
          background: transparent;
        }

        .btn-hub{
          border-radius: 14px !important;
          border: 1px solid rgba(34,211,238,0.25) !important;
          background: rgba(34,211,238,0.08) !important;
          color: var(--hub-text) !important;
        }
        .btn-hub:hover{ background: rgba(34,211,238,0.14) !important; }
        .btn-hub-danger{
          border-radius: 14px !important;
          border: 1px solid rgba(239,68,68,0.25) !important;
          background: rgba(239,68,68,0.08) !important;
          color: var(--hub-text) !important;
        }
        .btn-hub-danger:hover{ background: rgba(239,68,68,0.14) !important; }

        .pill{
          display:inline-flex; align-items:center; gap:8px;
          padding: 8px 12px;
          border-radius: 999px;
          border: 1px solid rgba(255,255,255,0.12);
          background: rgba(255,255,255,0.06);
          color: var(--hub-muted);
          font-size: 12px;
        }
      "))
    ),
    tags$div(
      class = "hub-shell",
      tags$div(
        class = "hub-header",
        tags$div(
          class = "hub-brand",
          tags$div(class = "hub-logo", shiny::icon("globe")),
          tags$div(
            class = "hub-titlebar",
            tags$div(class = "h1", "GMS App Hub"),
            tags$p(class = "hub-sub", "Launch tools, return to home, switch apps (local session).")
          )
        ),
        tags$div(
          class = "hub-top-actions",
          tags$span(class = "pill", uiOutput("status_pill")),
          actionButton("btn_home", "Home", class = "btn btn-hub"),
          actionButton("btn_stop", "Stop current app", class = "btn btn-hub-danger")
        )
      ),
      uiOutput("main_view")
    )
  )
  
  server <- function(input, output, session) {
    
    current_key <- reactiveVal(NULL)
    
    output$status_pill <- renderUI({
      key <- current_key()
      if (is.null(key)) {
        tagList(shiny::icon("circle"), "Idle")
      } else {
        ttl <- app_registry$title[match(key, app_registry$key)]
        if (is_running(key)) {
          tagList(shiny::icon("circle"), paste0("Running: ", ttl))
        } else {
          tagList(shiny::icon("circle"), paste0("Selected: ", ttl))
        }
      }
    })
    
    render_home <- function() {
      cards <- Map(
        card_button,
        key   = app_registry$key,
        title = app_registry$title,
        desc  = app_registry$desc,
        icon  = app_registry$icon
      )
      tagList(
        tags$div(class = "hub-grid", cards)
      )
    }
    
    render_iframe <- function(key) {
      ttl <- app_registry$title[match(key, app_registry$key)]
      url <- child_url(key)
      if (is.null(url) || !nzchar(url)) url <- "about:blank"
      tagList(
        tags$div(class = "hub-view",
                 tags$iframe(class = "hub-iframe", src = url)
        ),
        tags$div(style="margin-top:10px; color: rgba(255,255,255,0.55); font-size:12px;",
                 paste0("Viewing: ", ttl, "  |  ", url)
        )
      )
    }
    
    output$main_view <- renderUI({
      key <- current_key()
      if (is.null(key)) render_home() else render_iframe(key)
    })
    
    observeEvent(input$btn_home, {
      current_key(NULL)
    })
    
    observeEvent(input$btn_stop, {
      key <- current_key()
      if (is.null(key)) {
        showNotification("No app selected.", type = "message")
        return()
      }
      stop_child(key)
      showNotification("Stopped app.", type = "message")
      current_key(NULL)
    })
    
    observeEvent(input$launch_app, {
      key <- input$launch_app
      if (is.null(key) || !key %in% app_registry$key) return()
      
      # Start child if needed
      start_child(key)
      
      # Small delay helps on slower machines; iframe will still load once ready
      current_key(key)
      showNotification("Launching…", type = "message", duration = 2)
    }, ignoreInit = TRUE)
    
    session$onSessionEnded(function() {
      # Clean up child apps when hub closes
      for (k in app_registry$key) stop_child(k)
    })
  }
  
  app <- shinyApp(ui, server)
  
  if (isTRUE(launch.browser)) {
    shiny::runApp(app, port = port, launch.browser = TRUE)
  } else {
    shiny::runApp(app, port = port, launch.browser = FALSE)
  }
}
