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
  
  # ---- App registry (workflow order 1 -> 8) ----
  # Flow: establish template, search, record to DMS, analyse, OSINT networks, QA/management,
  #       shared URL overlap, public ECOSOLVE dashboard
  app_registry <- tibble::tibble(
    key   = c(
      "data_model",
      "search_router",
      "ecosolve_gms",
      "intel_dashboard",
      "network_v2",
      "admin_panel",
      "hash_app",
      "ecosolve_dash"
    ),
    step  = c(1, 2, 3, 4, 5, 6, 7, 8),
    title = c(
      "Data Model Builder",
      "GMS Search Router",
      "Eco-Solve Global Monitoring System",
      "Intel Dashboard",
      "OSINT Friend Networks",
      "GMS Admin & QA Panel",
      "URL Hash Overlap Hub",
      "ECOSOLVE Dashboard"
    ),
    desc  = c(
      "Define what data the team will capture and standardise field templates.",
      "Search for and collect online ads across platforms using native searches and dork syntax.",
      "Enter validated ads into the Data Management System, monitor operational cases, and use the AI webscraper.",
      "Explore, visualise, and summarise captured data for IWT intelligence.",
      "Identify trafficker networks and POI accounts from social graphs.",
      "Run quality control and assurance checks, review coverage, and support management decisions.",
      "Hash URLs, push sanitized hashes to a shared backend, and see overlaps with partner organisations.",
      "View high-level ECOSOLVE indicators and dashboards."
    ),
    cta   = c(
      "Establish data model",
      "Search & collect data",
      "Record & monitor data",
      "Analyse data",
      "Map trafficker networks",
      "Review QA & management",
      "Check shared URL overlaps",
      "Open ECOSOLVE dashboard"
    ),
    icon  = c(
      "layer-group",       # stacked model
      "search",
      "database",
      "chart-line",
      "project-diagram",
      "clipboard-check",
      "key",
      "gauge-high"
    )
  )
  
  # External website keys (no child process)
  external_keys <- c("ecosolve_gms", "ecosolve_dash")
  
  external_url <- function(key) {
    switch(
      key,
      "ecosolve_gms"  = "https://gms.ecosolve.eco/login",
      "ecosolve_dash" = "https://www.ecosolve.eco/dashboard",
      NULL
    )
  }
  
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
    # External steps do not spawn a child; iframe just points to URL
    if (key %in% external_keys) return(invisible(TRUE))
    
    if (is_running(key)) return(invisible(TRUE))
    
    child_port <- httpuv::randomPort()
    proc_state$ports[[key]] <- child_port
    
    # Expression that runs inside a fresh R session
    run_expr <- switch(
      key,
      "data_model"      = sprintf(
        "shiny::runApp(gmstools::data_model_app(), port=%d, host='127.0.0.1', launch.browser=FALSE)",
        child_port
      ),
      "search_router"   = sprintf(
        "shiny::runApp(gmstools::gms_search_router(), port=%d, host='127.0.0.1', launch.browser=FALSE)",
        child_port
      ),
      "intel_dashboard" = sprintf(
        "gmstools::run_intel_dashboard(launch.browser=FALSE, port=%d)",
        child_port
      ),
      "network_v2"      = sprintf(
        "shiny::runApp(gmstools::run_network_appV2(), port=%d, host='127.0.0.1', launch.browser=FALSE)",
        child_port
      ),
      "admin_panel"     = sprintf(
        "shiny::runApp(gmstools::admin_panel(), port=%d, host='127.0.0.1', launch.browser=FALSE)",
        child_port
      ),
      "hash_app"        = sprintf(
        "shiny::runApp(gmstools::hash_app(), port=%d, host='127.0.0.1', launch.browser=FALSE)",
        child_port
      ),
      stop("Unknown app key: ", key, call. = FALSE)
    )
    
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
    version    = 5,
    bootswatch = "darkly",
    base_font  = bslib::font_google("Inter")
  )
  
  card_button <- function(step, key, title, desc, cta, icon) {
    tags$div(
      class = "hub-card",
      tags$button(
        type = "button",
        class = "hub-card-btn",
        onclick = sprintf(
          "Shiny.setInputValue('launch_app', '%s', {priority:'event'});",
          key
        ),
        tags$div(
          class = "flow-card-header",
          tags$div(class = "flow-step-badge", step),
          tags$div(class = "hub-icon", shiny::icon(icon))
        ),
        tags$div(
          class = "hub-card-text",
          tags$div(
            class = "hub-title",
            sprintf("%d. %s", step, title)
          ),
          tags$div(class = "hub-desc", desc)
        ),
        tags$div(
          class = "flow-cta-row",
          tags$span(class = "flow-cta-label", cta),
          tags$span(class = "flow-cta-arrow", shiny::icon("arrow-right-long"))
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

        body {
          background:
            radial-gradient(1200px 800px at 10% 0%, rgba(34,211,238,0.10), transparent 60%),
            radial-gradient(1000px 700px at 90% 20%, rgba(34,211,238,0.08), transparent 55%),
            var(--hub-bg);
          color: var(--hub-text);
        }

        .hub-shell{ padding: 26px; height: 100vh; box-sizing: border-box; }
        .hub-header{
          display:flex; align-items:center; justify-content:space-between;
          gap:16px; margin-bottom: 18px;
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
          position: relative;
          display:grid;
          grid-template-columns: repeat(3, minmax(0, 1fr));
          gap: 16px;
          margin-top: 18px;
          max-width: 1200px;
          margin-left:auto;
          margin-right:auto;
        }

        @media (max-width: 1100px){
          .hub-grid{
            grid-template-columns: repeat(2, minmax(0, 1fr));
          }
        }

        @media (max-width: 780px){
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
          padding: 16px 16px 14px 16px;
          color: var(--hub-text);
          display:flex;
          flex-direction:column;
          gap: 6px;
          align-items:flex-start;
        }

        .hub-card-btn:hover{
          background: linear-gradient(180deg, var(--hub-card2), rgba(0,0,0,0.10));
          cursor: pointer;
        }

        .flow-card-header{
          width: 100%;
          display:flex;
          align-items:center;
          justify-content:space-between;
          margin-bottom: 4px;
        }

        .flow-step-badge{
          min-width: 26px;
          height: 26px;
          border-radius: 999px;
          border: 1px solid rgba(34,211,238,0.7);
          display:flex;
          align-items:center;
          justify-content:center;
          font-size: 12px;
          font-weight: 600;
          color: var(--hub-accent);
          background: radial-gradient(circle at 30% 0%, rgba(34,211,238,0.32), transparent 65%);
          box-shadow: 0 0 12px rgba(34,211,238,0.7);
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

        .flow-cta-row{
          margin-top: 6px;
          display:flex;
          align-items:center;
          justify-content:space-between;
          font-size: 12px;
          color: var(--hub-accent2);
        }

        .flow-cta-label{
          letter-spacing: 0.03em;
          text-transform: uppercase;
        }

        .flow-cta-arrow i{
          font-size: 13px;
        }

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
          font-size: 12px;
          padding: 6px 10px;
        }
        .btn-hub:hover{ background: rgba(34,211,238,0.14) !important; }
        .btn-hub-danger{
          border-radius: 14px !important;
          border: 1px solid rgba(239,68,68,0.25) !important;
          background: rgba(239,68,68,0.08) !important;
          color: var(--hub-text) !important;
          font-size: 12px;
          padding: 6px 10px;
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
          tags$div(class = "hub-logo", shiny::icon("house")),
          tags$div(
            class = "hub-titlebar",
            tags$div(class = "h1", "GMS App Hub"),
            tags$p(
              class = "hub-sub",
              "Step through the GMS workflow: define data, search, record, analyse, map networks, run QA/management checks, and coordinate on shared URLs."
            )
          )
        ),
        tags$div(
          class = "hub-top-actions",
          tags$span(class = "pill", uiOutput("status_pill")),
          actionButton("btn_devinfo", "Dev info", class = "btn btn-hub"),
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
        if (!is.null(ttl) && key %in% external_keys) {
          tagList(shiny::icon("circle"), paste0("Viewing external: ", ttl))
        } else if (!is.null(ttl) && is_running(key)) {
          tagList(shiny::icon("circle"), paste0("Running: ", ttl))
        } else {
          tagList(shiny::icon("circle"), paste0("Selected: ", ttl))
        }
      }
    })
    
    render_home <- function() {
      cards <- Map(
        card_button,
        step  = app_registry$step,
        key   = app_registry$key,
        title = app_registry$title,
        desc  = app_registry$desc,
        cta   = app_registry$cta,
        icon  = app_registry$icon
      )
      tagList(
        tags$div(class = "hub-grid", cards)
      )
    }
    
    render_iframe <- function(key) {
      ttl <- app_registry$title[match(key, app_registry$key)]
      url <- if (key %in% external_keys) {
        external_url(key)
      } else {
        child_url(key)
      }
      if (is.null(url) || !nzchar(url)) url <- "about:blank"
      tagList(
        tags$div(
          class = "hub-view",
          tags$iframe(class = "hub-iframe", src = url)
        ),
        tags$div(
          style = "margin-top:10px; color: rgba(255,255,255,0.55); font-size:12px;",
          paste0("Viewing: ", ttl, "  |  ", url)
        )
      )
    }
    
    output$main_view <- renderUI({
      key <- current_key()
      if (is.null(key)) render_home() else render_iframe(key)
    })
    
    # Dev info modal
    observeEvent(input$btn_devinfo, {
      pkg_ver <- tryCatch(
        as.character(utils::packageVersion("gmstools")),
        error = function(e) "Unknown"
      )
      r_ver   <- R.version.string
      plat    <- paste(R.version$platform, collapse = "")
      timestamp <- as.character(Sys.time())
      
      showModal(
        modalDialog(
          title = "GMS App Hub · Dev Info",
          easyClose = TRUE,
          footer = modalButton("Close"),
          size = "m",
          tags$p("Environment snapshot for debugging and deployment checks:"),
          tags$ul(
            tags$li(tags$b("Developed by: "), "Russell Gray | Head of Data - ECOSOLVE | GI-TOC"),
            tags$li(tags$b("gmstools version: "), pkg_ver),
            tags$li(tags$b("R version: "), r_ver),
            tags$li(tags$b("Platform: "), plat),
            tags$li(tags$b("Session timestamp: "), timestamp)
          ),
          tags$hr(),
          tags$p("Codebase and issue tracking:"),
          tags$ul(
            tags$li(
              tags$a(
                href = "https://github.com/RJGray-ECOSOLVE/gmstools",
                target = "_blank",
                "gmstools GitHub repository"
              )
            ),
            tags$li(
              tags$a(
                href = "https://github.com/RJGray-ECOSOLVE/gmstools/issues",
                target = "_blank",
                "Open / review GitHub issues"
              )
            )
          )
        )
      )
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
      if (!(key %in% external_keys)) {
        stop_child(key)
        showNotification("Stopped app.", type = "message")
      } else {
        showNotification("External link has no local app to stop.", type = "message")
      }
      current_key(NULL)
    })
    
    observeEvent(input$launch_app, {
      key <- input$launch_app
      if (is.null(key) || !key %in% app_registry$key) return()
      
      start_child(key)
      current_key(key)
      showNotification("Launching…", type = "message", duration = 2)
    }, ignoreInit = TRUE)
    
    session$onSessionEnded(function() {
      for (k in app_registry$key) {
        if (!(k %in% external_keys)) stop_child(k)
      }
    })
  }
  
  app <- shinyApp(ui, server)
  
  if (isTRUE(launch.browser)) {
    shiny::runApp(app, port = port, launch.browser = TRUE)
  } else {
    shiny::runApp(app, port = port, launch.browser = FALSE)
  }
}
