hash_app <- function(){
# =======================================================================
# IWT Hash Overlap Hub - Multi-organization URL hash coordination app
# =======================================================================
options(shiny.maxRequestSize = 1024^3)
# -----------------------------------------------------------------------
# Package setup - install missing packages on a fresh R instance
# -----------------------------------------------------------------------
required_packages <- c(
  "shiny",
  "bslib",
  "digest",
  "readxl",
  "readr",
  "DT",
  "dplyr",
  "tibble",
  "googlesheets4",
  "htmltools"
)

installed <- rownames(installed.packages())
for (pkg in required_packages) {
  if (!pkg %in% installed) {
    install.packages(pkg)
  }
}

library(shiny)
library(bslib)
library(digest)
library(readxl)
library(readr)
library(DT)
library(dplyr)
library(tibble)
library(googlesheets4)
library(htmltools)

# -----------------------------------------------------------------------
# Global configuration
# -----------------------------------------------------------------------

shared_key <- "HC_RandomGen_Seed"

use_shared_db   <- TRUE
shared_sheet_id <- "14bb1f7ZqkY9EaLK7WY9xvRW3jzc3YbZxTTKdjbu1yNY"
shared_sheet_name <- "hash_shared_db"

options(
  gargle_oauth_cache = ".secrets",
  gargle_oauth_email = TRUE
)

notice_levels <- tibble::tribble(
  ~code,    ~label,                        ~description,                                                                 ~color,
  "RED",    "Red – Active LE case",        "Sensitive law enforcement work in progress. Coordinate before any takedown.", "#e74c3c",
  "YELLOW", "Yellow – OSINT / monitoring", "Ongoing OSINT case building or monitoring. Avoid uncoordinated removal.",     "#f1c40f",
  "SILVER", "Silver – Takedown priority",  "Content suitable for platform removal or deplatforming.",                     "#bdc3c7",
  "GREEN",  "Green – Resolved / archived", "Historic or resolved material. No active follow up expected.",                "#2ecc71",
  "NONE",   "None / unassigned",           "No notice has been assigned yet.",                                            "#7f8c8d"
)

# -----------------------------------------------------------------------
# Helper functions
# -----------------------------------------------------------------------

has_shared_db_config <- function() {
  use_shared_db && nzchar(shared_sheet_id)
}

ensure_gs4_auth <- function() {
  if (!has_shared_db_config()) return(invisible(NULL))
  googlesheets4::gs4_auth(
    email  = TRUE,
    scopes = "https://www.googleapis.com/auth/spreadsheets",
    cache  = ".secrets"
  )
}

hash_url <- function(url) {
  u <- trimws(tolower(as.character(url)))
  if (!nzchar(u) || is.na(u)) return(NA_character_)
  digest::hmac(
    key    = shared_key,
    object = u,
    algo   = "sha256"
  )
}

empty_db_template <- function() {
  tibble::tibble(
    hash       = character(),
    org        = character(),
    url_type   = character(),
    species    = character(),
    notice     = character(),
    created_at = character(),
    updated_at = character()
  )
}

ensure_db_columns <- function(db) {
  required_cols <- c("hash", "org", "url_type", "species",
                     "notice", "created_at", "updated_at")
  if (is.null(db) || nrow(db) == 0) {
    db <- empty_db_template()
  } else {
    for (col in required_cols) {
      if (!col %in% names(db)) {
        db[[col]] <- NA_character_
      }
    }
    db <- db[, required_cols]
    db[] <- lapply(db, as.character)
  }
  db
}

read_shared_db <- function() {
  if (!has_shared_db_config()) {
    stop("Shared DB not configured. Edit use_shared_db and shared_sheet_id at the top of app.R.")
  }
  ensure_gs4_auth()
  db <- tryCatch(
    {
      googlesheets4::read_sheet(shared_sheet_id, sheet = shared_sheet_name,
                                col_types = "ccccccc")
    },
    error = function(e) {
      message("read_shared_db error: ", e$message)
      empty_db_template()
    }
  )
  ensure_db_columns(db)
}

write_shared_db <- function(db) {
  if (!has_shared_db_config()) {
    stop("Shared DB not configured. Edit use_shared_db and shared_sheet_id at the top of app.R.")
  }
  db <- ensure_db_columns(db)
  ensure_gs4_auth()
  googlesheets4::sheet_write(
    data  = db,
    ss    = shared_sheet_id,
    sheet = shared_sheet_name
  )
}

# -----------------------------------------------------------------------
# UI
# -----------------------------------------------------------------------

ui <- fluidPage(
  title = "IWT Hash Overlap Hub",
  theme = bs_theme(bootswatch = "lux"),
  
  tags$style(HTML("
    body {
      background-color: #020617;
      color: #e5e7eb;
    }
    p, label, .control-label, .help-block, h1, h2, h3, h4, h5, h6 {
      color: #e5e7eb;
    }
    .app-header-bar {
      background: linear-gradient(90deg, #020617, #111827);
      color: #f9fafb;
      padding: 10px 18px;
      margin: -10px -15px 12px -15px;
      display: flex;
      align-items: center;
      justify-content: space-between;
      box-shadow: 0 2px 8px rgba(15, 23, 42, 0.6);
      border-bottom: 1px solid #1f2937;
    }
    .app-header-title {
      font-size: 19px;
      font-weight: 700;
      letter-spacing: 0.08em;
      text-transform: uppercase;
    }
    .about-btn {
      border-radius: 20px;
      padding: 4px 12px;
      font-size: 12px;
      border-color: rgba(249,250,251,0.4);
      color: #e5e7eb;
      background-color: transparent;
    }
    .about-btn:hover {
      background-color: rgba(15,23,42,0.7);
      color: #f9fafb;
    }
    .well {
      background-color: #020617;
      border-radius: 12px;
      border: 1px solid #1f2937;
      box-shadow: 0 2px 6px rgba(15,23,42,0.45);
      color: #e5e7eb;
    }
    .tab-content {
      background-color: transparent;
    }
    .nav-tabs {
      border-bottom: 1px solid #1f2937;
    }
    .nav-tabs > li > a {
      background-color: #020617;
      border: 1px solid transparent;
      color: #9ca3af;
      border-radius: 0;
    }
    .nav-tabs > li > a:hover {
      background-color: #111827;
      color: #e5e7eb;
    }
    .nav-tabs > li.active > a,
    .nav-tabs > li.active > a:focus,
    .nav-tabs > li.active > a:hover {
      background-color: #111827;
      color: #f9fafb;
      border: 1px solid #1d4ed8;
      border-bottom-color: transparent;
    }
    .btn-primary {
      background-color: #1d4ed8;
      border-color: #1d4ed8;
    }
    .btn-primary:hover {
      background-color: #2563eb;
      border-color: #2563eb;
    }
    .btn-danger {
      background-color: #b91c1c;
      border-color: #b91c1c;
    }
    .btn-danger:hover {
      background-color: #dc2626;
      border-color: #dc2626;
    }
    .form-control {
      background-color: #020617;
      color: #e5e7eb;
      border: 1px solid #374151;
    }
    .form-control:focus {
      border-color: #60a5fa;
      box-shadow: none;
    }
    ::placeholder {
      color: #6b7280;
    }
    select.form-control {
      background-color: #020617;
    }
    .dataTables_wrapper .dataTables_length select,
    .dataTables_wrapper .dataTables_filter input {
      background-color: #020617;
      color: #e5e7eb;
      border: 1px solid #374151;
    }
    table.dataTable tbody tr {
      background-color: #020617;
      color: #e5e7eb;
    }
    table.dataTable thead th {
      background-color: #0b1120;
      color: #f3f4f6;
    }
    .modal-content {
      background-color: #020617;
      color: #e5e7eb;
      border: 1px solid #1f2937;
    }
    .modal-header, .modal-footer {
      border-color: #1f2937;
    }
    .modal-title {
      color: #f9fafb;
    }
    .notice-card {
      margin-top: 8px;
      border-radius: 12px;
      padding: 12px 16px;
      background: linear-gradient(135deg, #fdfdfd 0%, #f5f7fa 100%);
      border: 1px solid #dde3ec;
      display: flex;
      align-items: center;
      gap: 14px;
      box-shadow: 0 2px 6px rgba(15, 23, 42, 0.08);
      color: #111827;
    }
    .notice-swatch {
      width: 40px;
      height: 40px;
      border-radius: 10px;
      border: 2px solid rgba(15, 23, 42, 0.15);
      box-shadow: 0 0 10px rgba(15, 23, 42, 0.15);
      flex-shrink: 0;
    }
    .notice-text-title {
      font-weight: 700;
      font-size: 13px;
      letter-spacing: 0.03em;
      text-transform: uppercase;
      margin-bottom: 4px;
      color: #111827;
    }
    .notice-text-desc {
      font-size: 12px;
      margin-bottom: 0;
      color: #4b5563;
      line-height: 1.4;
    }
    .notice-pill {
      display: inline-flex;
      align-items: center;
      gap: 6px;
      padding: 2px 8px;
      border-radius: 9999px;
      font-size: 11px;
      font-weight: 600;
      background-color: #020617;
      border: 1px solid #374151;
      color: #e5e7eb;
    }
    .notice-pill-dot {
      width: 10px;
      height: 10px;
      border-radius: 9999px;
      box-shadow: 0 0 4px rgba(15,23,42,0.6);
      flex-shrink: 0;
    }
    .lds-dual-ring {
      display: inline-block;
      width: 40px;
      height: 40px;
    }
    .lds-dual-ring:after {
      content: \" \";
      display: block;
      width: 32px;
      height: 32px;
      margin: 4px;
      border-radius: 50%;
      border: 4px solid #0d6efd;
      border-color: #0d6efd transparent #0d6efd transparent;
      animation: lds-dual-ring 1.2s linear infinite;
    }
    @keyframes lds-dual-ring {
      0% { transform: rotate(0deg); }
      100% { transform: rotate(360deg); }
    }
  ")),
  
  # Header bar with About button
  div(
    class = "app-header-bar",
    div(class = "app-header-title", "IWT Hash Overlap Hub"),
    actionButton(
      "about_app",
      label = "About this app",
      icon = icon("info-circle"),
      class = "btn btn-outline-light btn-sm about-btn"
    )
  ),
  
  tabsetPanel(
    tabPanel(
      "1. Upload & Hash",
      br(),
      fluidRow(
        column(
          width = 4,
          wellPanel(
            h4("Upload your dataset"),
            fileInput(
              "file_upload",
              "Upload CSV or XLSX file",
              accept = c(".csv", ".xls", ".xlsx")
            ),
            textInput(
              "org_name",
              "Your organization / agency",
              placeholder = "e.g. ECOSOLVE GMS"
            ),
            radioButtons(
              "record_type",
              "Record type for this batch",
              choices = c(
                "Persons of interest (POIs)" = "POI",
                "Advertisements"            = "AD"
              ),
              selected = "AD"
            ),
            uiOutput("url_col_ui"),
            uiOutput("species_col_ui"),
            actionButton(
              "do_hash",
              "Hash URLs",
              class = "btn btn-primary btn-block"
            )
          )
        ),
        column(
          width = 8,
          h4("Preview of uploaded data"),
          helpText("This is a sample of the raw data you uploaded. URL and species columns are selected on the left."),
          DTOutput("preview_data")
        )
      )
    ),
    
    tabPanel(
      "2. Hashed dataset",
      br(),
      fluidRow(
        column(
          width = 12,
          wellPanel(
            h4("Hashed view for this batch"),
            helpText("Raw URLs stay local to your machine. Only hashes, species, type, and org are sent to the shared database."),
            downloadButton(
              "download_hashed_local",
              "Download hashed dataset (CSV)",
              class = "btn btn-success"
            )
          ),
          DTOutput("hashed_table")
        )
      )
    ),
    
    tabPanel(
      "3. Shared DB sync",
      br(),
      fluidRow(
        column(
          width = 4,
          wellPanel(
            h4("Shared database backend"),
            verbatimTextOutput("shared_status", placeholder = TRUE),
            tags$hr(),
            actionButton(
              "load_shared",
              "Reload shared DB",
              class = "btn btn-default"
            ),
            br(), br(),
            actionButton(
              "push_hashes",
              "Push my hashed URLs to shared DB",
              class = "btn btn-primary"
            )
          )
        ),
        column(
          width = 8,
          h4("Shared DB overview"),
          helpText("High level overview of all hashed entries across participating organizations."),
          DTOutput("shared_summary")
        )
      )
    ),
    
    tabPanel(
      "4. Overlap analysis",
      br(),
      fluidRow(
        column(
          width = 12,
          wellPanel(
            h4("Cross-organization overlap statistics"),
            helpText("This table shows how much of your current batch overlaps with other organizations in the shared DB."),
            DTOutput("overlap_table")
          )
        )
      )
    ),
    
    tabPanel(
      "5. Match inspector & notices",
      br(),
      fluidRow(
        column(
          width = 4,
          wellPanel(
            h4("Select organization scope"),
            uiOutput("partner_org_ui"),
            tags$hr(),
            h4("Assign notices"),
            selectInput(
              "notice_choice",
              "Notice to assign to selected rows",
              choices = setNames(notice_levels$code, notice_levels$label),
              selected = "RED"
            ),
            uiOutput("notice_card"),
            br(),
            actionButton(
              "apply_notice",
              "Apply notice to selected hashes",
              class = "btn btn-danger"
            )
          )
        ),
        column(
          width = 8,
          h4("Matching hashed URLs with selected scope"),
          helpText("The notice pill reflects current status. In 'All Orgs / Agencies' mode, all overlapping partners for a hash are shown in the Partner orgs column."),
          DTOutput("match_table")
        )
      )
    )
  )
)

# -----------------------------------------------------------------------
# Server
# -----------------------------------------------------------------------

server <- function(input, output, session) {
  
  # Pending state for overwrite confirmation
  rv <- reactiveValues(
    pending_hashes  = NULL,
    pending_mode    = NULL,  # 'ALL' or 'SINGLE'
    pending_partner = NULL
  )
  
  # -------- About modal --------
  observeEvent(input$about_app, {
    showModal(
      modalDialog(
        title = "About the IWT Hash Overlap Hub",
        easyClose = TRUE,
        footer = modalButton("Close"),
        size = "m",
        tagList(
          p("This app lets multiple organizations find overlapping online wildlife trade detections without exposing raw URLs or identifiers."),
          tags$h5("How URL hashing works"),
          tags$ul(
            tags$li("Each URL in your dataset is transformed with a cryptographic HMAC using the SHA-256 algorithm and a shared key."),
            tags$li("HMAC-SHA256 is a one-way function: there is no decode function that can reconstruct the original URL from the hash value, even if someone knows the algorithm and the key."),
            tags$li("The only theoretical way to recover a specific URL from a hash is to guess candidate URLs and hash them one by one until a match appears, which is not realistic at scale for arbitrary URLs.")
          ),
          tags$h5("What is stored in the shared database"),
          tags$ul(
            tags$li("Only sanitized data are pushed: the hash of the URL, your organization name, record type (POI vs advert), optional species label, notice level, and timestamps."),
            tags$li("No raw URLs, contact details, names, screenshots, or other identifying text are ever written to the shared sheet."),
            tags$li("If someone discovers the shared database, they see only opaque hash strings and high-level metadata. There is no direct way to convert those hashes back into real URLs.")
          ),
          p("The goal is to enable coordination on shared POIs and adverts while keeping operational details and true URLs inside each organization's own secure systems.")
        )
      )
    )
  })
  
  # -------- Upload + preview --------
  raw_data <- reactive({
    req(input$file_upload)
    ext <- tools::file_ext(input$file_upload$name)
    switch(
      ext,
      "csv" = readr::read_csv(input$file_upload$datapath, show_col_types = FALSE),
      "xls" = readxl::read_excel(input$file_upload$datapath),
      "xlsx" = readxl::read_excel(input$file_upload$datapath),
      {
        validate("Please upload a .csv, .xls, or .xlsx file.")
        NULL
      }
    )
  })
  
  output$preview_data <- renderDT({
    df <- raw_data()
    head(df, 20)
  }, options = list(pageLength = 10, scrollX = TRUE))
  
  output$url_col_ui <- renderUI({
    df <- raw_data()
    if (is.null(df)) return(NULL)
    selectInput(
      "url_col",
      "URL column",
      choices = names(df),
      selected = names(df)[1]
    )
  })
  
  output$species_col_ui <- renderUI({
    df <- raw_data()
    if (is.null(df)) return(NULL)
    selectInput(
      "species_col",
      "Species column (optional)",
      choices = c("None" = "", names(df)),
      selected = ""
    )
  })
  
  user_org <- reactive({
    req(input$org_name)
    trimws(input$org_name)
  })
  
  hashed_data <- reactiveVal(NULL)
  
  observeEvent(input$do_hash, {
    df <- raw_data()
    req(df)
    
    validate(
      need(input$url_col %in% names(df), "Selected URL column not found in data."),
      need(nzchar(input$org_name), "Please enter your organization / agency name.")
    )
    
    url_vec <- df[[input$url_col]]
    species_vec <- if (!is.null(input$species_col) &&
                       nzchar(input$species_col) &&
                       input$species_col %in% names(df)) {
      as.character(df[[input$species_col]])
    } else {
      rep(NA_character_, length(url_vec))
    }
    
    hashed_vec <- vapply(url_vec, hash_url, FUN.VALUE = character(1))
    
    hd <- tibble::tibble(
      local_row  = seq_along(url_vec),
      orig_url   = as.character(url_vec),
      hash       = hashed_vec,
      species    = species_vec,
      url_type   = input$record_type,
      org        = user_org()
    ) %>%
      filter(!is.na(hash) & nzchar(hash))
    
    hashed_data(hd)
    
    showNotification(
      paste0("Hashed ", nrow(hd), " records for org: ", user_org()),
      type = "message",
      duration = 4
    )
  })
  
  output$hashed_table <- renderDT({
    hd <- hashed_data()
    if (is.null(hd)) return(NULL)
    hd_display <- hd %>%
      select(local_row, org, url_type, species, hash)
    datatable(
      hd_display,
      options = list(pageLength = 15, scrollX = TRUE),
      rownames = FALSE
    )
  })
  
  output$download_hashed_local <- downloadHandler(
    filename = function() {
      paste0("hashed_dataset_", Sys.Date(), ".csv")
    },
    content = function(file) {
      hd <- hashed_data()
      if (is.null(hd)) {
        write.csv(data.frame(), file, row.names = FALSE)
      } else {
        write.csv(hd, file, row.names = FALSE)
      }
    }
  )
  
  # -------- Shared DB status --------
  output$shared_status <- renderText({
    if (!use_shared_db) {
      return("Shared DB backend is disabled.\n\nSet use_shared_db <- TRUE and provide a Google Sheet ID in shared_sheet_id at the top of app.R.")
    }
    if (!nzchar(shared_sheet_id)) {
      return("use_shared_db is TRUE but shared_sheet_id is empty.\n\nPaste your Google Sheet ID into shared_sheet_id at the top of app.R.")
    }
    paste(
      "Shared DB is configured.\n",
      "Spreadsheet ID:", shared_sheet_id, "\n",
      "Sheet name:    ", shared_sheet_name
    )
  })
  
  shared_db <- reactiveVal(empty_db_template())
  
  # -------- Load shared DB (with spinner) --------
  observeEvent(input$load_shared, {
    if (!has_shared_db_config()) {
      showNotification(
        "Shared DB not configured. Edit use_shared_db and shared_sheet_id at the top of app.R.",
        type = "error",
        duration = 6
      )
      return(NULL)
    }
    
    showModal(
      modalDialog(
        title = "Loading shared data",
        easyClose = FALSE,
        footer = NULL,
        div(
          style = "display:flex; flex-direction:column; align-items:center; justify-content:center; gap:12px; padding:10px 0;",
          div(class = "lds-dual-ring"),
          div(
            style = "text-align:center;",
            strong("Shared data are being loaded from the server."),
            tags$br(),
            span("Please wait a moment.")
          )
        )
      )
    )
    on.exit(removeModal(), add = TRUE)
    
    tryCatch(
      {
        db <- read_shared_db()
        shared_db(db)
        showNotification(
          paste0("Loaded shared DB with ", nrow(db), " rows and ",
                 length(unique(db$org)), " organizations."),
          type = "message",
          duration = 4
        )
      },
      error = function(e) {
        showNotification(
          paste("Error loading shared DB:", e$message),
          type = "error",
          duration = 10
        )
      }
    )
  })
  
  # -------- Push hashes (with spinner) --------
  observeEvent(input$push_hashes, {
    if (!has_shared_db_config()) {
      showNotification(
        "Shared DB not configured. Edit use_shared_db and shared_sheet_id at the top of app.R.",
        type = "error",
        duration = 6
      )
      return(NULL)
    }
    
    hd <- hashed_data()
    req(hd)
    
    base_push <- hd %>%
      select(hash, org, url_type, species) %>%
      distinct() %>%
      mutate(
        notice     = "NONE",
        created_at = as.character(Sys.time()),
        updated_at = as.character(Sys.time())
      )
    
    if (nrow(base_push) == 0) {
      showNotification("No hashed records to push.", type = "warning", duration = 4)
      return(NULL)
    }
    
    showModal(
      modalDialog(
        title = "Pushing hashed data",
        easyClose = FALSE,
        footer = NULL,
        div(
          style = "display:flex; flex-direction:column; align-items:center; justify-content:center; gap:12px; padding:10px 0;",
          div(class = "lds-dual-ring"),
          div(
            style = "text-align:center;",
            strong("Hashed data is being pushed to server."),
            tags$br(),
            span("Please wait a moment.")
          )
        )
      )
    )
    on.exit(removeModal(), add = TRUE)
    
    tryCatch(
      {
        remote_db <- read_shared_db()
        remote_db <- ensure_db_columns(remote_db)
        
        if (nrow(remote_db) > 0) {
          existing_keys <- remote_db %>%
            select(hash, org) %>%
            distinct()
          to_push <- anti_join(base_push, existing_keys, by = c("hash", "org"))
        } else {
          to_push <- base_push
        }
        
        if (nrow(to_push) == 0) {
          showNotification(
            "No new hash + org combinations to push (all already present in shared DB).",
            type = "warning",
            duration = 5
          )
          return(NULL)
        }
        
        new_db <- bind_rows(remote_db, to_push)
        write_shared_db(new_db)
        shared_db(new_db)
        
        showNotification(
          paste0("Pushed ", nrow(to_push), " new records to shared DB."),
          type = "message",
          duration = 4
        )
      },
      error = function(e) {
        showNotification(
          paste("Error pushing hashes to shared DB:", e$message),
          type = "error",
          duration = 10
        )
      }
    )
  })
  
  output$shared_summary <- renderDT({
    db <- shared_db()
    db <- ensure_db_columns(db)
    if (nrow(db) == 0) {
      return(datatable(
        data.frame(
          Metric = "No data in shared DB yet.",
          Value  = ""
        ),
        options = list(dom = "t"),
        rownames = FALSE
      ))
    }
    
    summary_tbl <- tibble::tibble(
      Metric = c(
        "Total rows",
        "Unique hashes (URLs)",
        "Unique organizations",
        "POI rows",
        "Advert rows"
      ),
      Value = c(
        nrow(db),
        length(unique(db$hash)),
        length(unique(db$org)),
        sum(db$url_type == "POI", na.rm = TRUE),
        sum(db$url_type == "AD",  na.rm = TRUE)
      )
    )
    
    datatable(summary_tbl, options = list(dom = "t"), rownames = FALSE)
  })
  
  # -------- Overlap analysis --------
  overlap_summary <- reactive({
    hd <- hashed_data()
    db <- shared_db()
    
    req(hd)
    db <- ensure_db_columns(db)
    
    if (nrow(db) == 0) {
      return(NULL)
    }
    
    my_org <- unique(hd$org)
    my_hashes <- unique(hd$hash)
    
    db_other <- db %>% filter(org != my_org)
    if (nrow(db_other) == 0) {
      return(NULL)
    }
    
    matches <- db_other %>%
      filter(hash %in% my_hashes)
    
    if (nrow(matches) == 0) {
      return(NULL)
    }
    
    total_user_hashes <- length(my_hashes)
    
    per_org <- matches %>%
      group_by(org) %>%
      summarise(
        n_matching_hashes = n_distinct(hash),
        .groups = "drop"
      ) %>%
      left_join(
        db %>% group_by(org) %>% summarise(total_hashes_org = n_distinct(hash), .groups = "drop"),
        by = "org"
      ) %>%
      mutate(
        pct_of_my_hashes    = round(100 * n_matching_hashes / total_user_hashes, 1),
        pct_of_their_hashes = round(100 * n_matching_hashes / total_hashes_org, 1)
      )
    
    overall_matches <- tibble::tibble(
      org                 = "ALL",
      n_matching_hashes   = n_distinct(matches$hash),
      total_hashes_org    = NA_integer_,
      pct_of_my_hashes    = round(100 * n_distinct(matches$hash) / total_user_hashes, 1),
      pct_of_their_hashes = NA_real_
    )
    
    bind_rows(overall_matches, per_org)
  })
  
  output$overlap_table <- renderDT({
    os <- overlap_summary()
    if (is.null(os)) {
      return(datatable(
        data.frame(
          Message = "No overlaps detected yet. Make sure your batch is hashed and the shared DB has data from other organizations."
        ),
        options = list(dom = "t"),
        rownames = FALSE
      ))
    }
    datatable(
      os,
      options = list(pageLength = 10, scrollX = TRUE),
      rownames = FALSE
    )
  })
  
  # -------- All matches (user vs shared DB) --------
  all_matches <- reactive({
    hd <- hashed_data()
    db <- shared_db()
    req(hd)
    db <- ensure_db_columns(db)
    if (nrow(db) == 0) return(NULL)
    
    my_org <- unique(hd$org)
    my_hashes <- unique(hd$hash)
    
    db_with_user_info <- db %>%
      filter(hash %in% my_hashes) %>%
      left_join(
        hd %>%
          select(hash, user_url = orig_url, user_species = species, user_url_type = url_type) %>%
          distinct(),
        by = "hash"
      )
    
    if (nrow(db_with_user_info) == 0) return(NULL)
    
    db_with_user_info
  })
  
  # -------- Partner org selection UI (with ALL option) --------
  output$partner_org_ui <- renderUI({
    am <- all_matches()
    if (is.null(am)) {
      return(helpText("No overlaps yet. Push your hashes and reload the shared DB."))
    }
    
    my_org <- unique(na.omit(hashed_data()$org))
    partners <- sort(unique(am$org[am$org != my_org]))
    
    if (length(partners) == 0) {
      return(helpText("No partner organizations with overlapping hashes yet."))
    }
    
    choices <- c("All Orgs / Agencies" = "ALL", setNames(partners, partners))
    
    selectInput(
      "partner_org",
      "Organization in overlap",
      choices = choices,
      selected = "ALL"
    )
  })
  
  # -------- Partner matches (logic rows for table) --------
  partner_matches <- reactive({
    am <- all_matches()
    req(am)
    
    my_org <- unique(na.omit(hashed_data()$org))
    partner_choice <- input$partner_org
    if (is.null(partner_choice)) partner_choice <- "ALL"
    
    if (partner_choice == "ALL") {
      # All partners: aggregate per hash, list partner orgs comma-delimited
      am_partner <- am %>% filter(org != my_org)
      if (nrow(am_partner) == 0) return(tibble())
      
      aggregated <- am_partner %>%
        group_by(hash) %>%
        summarise(
          user_url = {
            u <- user_url[!is.na(user_url) & nzchar(user_url)]
            if (length(u)) u[1] else NA_character_
          },
          user_species = {
            s <- user_species[!is.na(user_species) & nzchar(user_species)]
            if (length(s)) s[1] else NA_character_
          },
          user_url_type = {
            t <- user_url_type[!is.na(user_url_type) & nzchar(user_url_type)]
            if (length(t)) t[1] else NA_character_
          },
          partner_org = paste(sort(unique(org)), collapse = ", "),
          notice = {
            notices <- unique(notice)
            notices[is.na(notices) | notices == ""] <- "NONE"
            non_none <- notices[notices != "NONE"]
            if (length(non_none) == 0) {
              "NONE"
            } else if (length(unique(non_none)) == 1) {
              unique(non_none)
            } else {
              "MIXED"
            }
          },
          .groups = "drop"
        ) %>%
        mutate(
          partner_species   = NA_character_,
          partner_url_type  = NA_character_
        ) %>%
        select(
          hash,
          user_url,
          user_species,
          user_url_type,
          partner_org,
          partner_species,
          partner_url_type,
          notice
        ) %>%
        arrange(hash)
      
      aggregated
      
    } else {
      # Single partner org: one row per hash+org
      pm <- am %>%
        filter(org == partner_choice) %>%
        mutate(
          notice = ifelse(is.na(notice) | notice == "", "NONE", notice)
        ) %>%
        select(
          hash,
          user_url,
          user_species,
          user_url_type,
          partner_org = org,
          partner_species = species,
          partner_url_type = url_type,
          notice
        ) %>%
        arrange(hash)
      
      pm
    }
  })
  
  # -------- Match table with notice pill and hash link --------
  output$match_table <- renderDT({
    pm <- partner_matches()
    if (is.null(pm) || nrow(pm) == 0) {
      return(datatable(
        data.frame(
          Message = "No matching hashes with this selection."
        ),
        options = list(dom = "t"),
        rownames = FALSE
      ))
    }
    
    pm_joined <- pm %>%
      left_join(notice_levels, by = c("notice" = "code"))
    
    notice_html <- ifelse(
      is.na(pm_joined$notice),
      "",
      sprintf(
        '<span class="notice-pill"><span class="notice-pill-dot" style="background-color:%s;"></span>%s</span>',
        htmlEscape(ifelse(is.na(pm_joined$color), "#6b7280", pm_joined$color)),
        htmlEscape(ifelse(is.na(pm_joined$label), pm_joined$notice, pm_joined$label))
      )
    )
    
    hash_html <- ifelse(
      is.na(pm_joined$user_url) | pm_joined$user_url == "",
      pm_joined$hash,
      sprintf(
        '<a href="%s" target="_blank">%s</a>',
        htmlEscape(pm_joined$user_url),
        htmlEscape(pm_joined$hash)
      )
    )
    
    pm_display <- tibble::tibble(
      Notice          = notice_html,
      Hash            = hash_html,
      user_species    = pm_joined$user_species,
      user_url_type   = pm_joined$user_url_type,
      partner_org     = pm_joined$partner_org,
      partner_species = pm_joined$partner_species,
      partner_url_type= pm_joined$partner_url_type
    )
    
    datatable(
      pm_display,
      selection = "multiple",
      escape = FALSE,
      options = list(pageLength = 15, scrollX = TRUE),
      rownames = FALSE
    )
  })
  
  # -------- Notice preview card --------
  output$notice_card <- renderUI({
    code <- input$notice_choice
    info <- notice_levels[notice_levels$code == code, , drop = FALSE]
    if (nrow(info) == 0) return(NULL)
    
    color <- info$color[[1]]
    label <- info$label[[1]]
    desc  <- info$description[[1]]
    
    tags$div(
      class = "notice-card",
      tags$div(
        class = "notice-swatch",
        style = paste0("background-color:", color, ";")
      ),
      tags$div(
        class = "notice-text",
        tags$div(
          class = "notice-text-title",
          paste(code, "·", label)
        ),
        tags$p(
          class = "notice-text-desc",
          desc
        )
      )
    )
  })
  
  # -------- Helper to actually write notices (with spinner) --------
  apply_notice_to_db <- function(target_hashes, mode, partner_org_choice) {
    db <- shared_db()
    db <- ensure_db_columns(db)
    hd <- hashed_data()
    req(hd)
    my_org <- unique(na.omit(hd$org))
    new_code <- isolate(input$notice_choice)
    
    showModal(
      modalDialog(
        title = "Updating notices",
        easyClose = FALSE,
        footer = NULL,
        div(
          style = "display:flex; flex-direction:column; align-items:center; justify-content:center; gap:12px; padding:10px 0;",
          div(class = "lds-dual-ring"),
          div(
            style = "text-align:center;",
            strong("Notice levels are being updated in the shared DB."),
            tags$br(),
            span("Please wait a moment.")
          )
        )
      )
    )
    on.exit(removeModal(), add = TRUE)
    
    tryCatch(
      {
        if (mode == "ALL") {
          idx <- which(db$hash %in% target_hashes & db$org != my_org)
        } else {
          idx <- which(db$hash %in% target_hashes & db$org == partner_org_choice)
        }
        
        if (length(idx) == 0) {
          showNotification(
            "No matching rows in shared DB for the selected records.",
            type = "warning",
            duration = 5
          )
          return(NULL)
        }
        
        db$notice[idx]     <- new_code
        db$updated_at[idx] <- as.character(Sys.time())
        
        write_shared_db(db)
        shared_db(db)
        showNotification(
          paste0("Applied notice '", new_code, "' to ", length(idx), " rows in shared DB."),
          type = "message",
          duration = 4
        )
      },
      error = function(e) {
        showNotification(
          paste("Error updating notices in shared DB:", e$message),
          type = "error",
          duration = 10
        )
      }
    )
  }
  
  # -------- Apply notices (with overwrite confirmation) --------
  observeEvent(input$apply_notice, {
    pm <- partner_matches()
    db <- shared_db()
    db <- ensure_db_columns(db)
    req(pm)
    
    sel_idx <- input$match_table_rows_selected
    if (length(sel_idx) == 0) {
      showNotification("No rows selected. Select one or more hashes in the table first.", type = "warning", duration = 4)
      return(NULL)
    }
    
    selected_hashes <- unique(pm$hash[sel_idx])
    hd <- hashed_data()
    req(hd)
    my_org <- unique(na.omit(hd$org))
    
    partner_choice <- input$partner_org
    if (is.null(partner_choice) || partner_choice == "ALL") {
      mode <- "ALL"
      target_rows <- db %>% filter(hash %in% selected_hashes & org != my_org)
    } else {
      mode <- "SINGLE"
      target_rows <- db %>% filter(hash %in% selected_hashes & org == partner_choice)
    }
    
    if (nrow(target_rows) == 0) {
      showNotification(
        "No matching rows in shared DB for these records.",
        type = "warning",
        duration = 5
      )
      return(NULL)
    }
    
    existing_non_none <- any(target_rows$notice != "NONE" & !is.na(target_rows$notice))
    
    if (!existing_non_none) {
      # Safe to write directly
      apply_notice_to_db(
        target_hashes      = selected_hashes,
        mode               = mode,
        partner_org_choice = if (mode == "SINGLE") partner_choice else NULL
      )
    } else {
      # Ask for confirmation before overwriting
      rv$pending_hashes  <- selected_hashes
      rv$pending_mode    <- mode
      rv$pending_partner <- if (mode == "SINGLE") partner_choice else NULL
      
      showModal(
        modalDialog(
          title = "Overwrite existing notices?",
          "One or more of these records already have an assigned notice, are you sure you want to overwrite?",
          footer = tagList(
            modalButton("No"),
            actionButton("confirm_overwrite_yes", "Yes", class = "btn btn-danger")
          ),
          easyClose = TRUE
        )
      )
    }
  })
  
  # -------- Confirmation handler for overwrite --------
  observeEvent(input$confirm_overwrite_yes, {
    req(rv$pending_hashes, rv$pending_mode)
    removeModal()
    
    apply_notice_to_db(
      target_hashes      = rv$pending_hashes,
      mode               = rv$pending_mode,
      partner_org_choice = rv$pending_partner
    )
    
    rv$pending_hashes  <- NULL
    rv$pending_mode    <- NULL
    rv$pending_partner <- NULL
  })
}

# -----------------------------------------------------------------------
# Launch app
# -----------------------------------------------------------------------
shinyApp(ui = ui, server = server)

}
