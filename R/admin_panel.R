
admin_panel <- function(){
  # app.R - GMS Admin Panel (data entry + quality monitoring)
  # Run: shiny::runApp()
  
  options(shiny.maxRequestSize = 1024^3) # 1 GB
  
  req_pkgs <- c(
    "shiny", "bslib", "dplyr", "tidyr", "lubridate", "ggplot2", "DT",
    "readr", "stringr", "purrr", "tibble", "echarts4r", "htmlwidgets", "htmltools",
    "plotly"
  )
  
  missing_pkgs <- setdiff(req_pkgs, rownames(installed.packages()))
  if (length(missing_pkgs) > 0) {
    stop(
      "Missing packages: ", paste(missing_pkgs, collapse = ", "),
      "\nInstall them, then re-run."
    )
  }
  
  library(shiny)
  library(bslib)
  library(dplyr)
  library(tidyr)
  library(lubridate)
  library(ggplot2)
  library(DT)
  library(readr)
  library(stringr)
  library(purrr)
  library(tibble)
  library(echarts4r)
  library(htmlwidgets)
  library(htmltools)
  library(plotly)
  
  # -----------------------------
  # Helpers
  # -----------------------------
  std_names <- function(nms) {
    nms <- tolower(nms)
    nms <- gsub("[^a-z0-9]+", "_", nms)
    nms <- gsub("^_+|_+$", "", nms)
    nms <- gsub("_+", "_", nms)
    nms
  }
  
  is_blank <- function(x) {
    if (is.null(x)) return(TRUE)
    if (inherits(x, "Date") || inherits(x, "POSIXt")) return(is.na(x))
    y <- trimws(as.character(x))
    is.na(y) | y == "" | tolower(y) %in% c("na", "n/a", "null", "none")
  }
  
  coerce_logical_smart <- function(x) {
    if (is.logical(x)) return(x)
    if (is.numeric(x)) return(ifelse(is.na(x), NA, x != 0))
    y <- tolower(trimws(as.character(x)))
    ok <- y %in% c("true","false","t","f","1","0","yes","no","y","n","", "na", "n/a", "null", "none") | is.na(y)
    if (!all(ok, na.rm = TRUE)) return(x)
    out <- rep(NA, length(y))
    out[y %in% c("true","t","1","yes","y")] <- TRUE
    out[y %in% c("false","f","0","no","n")] <- FALSE
    out
  }
  
  parse_date_any <- function(x) {
    if (inherits(x, "Date")) return(x)
    if (inherits(x, "POSIXt")) return(as.Date(x))
    y <- trimws(as.character(x))
    y[y == ""] <- NA
    suppressWarnings(as.Date(parse_date_time(
      y,
      orders = c("Y-m-d", "Y/m/d", "d/m/Y", "m/d/Y", "d-m-Y", "m-d-Y", "Ymd", "dmY", "mdY"),
      exact = FALSE
    )))
  }
  
  clean_admin_data <- function(dat) {
    dat <- as_tibble(dat)
    
    idx_cols <- intersect(names(dat), c("Unnamed: 0", "unnamed: 0", "X", "x"))
    if (length(idx_cols) > 0) dat <- dat %>% select(-all_of(idx_cols))
    
    names(dat) <- std_names(names(dat))
    
    if (!("datahub" %in% names(dat)) && ("data_hub" %in% names(dat))) {
      dat <- dat %>% rename(datahub = data_hub)
    }
    if (!("datahub" %in% names(dat))) {
      dat <- dat %>% mutate(datahub = "Unknown")
    }
    
    if ("record_date" %in% names(dat)) dat <- dat %>% mutate(record_date = parse_date_any(record_date))
    if ("item_date_posted" %in% names(dat)) dat <- dat %>% mutate(item_date_posted = parse_date_any(item_date_posted))
    
    flag_cols <- intersect(
      names(dat),
      c(
        "was_recommended", "is_case_of_interest",
        "location_known", "price_available", "seller_contact_known",
        "item_haseggs", "item_multiple_detections",
        "is_group", "is_delivery_available",
        "is_seller_vulnerable_group", "is_bycatch"
      )
    )
    for (cc in flag_cols) dat[[cc]] <- coerce_logical_smart(dat[[cc]])
    
    char_cols <- names(dat)[vapply(dat, is.character, logical(1))]
    if (length(char_cols) > 0) {
      dat <- dat %>% mutate(across(all_of(char_cols), ~ str_squish(.x)))
      dat <- dat %>% mutate(across(all_of(char_cols), ~ ifelse(is_blank(.x), NA, .x)))
    }
    
    dat <- dat %>% mutate(datahub = ifelse(is_blank(datahub), "Unknown", as.character(datahub)))
    dat
  }
  
  maybe_call_clean_gms_data <- function(dat) {
    out <- NULL
    if (requireNamespace("gmstools", quietly = TRUE)) {
      if (exists("clean_gms_data", envir = asNamespace("gmstools"), inherits = FALSE)) {
        out <- tryCatch(gmstools::clean_gms_data(dat), error = function(e) NULL)
      }
    }
    if (is.null(out)) out <- dat
    out
  }
  
  get_fun <- function(fname) {
    if (requireNamespace("gmstools", quietly = TRUE)) {
      if (exists(fname, envir = asNamespace("gmstools"), inherits = FALSE)) {
        return(get(fname, envir = asNamespace("gmstools")))
      }
    }
    if (exists(fname, mode = "function", inherits = TRUE)) return(get(fname, mode = "function", inherits = TRUE))
    NULL
  }
  
  field_groups <- list(
    "Core" = c("url","platform_name","website_type","item_title","item_text","ad_language","search_term","record_date"),
    "Taxonomy" = c("item_common_name","item_taxa","item_phylum","item_class","item_order","item_family","item_genus","item_species","item_cites"),
    "Geo" = c("location_known","origin_country","destination_country","location_level0","location_level1","location_level2","ad_location","item_sold_in"),
    "Price" = c("price_available","item_price","item_currency","item_count","item_unit","payment_method"),
    "Seller and Group" = c("seller_contact_known","item_seller_name","item_seller_contact","is_group","group_type","group_name"),
    "Workflow" = c("was_recommended","is_case_of_interest","case_id","note"),
    "Delivery and Vulnerability" = c("is_delivery_available","delivery_method","is_seller_vulnerable_group","vulnerable_group","is_bycatch")
  )
  
  available_fields_for_group <- function(dat, grp) {
    cols <- field_groups[[grp]]
    intersect(cols, names(dat))
  }
  
  completeness_table <- function(dat, fields) {
    fields <- intersect(fields, names(dat))
    if (length(fields) == 0) return(tibble(field = character(), filled_n = integer(), missing_n = integer(), pct_filled = numeric()))
    total_n <- nrow(dat)
    filled <- vapply(fields, function(f) sum(!is_blank(dat[[f]])), integer(1))
    tibble(
      field = fields,
      filled_n = as.integer(filled),
      missing_n = as.integer(total_n - filled),
      pct_filled = if (total_n == 0) NA_real_ else filled / total_n
    ) %>% arrange(pct_filled, field)
  }
  
  completeness_by_hub <- function(dat, fields) {
    fields <- intersect(fields, names(dat))
    if (length(fields) == 0) return(tibble())
    dat %>%
      group_by(datahub) %>%
      summarise(
        n = n(),
        across(all_of(fields), ~ mean(!is_blank(.x)), .names = "{.col}"),
        .groups = "drop"
      ) %>%
      arrange(desc(n), datahub)
  }
  
  quality_issue_table <- function(dat) {
    if (nrow(dat) == 0) return(tibble())
    
    g <- function(col) if (col %in% names(dat)) dat[[col]] else rep(NA, nrow(dat))
    
    url <- g("url")
    rd  <- g("record_date")
    pd  <- g("item_date_posted")
    
    price_available <- g("price_available")
    item_price <- g("item_price")
    item_currency <- g("item_currency")
    seller_contact_known <- g("seller_contact_known")
    item_seller_contact <- g("item_seller_contact")
    location_known <- g("location_known")
    loc0 <- g("location_level0")
    loc1 <- g("location_level1")
    loc2 <- g("location_level2")
    adloc <- g("ad_location")
    is_group <- g("is_group")
    group_name <- g("group_name")
    item_common_name <- g("item_common_name")
    item_taxa <- g("item_taxa")
    
    dup_url <- !is_blank(url) & duplicated(url)
    delay_days <- suppressWarnings(as.numeric(rd - pd))
    
    issues <- tibble(
      missing_url = is_blank(url),
      missing_record_date = is.na(rd),
      duplicate_url = dup_url,
      
      price_flag_true_but_missing_price = (price_available %in% TRUE) & is_blank(item_price),
      price_present_but_flag_false = (!is_blank(item_price)) & (price_available %in% FALSE),
      currency_missing_when_price_present = (!is_blank(item_price)) & is_blank(item_currency),
      
      contact_flag_true_but_missing_contact = (seller_contact_known %in% TRUE) & is_blank(item_seller_contact),
      contact_present_but_flag_false = (!is_blank(item_seller_contact)) & (seller_contact_known %in% FALSE),
      
      location_known_true_but_no_location_fields = (location_known %in% TRUE) & is_blank(loc0) & is_blank(loc1) & is_blank(loc2) & is_blank(adloc),
      
      is_group_true_but_group_name_missing = (is_group %in% TRUE) & is_blank(group_name),
      
      common_name_present_but_taxa_missing = (!is_blank(item_common_name)) & is_blank(item_taxa),
      
      record_date_in_future = !is.na(rd) & rd > Sys.Date(),
      negative_delay_record_before_posted = !is.na(delay_days) & delay_days < 0,
      very_late_entry_gt_14d = !is.na(delay_days) & delay_days > 14
    )
    
    issues_long <- issues %>%
      mutate(row_id = row_number()) %>%
      pivot_longer(-row_id, names_to = "issue", values_to = "flag") %>%
      filter(flag %in% TRUE) %>%
      group_by(row_id) %>%
      summarise(
        issues = paste(issue, collapse = "; "),
        n_issues = n(),
        .groups = "drop"
      )
    
    dat %>%
      mutate(row_id = row_number()) %>%
      select(any_of(c("row_id","datahub","record_date","url","platform_name","item_common_name","origin_country","destination_country"))) %>%
      left_join(issues_long, by = "row_id") %>%
      mutate(
        n_issues = ifelse(is.na(n_issues), 0L, n_issues),
        issues = ifelse(is.na(issues), "", issues)
      ) %>%
      arrange(desc(n_issues), desc(record_date))
  }
  
  issue_counts <- function(issue_tbl) {
    if (nrow(issue_tbl) == 0) return(tibble(issue = character(), n = integer()))
    issue_tbl %>%
      filter(n_issues > 0, issues != "") %>%
      separate_rows(issues, sep = ";\\s*") %>%
      count(issues, sort = TRUE) %>%
      rename(issue = issues, n = n)
  }
  
  # -----------------------------
  # Your dms_audit (kept as-is)
  # -----------------------------
  dms_audit <- function(dat,
                        date_col = "record_date",
                        hub_col  = NULL,
                        week_start = 1,
                        fill_missing_weeks = TRUE,
                        free_y = TRUE,
                        flip = TRUE) {
    
    if (is.null(hub_col)) {
      if ("datahub" %in% names(dat)) {
        hub_col <- "datahub"
      } else if ("data_hub" %in% names(dat)) {
        hub_col <- "data_hub"
      } else {
        stop("dms_audit(): Provide `hub_col` or include `datahub`/`data_hub` in data.")
      }
    }
    if (!date_col %in% names(dat)) {
      stop(sprintf("dms_audit(): date_col '%s' not found.", date_col))
    }
    
    dat <- dat %>%
      dplyr::mutate(
        !!hub_col  := dplyr::coalesce(as.character(.data[[hub_col]]), ""),
        !!hub_col  := dplyr::if_else(.data[[hub_col]] == "", "Unknown", .data[[hub_col]]),
        !!date_col := lubridate::as_date(.data[[date_col]])
      )
    
    if (all(is.na(dat[[date_col]]))) {
      empty_tbl  <- tibble::tibble(week_start = as.Date(character()))
      empty_plot <- ggplot2::ggplot() + ggplot2::labs(title = "No valid dates in input data")
      return(list(plot = empty_plot, table = empty_tbl))
    }
    
    dat_weekly <- dat %>%
      dplyr::mutate(week_start = lubridate::floor_date(.data[[date_col]], unit = "week", week_start = week_start)) %>%
      dplyr::filter(!is.na(week_start)) %>%
      dplyr::count(!!rlang::sym(hub_col), week_start, name = "n") %>%
      dplyr::rename(datahub = !!rlang::sym(hub_col))
    
    if (nrow(dat_weekly) == 0) {
      empty_tbl  <- tibble::tibble(week_start = as.Date(character()))
      empty_plot <- ggplot2::ggplot() + ggplot2::labs(title = "No weekly records to plot")
      return(list(plot = empty_plot, table = empty_tbl))
    }
    
    if (isTRUE(fill_missing_weeks)) {
      all_weeks <- tibble::tibble(
        week_start = seq(min(dat_weekly$week_start), max(dat_weekly$week_start), by = "1 week")
      )
      hubs <- dplyr::distinct(dat_weekly, datahub)
      dat_weekly_complete <- hubs %>%
        tidyr::crossing(all_weeks) %>%
        dplyr::left_join(dat_weekly, by = c("datahub", "week_start")) %>%
        dplyr::mutate(n = tidyr::replace_na(n, 0L)) %>%
        dplyr::arrange(week_start, datahub)
    } else {
      dat_weekly_complete <- dat_weekly %>% dplyr::arrange(week_start, datahub)
    }
    
    p <- ggplot2::ggplot(dat_weekly_complete, ggplot2::aes(x = week_start, y = n)) +
      ggplot2::geom_col() +
      ggplot2::facet_wrap(~ datahub, scales = if (free_y) "free_y" else "fixed") +
      ggplot2::scale_x_date(date_breaks = "1 month", date_labels = "%b %Y",
                            expand = ggplot2::expansion(mult = c(0.01, 0.01))) +
      ggplot2::labs(
        title = "Weekly record entries by Data Hub",
        x = "Week starting (Mon)",
        y = "Records per week"
      ) +
      ggplot2::theme_minimal(base_size = 11) +
      ggplot2::theme(
        panel.grid.minor = ggplot2::element_blank(),
        axis.title.x = ggplot2::element_text(margin = ggplot2::margin(t = 6)),
        axis.title.y = ggplot2::element_text(margin = ggplot2::margin(r = 6))
      )
    
    if (isTRUE(flip)) p <- p + ggplot2::coord_flip()
    
    weekly_table <- dat_weekly_complete %>%
      dplyr::select(week_start, datahub, n) %>%
      tidyr::pivot_wider(names_from = datahub, values_from = n, values_fill = 0) %>%
      dplyr::arrange(week_start) %>%
      dplyr::mutate(Total = rowSums(dplyr::across(where(is.numeric))))
    
    list(plot = p, table = weekly_table)
  }
  
  # -----------------------------
  # UI
  # -----------------------------
  theme <- bs_theme(version = 5, bootswatch = "darkly")
  
  css <- "
.kpi-row { display:flex; gap:12px; flex-wrap:wrap; margin-bottom: 12px; }
.kpi-box {
  flex: 1 1 180px; min-width: 180px;
  border: 1px solid rgba(255,255,255,0.10);
  background: rgba(0,0,0,0.25);
  border-radius: 10px;
  padding: 12px 14px;
}
.kpi-title { font-size: 12px; opacity: 0.85; margin-bottom: 6px; }
.kpi-value { font-size: 22px; font-weight: 700; line-height: 1.1; }
.kpi-sub { font-size: 12px; opacity: 0.75; margin-top: 4px; }
.small-note { font-size: 12px; opacity: 0.75; }
hr.soft { border-top: 1px solid rgba(255,255,255,0.10); }
.btn-row { display:flex; gap:10px; flex-wrap:wrap; margin: 10px 0 12px 0; }
"

ui <- fluidPage(
  theme = theme,
  
  # ---- Head: existing CSS + Busy overlay CSS + Busy JS ----
  tags$head(
    tags$style(HTML(paste0(
      css,
      "
/* =========================
   Busy overlay (global)
   ========================= */
.gms-busy-wrap {
  position: fixed;
  inset: 0;
  z-index: 99999;
  display: none;
  align-items: center;
  justify-content: center;
  background: rgba(0,0,0,0.70);
  backdrop-filter: blur(2px);
}
.gms-busy-card{
  width: min(420px, 92vw);
  padding: 18px 18px 14px 18px;
  border-radius: 14px;
  border: 1px solid rgba(255,255,255,0.12);
  background: rgba(0,0,0,0.55);
  box-shadow: 0 12px 36px rgba(0,0,0,0.55);
  text-align: center;
}
.gms-busy-title{
  color: rgba(255,255,255,0.92);
  font-size: 16px;
  font-weight: 700;
  margin: 8px 0 4px 0;
}
.gms-busy-sub{
  color: rgba(255,255,255,0.70);
  font-size: 12px;
  margin: 0;
}
.gms-ring {
  width: 58px;
  height: 58px;
  border-radius: 999px;
  border: 4px solid rgba(255,255,255,0.18);
  border-top-color: rgba(0, 220, 255, 0.95);
  animation: gms-spin 0.9s linear infinite;
  margin: 0 auto;
}
@keyframes gms-spin { to { transform: rotate(360deg); } }
      "
    ))),
    tags$script(HTML("
(function(){
  function setBusy(on){
    var el = document.getElementById('gms-busy-wrap');
    if(!el) return;
    el.style.display = on ? 'flex' : 'none';
  }
  $(document).on('shiny:busy', function(){ setBusy(true); });
  $(document).on('shiny:idle', function(){ setBusy(false); });
})();
    "))
  ),
  
  # ---- Busy overlay UI (shows whenever Shiny is busy) ----
  tags$div(
    class = "gms-busy-wrap",
    id = "gms-busy-wrap",
    tags$div(
      class = "gms-busy-card",
      tags$div(class = "gms-ring"),
      tags$div(class = "gms-busy-title", "Loading and cleaning data…"),
      tags$p(class = "gms-busy-sub", "Please wait. This can take a moment for large uploads.")
    )
  ),
  
  titlePanel("GMS Admin Panel"),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      
      h5("Data"),
      fileInput("files", "Upload CSV file(s)", multiple = TRUE, accept = c(".csv")),
      actionButton("clear_data", "Clear loaded data"),
      hr(class = "soft"),
      
      h5("Filters"),
      uiOutput("hub_ui"),
      uiOutput("date_ui"),
      checkboxInput("include_unknown", "Include Unknown hub", value = TRUE),
      hr(class = "soft"),
      
      h5("Quality focus"),
      checkboxGroupInput(
        "quality_groups",
        "Field groups to score",
        choices = names(field_groups),
        selected = c("Core", "Taxonomy", "Geo", "Price", "Seller and Group")
      ),
      checkboxInput("show_only_flagged", "In issue table, show only flagged rows", value = TRUE),
      hr(class = "soft"),
      
      h5("Export"),
      downloadButton("dl_weekly", "Weekly table (CSV)"),
      br(), br(),
      downloadButton("dl_completeness", "Completeness (CSV)"),
      br(), br(),
      downloadButton("dl_issues", "Issues (CSV)"),
      br(), br(),
      downloadButton("dl_filtered", "Filtered data (CSV)"),
      hr(class = "soft"),
      
      div(class = "small-note", uiOutput("status_ui"))
    ),
    
    mainPanel(
      width = 9,
      tabsetPanel(
        id = "main_tabs",
        
        tabPanel(
          "Overview",
          div(class = "kpi-row",
              div(class = "kpi-box",
                  div(class = "kpi-title", "Records (filtered)"),
                  div(class = "kpi-value", textOutput("kpi_records", inline = TRUE)),
                  div(class = "kpi-sub", textOutput("kpi_datespan", inline = TRUE))
              ),
              div(class = "kpi-box",
                  div(class = "kpi-title", "Unique URLs"),
                  div(class = "kpi-value", textOutput("kpi_urls", inline = TRUE)),
                  div(class = "kpi-sub", textOutput("kpi_dups", inline = TRUE))
              ),
              div(class = "kpi-box",
                  div(class = "kpi-title", "Recommended"),
                  div(class = "kpi-value", textOutput("kpi_reco", inline = TRUE)),
                  div(class = "kpi-sub", "Counts where was_recommended is TRUE")
              ),
              div(class = "kpi-box",
                  div(class = "kpi-title", "Cases of interest"),
                  div(class = "kpi-value", textOutput("kpi_coi", inline = TRUE)),
                  div(class = "kpi-sub", "Counts where is_case_of_interest is TRUE")
              )
          ),
          
          fluidRow(
            column(
              6,
              h4("Daily activity calendar"),
              uiOutput("calendar_ui")
            ),
            column(
              6,
              h4("Weekly breakdown (newest first, excluding Management)"),
              DTOutput("weekly_tbl")
            )
          ),
          br(),
          h4("Last week vs hub average (excluding Management)"),
          DTOutput("under_tbl")
        ),
        
        tabPanel(
          "Quality",
          h4("Field completeness (interactive)"),
          plotlyOutput("comp_heat", height = "520px"),
          br(),
          DTOutput("comp_tbl"),
          br(),
          h4("Logic and consistency checks"),
          div(class = "small-note",
              "These checks catch mismatches (flag TRUE but value missing), duplicates, and date problems."
          ),
          DTOutput("issues_tbl"),
          br(),
          h4("Most common issue types"),
          DTOutput("issue_counts_tbl"),
          br(),
          h4("Timeliness (record_date vs item_date_posted)"),
          plotOutput("delay_plot", height = "360px")
        ),
        
        tabPanel(
          "Newsletter Content",
          h4("Generate newsletter-ready outputs from the currently filtered data"),
          fluidRow(
            column(
              7,
              textInput("newsletter_dir", "Save directory", value = getwd()),
              textInput("newsletter_prefix", "Filename prefix", value = paste0("gms_newsletter_", format(Sys.Date(), "%Y%m%d"))),
              div(class = "btn-row",
                  actionButton("btn_sunburst", "Sunburst (plot + save HTML/PNG)"),
                  actionButton("btn_scoreboard", "Print scoreboard"),
                  actionButton("btn_kpi", "KPI paragraph")
              ),
              div(class = "small-note", verbatimTextOutput("newsletter_status"))
            ),
            column(
              5,
              div(class = "kpi-box",
                  div(class = "kpi-title", "Notes"),
                  div(class = "small-note",
                      HTML(paste0(
                        "<ul style='margin:0; padding-left:18px;'>",
                        "<li>Sunburst uses plot_ecosolve_sunburst().</li>",
                        "<li>Scoreboard uses print_gms_scoreboard().</li>",
                        "<li>KPI paragraph uses gms_kpi_paragraph().</li>",
                        "<li>If PNG saving fails, install webshot2.</li>",
                        "</ul>"
                      ))
                  )
              )
            )
          ),
          hr(class = "soft"),
          h4("Sunburst preview"),
          uiOutput("sunburst_ui"),
          br(),
          h4("Scoreboard output"),
          verbatimTextOutput("scoreboard_out"),
          br(),
          h4("KPI paragraph output"),
          verbatimTextOutput("kpi_out")
        ),
        
        tabPanel(
          "Explorer",
          h4("Filtered data explorer"),
          div(class = "small-note", "Tip: use the search box and column sorting, then export with 'Filtered data (CSV)'."),
          DTOutput("raw_tbl")
        )
      )
    )
  )
)

# -----------------------------
# Server
# -----------------------------
server <- function(input, output, session) {
  
  rv <- reactiveValues(
    dat = NULL,
    loaded_at = NULL,
    sunburst_widget = NULL,
    newsletter_status = "",
    scoreboard_text = "",
    kpi_text = ""
  )
  
  update_status <- reactive({
    if (is.null(rv$dat)) {
      return("No data loaded yet.")
    }
    paste0(
      "Loaded rows: ", format(nrow(rv$dat), big.mark = ","),
      " | Hubs: ", length(unique(rv$dat$datahub)),
      " | Latest record_date: ", {
        if ("record_date" %in% names(rv$dat) && any(!is.na(rv$dat$record_date))) as.character(max(rv$dat$record_date, na.rm = TRUE)) else "NA"
      },
      " | Loaded at: ", format(rv$loaded_at, "%Y-%m-%d %H:%M:%S")
    )
  })
  
  output$status_ui <- renderUI({ span(update_status()) })
  
  observeEvent(input$clear_data, {
    rv$dat <- NULL
    rv$loaded_at <- NULL
    rv$sunburst_widget <- NULL
    rv$newsletter_status <- ""
    rv$scoreboard_text <- ""
    rv$kpi_text <- ""
  })
  
  observeEvent(input$files, {
    req(input$files)
    paths <- input$files$datapath
    if (length(paths) == 0) return()
    
    lst <- map(paths, ~ suppressWarnings(readr::read_csv(.x, show_col_types = FALSE, progress = FALSE)))
    dat <- bind_rows(lst)
    
    dat <- maybe_call_clean_gms_data(dat)
    dat <- clean_admin_data(dat)
    
    rv$dat <- dat
    rv$loaded_at <- Sys.time()
  })
  
  output$hub_ui <- renderUI({
    dat <- rv$dat
    if (is.null(dat)) {
      selectInput("hub_sel", "Data hub", choices = c("All"), selected = "All")
    } else {
      hubs <- sort(unique(dat$datahub))
      if (!isTRUE(input$include_unknown)) hubs <- setdiff(hubs, "Unknown")
      hubs <- setdiff(hubs, "Management")
      choices <- c("All", hubs)
      selectInput("hub_sel", "Data hub", choices = choices, selected = "All")
    }
  })
  
  output$date_ui <- renderUI({
    dat <- rv$dat
    if (is.null(dat) || !("record_date" %in% names(dat)) || all(is.na(dat$record_date))) {
      dateRangeInput("date_rng", "record_date filter", start = Sys.Date() - 30, end = Sys.Date())
    } else {
      mn <- min(dat$record_date, na.rm = TRUE)
      mx <- max(dat$record_date, na.rm = TRUE)
      dateRangeInput("date_rng", "record_date filter", start = mn, end = mx, min = mn, max = mx)
    }
  })
  
  filtered <- reactive({
    dat <- rv$dat
    req(dat)
    
    dat2 <- dat %>% mutate(datahub = ifelse(is_blank(datahub), "Unknown", datahub))
    
    if (!isTRUE(input$include_unknown)) dat2 <- dat2 %>% filter(datahub != "Unknown")
    
    if (!is.null(input$hub_sel) && input$hub_sel != "All") {
      dat2 <- dat2 %>% filter(datahub == input$hub_sel)
    } else {
      dat2 <- dat2 %>% filter(datahub != "Management")
    }
    
    if ("record_date" %in% names(dat2) && !is.null(input$date_rng) && length(input$date_rng) == 2) {
      dr <- input$date_rng
      dat2 <- dat2 %>% filter(is.na(record_date) | (record_date >= dr[1] & record_date <= dr[2]))
    }
    
    dat2
  })
  
  output$kpi_records <- renderText({ format(nrow(filtered()), big.mark = ",") })
  
  output$kpi_datespan <- renderText({
    dat <- filtered()
    if (!("record_date" %in% names(dat)) || all(is.na(dat$record_date))) return("record_date not available")
    paste0(as.character(min(dat$record_date, na.rm = TRUE)), " to ", as.character(max(dat$record_date, na.rm = TRUE)))
  })
  
  output$kpi_urls <- renderText({
    dat <- filtered()
    if (!("url" %in% names(dat))) return("NA")
    format(n_distinct(dat$url[!is.na(dat$url)]), big.mark = ",")
  })
  
  output$kpi_dups <- renderText({
    dat <- filtered()
    if (!("url" %in% names(dat))) return("Dup URLs: NA")
    paste0("Dup URLs: ", format(sum(!is.na(dat$url) & duplicated(dat$url)), big.mark = ","))
  })
  
  output$kpi_reco <- renderText({
    dat <- filtered()
    if (!("was_recommended" %in% names(dat))) return("NA")
    format(sum(dat$was_recommended %in% TRUE, na.rm = TRUE), big.mark = ",")
  })
  
  output$kpi_coi <- renderText({
    dat <- filtered()
    if (!("is_case_of_interest" %in% names(dat))) return("NA")
    format(sum(dat$is_case_of_interest %in% TRUE, na.rm = TRUE), big.mark = ",")
  })
  
  
  # ---- Calendar (stacked: one widget per year) ----
  output$calendar_ui <- renderUI({
    if (is.null(rv$dat)) {
      return(div(class = "small-note", "Upload data to view the calendar."))
    }
    
    dat <- filtered()
    if (!("record_date" %in% names(dat))) {
      return(div(class = "small-note", "record_date missing"))
    }
    
    dates <- dat %>% filter(!is.na(record_date)) %>% distinct(record_date)
    if (nrow(dates) == 0) {
      return(div(class = "small-note", "No dated records under current filters"))
    }
    
    yrs <- seq(year(min(dates$record_date)), year(max(dates$record_date)))
    
    tagList(
      lapply(yrs, function(yr) {
        echarts4rOutput(outputId = paste0("calendar_plot_", yr), height = "240px")
      })
    )
  })
  
  observe({
    req(rv$dat)
    
    dat <- filtered()
    req("record_date" %in% names(dat))
    
    dd <- dat %>%
      filter(!is.na(record_date)) %>%
      count(record_date, name = "n") %>%
      arrange(record_date)
    
    validate(need(nrow(dd) > 0, "No records with record_date under current filters."))
    
    yrs <- seq(year(min(dd$record_date)), year(max(dd$record_date)))
    minv <- min(dd$n, na.rm = TRUE)
    maxv <- max(dd$n, na.rm = TRUE)
    
    for (yr in yrs) {
      local({
        y <- yr
        out_id <- paste0("calendar_plot_", y)
        
        output[[out_id]] <- renderEcharts4r({
          dfy <- dd %>% filter(year(record_date) == y)
          validate(need(nrow(dfy) > 0, ""))
          
          dfy %>%
            e_charts(record_date) %>%
            e_calendar(
              range = as.character(y),
              left = "center",
              top = 25,
              cellSize = c(16, 16),
              itemStyle = list(
                color = "rgba(0,0,0,0)",         # transparent so it blends with dark app bg
                borderWidth = 1,
                borderColor = "rgba(255,255,255,0.10)"
              ),
              yearLabel = list(
                show = TRUE,
                margin = 30,
                textStyle = list(color = "rgba(255,255,255,0.90)")
              ),
              monthLabel = list(textStyle = list(color = "rgba(255,255,255,0.80)")),
              dayLabel   = list(firstDay = 1, textStyle = list(color = "rgba(255,255,255,0.55)"))
            ) %>%
            e_heatmap(n, coord_system = "calendar", name = "Records") %>%
            e_visual_map(
              n,
              min = minv,
              max = maxv,
              orient = "horizontal",
              bottom = 0,
              textStyle = list(color = "rgba(255,255,255,0.85)")
            ) %>%
            e_tooltip(
              formatter = htmlwidgets::JS(
                "function(params){
                 if(!params || !params.value) return '';
                 return params.value[0] + '<br/>' + params.value[1] + ' records';
               }"
              )
            ) %>%
            e_theme("dark")
        })
      })
    }
  })
  
  weekly_tbl_re <- reactive({
    dat <- filtered()
    req(dat)
    validate(need("record_date" %in% names(dat), "record_date missing"))
    
    wk <- dat %>%
      filter(!is.na(record_date), datahub != "Management") %>%
      mutate(week_start = floor_date(record_date, unit = "week", week_start = 1)) %>%
      count(datahub, week_start, name = "n")
    
    if (nrow(wk) == 0) return(tibble())
    
    all_weeks <- tibble(week_start = seq(min(wk$week_start), max(wk$week_start), by = "1 week"))
    hubs <- tibble(datahub = sort(unique(wk$datahub)))
    
    wk_complete <- hubs %>%
      crossing(all_weeks) %>%
      left_join(wk, by = c("datahub", "week_start")) %>%
      mutate(n = replace_na(n, 0L))
    
    wk_complete %>%
      select(week_start, datahub, n) %>%
      pivot_wider(names_from = datahub, values_from = n, values_fill = 0) %>%
      mutate(Total = rowSums(across(where(is.numeric)))) %>%
      arrange(desc(week_start))
  })
  
  output$weekly_tbl <- renderDT({
    datatable(weekly_tbl_re(), rownames = FALSE, options = list(pageLength = 12, scrollX = TRUE))
  })
  
  output$under_tbl <- renderDT({
    dat <- filtered()
    req(dat)
    validate(need("record_date" %in% names(dat), "record_date missing"))
    
    dd <- dat %>%
      filter(!is.na(record_date), datahub != "Management") %>%
      mutate(week_start = floor_date(record_date, unit = "week", week_start = 1)) %>%
      count(datahub, week_start, name = "n")
    
    if (nrow(dd) == 0) {
      return(datatable(tibble(note = "No weekly data available under current filters."), rownames = FALSE))
    }
    
    last_week <- max(dd$week_start, na.rm = TRUE)
    
    first_weeks <- dd %>%
      filter(n > 0) %>%
      group_by(datahub) %>%
      summarise(first_week = min(week_start), .groups = "drop")
    
    hubs <- sort(unique(dd$datahub))
    
    avg_tbl <- map_dfr(hubs, function(h) {
      fw <- first_weeks %>% filter(datahub == h) %>% pull(first_week)
      if (length(fw) == 0 || is.na(fw)) return(tibble(datahub = h, avg_weekly = NA_real_))
      weeks_seq <- tibble(week_start = seq(fw, last_week, by = "1 week"))
      n_tbl <- weeks_seq %>%
        left_join(dd %>% filter(datahub == h) %>% select(week_start, n), by = "week_start") %>%
        mutate(n = replace_na(n, 0L))
      tibble(datahub = h, avg_weekly = mean(n_tbl$n))
    })
    
    last_tbl <- tibble(datahub = hubs) %>%
      left_join(dd %>% filter(week_start == last_week) %>% select(datahub, records_last_week = n), by = "datahub") %>%
      mutate(records_last_week = replace_na(records_last_week, 0L)) %>%
      left_join(avg_tbl, by = "datahub") %>%
      mutate(
        status = case_when(
          records_last_week == 0 ~ "No Records",
          is.na(avg_weekly) ~ "No Records",
          records_last_week > avg_weekly ~ "Above Average",
          records_last_week < avg_weekly ~ "Below Average",
          TRUE ~ "OK"
        ),
        avg_weekly = round(avg_weekly, 2),
        last_week = as.character(last_week)
      ) %>%
      arrange(match(status, c("Below Average", "No Records", "OK", "Above Average")), desc(records_last_week), datahub) %>%
      select(datahub, last_week, records_last_week, avg_weekly, status)
    
    datatable(last_tbl, rownames = FALSE, options = list(pageLength = 25, scrollX = TRUE), class = "compact stripe")
  })
  
  selected_quality_fields <- reactive({
    dat <- filtered()
    req(dat)
    groups <- input$quality_groups
    if (is.null(groups) || length(groups) == 0) return(character())
    unique(unlist(map(groups, ~ available_fields_for_group(dat, .x))))
  })
  
  comp_tbl_re <- reactive({
    dat <- filtered()
    req(dat)
    completeness_table(dat, selected_quality_fields())
  })
  
  output$comp_tbl <- renderDT({
    tbl <- comp_tbl_re() %>% mutate(pct_filled = round(100 * pct_filled, 1))
    datatable(tbl, rownames = FALSE, options = list(pageLength = 25, scrollX = TRUE))
  })
  
  output$comp_heat <- renderPlotly({
    dat <- filtered()
    req(dat)
    
    fields <- selected_quality_fields()
    if (length(fields) == 0) {
      p0 <- ggplot() + labs(title = "No fields selected for completeness scoring")
      g0 <- ggplotly(p0)
      return(g0 %>% layout(
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor = "rgba(0,0,0,0)",
        font = list(color = "white")
      ))
    }
    
    byhub <- completeness_by_hub(dat, fields)
    if (nrow(byhub) == 0) {
      p0 <- ggplot() + labs(title = "No data after filtering")
      g0 <- ggplotly(p0)
      return(g0 %>% layout(
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor = "rgba(0,0,0,0)",
        font = list(color = "white")
      ))
    }
    
    long <- byhub %>%
      select(-n) %>%
      pivot_longer(-datahub, names_to = "field", values_to = "pct") %>%
      mutate(
        field = factor(field, levels = fields),
        datahub = factor(datahub, levels = sort(unique(datahub)))
      )
    
    p <- ggplot(
      long,
      aes(
        x = field, y = datahub, fill = pct,
        text = paste0("Hub: ", datahub, "<br>Field: ", field, "<br>Filled: ", round(100 * pct, 1), "%")
      )
    ) +
      geom_tile(color = NA) +
      labs(
        title = "Completeness by hub (share filled per field)",
        x = "Field",
        y = "Data hub",
        fill = "Filled"
      ) +
      theme_minimal(base_size = 12) +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1, colour = "white"),
        axis.text.y = element_text(colour = "white"),
        axis.title.x = element_text(colour = "white"),
        axis.title.y = element_text(colour = "white"),
        plot.title = element_text(colour = "white"),
        legend.title = element_text(colour = "white"),
        legend.text = element_text(colour = "white"),
        panel.grid = element_blank(),
        panel.background = element_rect(fill = "transparent", colour = NA),
        plot.background = element_rect(fill = "transparent", colour = NA)
      )
    
    g <- ggplotly(p, tooltip = "text") %>% layout(hoverlabel = list(align = "left"))
    g %>% layout(
      paper_bgcolor = "rgba(0,0,0,0)",
      plot_bgcolor = "rgba(0,0,0,0)",
      font = list(color = "white"),
      xaxis = list(
        tickfont = list(color = "white"),
        titlefont = list(color = "white")
      ),
      yaxis = list(
        tickfont = list(color = "white"),
        titlefont = list(color = "white")
      ),
      legend = list(font = list(color = "white"))
    )
  })
  
  issues_re <- reactive({
    dat <- filtered()
    req(dat)
    quality_issue_table(dat)
  })
  
  output$issues_tbl <- renderDT({
    tbl <- issues_re()
    if (isTRUE(input$show_only_flagged)) tbl <- tbl %>% filter(n_issues > 0)
    datatable(tbl, rownames = FALSE, options = list(pageLength = 25, scrollX = TRUE), filter = "top")
  })
  
  output$issue_counts_tbl <- renderDT({
    datatable(issue_counts(issues_re()), rownames = FALSE, options = list(pageLength = 15))
  })
  
  output$delay_plot <- renderPlot({
    dat <- filtered()
    req(dat)
    if (!all(c("record_date","item_date_posted") %in% names(dat))) {
      return(ggplot() + labs(title = "record_date and/or item_date_posted missing"))
    }
    
    dd <- dat %>%
      mutate(delay_days = as.numeric(record_date - item_date_posted)) %>%
      filter(!is.na(delay_days), is.finite(delay_days))
    
    if (nrow(dd) == 0) return(ggplot() + labs(title = "No delay values available"))
    
    ggplot(dd, aes(x = delay_days)) +
      geom_histogram(bins = 40) +
      labs(
        title = "Entry delay (days): record_date minus item_date_posted",
        x = "Days",
        y = "Records"
      ) +
      theme_minimal(base_size = 12) +
      theme(
        axis.text = element_text(colour = "white"),
        axis.title = element_text(colour = "white"),
        plot.title = element_text(colour = "white"),
        panel.background = element_rect(fill = "transparent", colour = NA),
        plot.background = element_rect(fill = "transparent", colour = NA)
      )
  })
  
  output$raw_tbl <- renderDT({
    dat <- filtered()
    req(dat)
    datatable(dat, rownames = FALSE, options = list(pageLength = 25, scrollX = TRUE), filter = "top")
  })
  
  # Newsletter Content
  output$newsletter_status <- renderText({ rv$newsletter_status })
  
  output$sunburst_ui <- renderUI({
    req(rv$sunburst_widget)
    rv$sunburst_widget
  })
  
  output$scoreboard_out <- renderText({ rv$scoreboard_text })
  output$kpi_out <- renderText({ rv$kpi_text })
  
  observeEvent(input$btn_sunburst, {
    dat <- filtered()
    req(dat)
    
    f <- get_fun("plot_ecosolve_sunburst")
    if (is.null(f)) {
      rv$newsletter_status <- "plot_ecosolve_sunburst() not found. Install/load gmstools."
      return()
    }
    
    out_dir <- input$newsletter_dir
    if (is_blank(out_dir)) out_dir <- getwd()
    if (!dir.exists(out_dir)) {
      ok <- tryCatch({ dir.create(out_dir, recursive = TRUE); TRUE }, error = function(e) FALSE)
      if (!ok) {
        rv$newsletter_status <- paste0("Could not create directory: ", out_dir)
        return()
      }
    }
    
    prefix <- input$newsletter_prefix
    if (is_blank(prefix)) prefix <- paste0("gms_newsletter_", format(Sys.time(), "%Y%m%d_%H%M%S"))
    
    res <- tryCatch(f(dat), error = function(e) e)
    if (inherits(res, "error")) {
      rv$newsletter_status <- paste0("Sunburst failed: ", conditionMessage(res))
      return()
    }
    
    rv$sunburst_widget <- res
    
    html_path <- file.path(out_dir, paste0(prefix, "_sunburst.html"))
    png_path  <- file.path(out_dir, paste0(prefix, "_sunburst.png"))
    
    ok_html <- tryCatch({
      htmlwidgets::saveWidget(res, html_path, selfcontained = TRUE)
      TRUE
    }, error = function(e) FALSE)
    
    ok_png <- FALSE
    if (ok_html && requireNamespace("webshot2", quietly = TRUE)) {
      ok_png <- tryCatch({
        webshot2::webshot(html_path, png_path, vwidth = 1400, vheight = 900)
        TRUE
      }, error = function(e) FALSE)
    }
    
    rv$newsletter_status <- paste0(
      "Saved HTML: ", html_path,
      if (ok_png) paste0("\nSaved PNG:  ", png_path) else "\nPNG not saved (install webshot2 or fix headless browser setup)."
    )
  })
  
  observeEvent(input$btn_scoreboard, {
    dat <- filtered()
    req(dat)
    
    f <- get_fun("print_gms_scoreboard")
    if (is.null(f)) {
      rv$scoreboard_text <- "print_gms_scoreboard() not found. Install/load gmstools."
      return()
    }
    
    txt <- tryCatch({
      capture.output(f(dat)) %>% paste(collapse = "\n")
    }, error = function(e) paste0("Scoreboard failed: ", conditionMessage(e)))
    
    rv$scoreboard_text <- txt
  })
  
  observeEvent(input$btn_kpi, {
    dat <- filtered()
    req(dat)
    
    f <- get_fun("gms_kpi_paragraph")
    if (is.null(f)) {
      rv$kpi_text <- "gms_kpi_paragraph() not found. Install/load gmstools."
      return()
    }
    
    txt <- tryCatch({
      res <- f(dat)
      if (is.character(res) && length(res) == 1) res else capture.output(res) %>% paste(collapse = "\n")
    }, error = function(e) paste0("KPI paragraph failed: ", conditionMessage(e)))
    
    rv$kpi_text <- txt
  })
  
  # Downloads
  output$dl_weekly <- downloadHandler(
    filename = function() paste0("weekly_breakdown_", format(Sys.Date(), "%Y%m%d"), ".csv"),
    content = function(file) readr::write_csv(weekly_tbl_re(), file)
  )
  
  output$dl_completeness <- downloadHandler(
    filename = function() paste0("completeness_", format(Sys.Date(), "%Y%m%d"), ".csv"),
    content = function(file) readr::write_csv(comp_tbl_re(), file)
  )
  
  output$dl_issues <- downloadHandler(
    filename = function() paste0("issues_", format(Sys.Date(), "%Y%m%d"), ".csv"),
    content = function(file) {
      tbl <- issues_re()
      if (isTRUE(input$show_only_flagged)) tbl <- tbl %>% filter(n_issues > 0)
      readr::write_csv(tbl, file)
    }
  )
  
  output$dl_filtered <- downloadHandler(
    filename = function() paste0("filtered_data_", format(Sys.Date(), "%Y%m%d"), ".csv"),
    content = function(file) readr::write_csv(filtered(), file)
  )
}

shinyApp(ui, server)
}