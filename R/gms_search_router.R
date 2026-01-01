gms_search_router <- function(){
  # ---- Packages: install if missing, then load quietly ----
  required_pkgs <- c(
    "shiny","DT","dplyr","purrr","tibble","stringr","glue",
    "curl","readr","tidyr","shinyWidgets"
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
  
  `%||%` <- function(x, y) if (is.null(x)) y else x
  
  
  # ---------- State save / load helpers ----------
  gms_router_state_capture <- function(input) {
    list(
      browser       = input$browser,
      login_plats   = input$login_plats,
      dork_sites    = input$dork_sites,
      dork_extra    = input$dork_extra,
      geo_terms     = input$geo_terms,
      species_terms = input$species_terms,
      sales_terms   = input$sales_terms,
      terms         = input$terms,
      cat           = input$cat,
      plats         = input$plats
    )
  }
  
  gms_router_state_apply <- function(session, state) {
    if (!is.null(state$browser)) {
      updateRadioButtons(session, "browser", selected = state$browser)
    }
    if (!is.null(state$login_plats)) {
      shinyWidgets::updatePickerInput(session, "login_plats", selected = state$login_plats)
    }
    if (!is.null(state$dork_sites)) {
      updatePickerInput(session, "dork_sites", selected = state$dork_sites)
    }
    if (!is.null(state$dork_extra)) {
      updateTextInput(session, "dork_extra", value = state$dork_extra)
    }
    if (!is.null(state$geo_terms)) {
      updateTextAreaInput(session, "geo_terms", value = state$geo_terms)
    }
    if (!is.null(state$species_terms)) {
      updateTextAreaInput(session, "species_terms", value = state$species_terms)
    }
    if (!is.null(state$sales_terms)) {
      updateTextAreaInput(session, "sales_terms", value = state$sales_terms)
    }
    if (!is.null(state$terms)) {
      updateTextAreaInput(session, "terms", value = state$terms)
    }
    if (!is.null(state$cat)) {
      updateCheckboxGroupInput(session, "cat", selected = state$cat)
    }
    if (!is.null(state$plats)) {
      shinyWidgets::updatePickerInput(session, "plats", selected = state$plats)
    }
  }
  
  # ---------- Platforms (non-Google links stay as before) ----------
  platforms <- tribble(
    ~name,                        ~category,   ~template,
    # ---------------- Social / Forums ----------------
    "YouTube",                    "Social",    "https://www.youtube.com/results?search_query=%s",
    "X (Twitter)",                "Social",    "https://x.com/search?q=%s&src=typed_query&f=top",
    "Reddit",                     "Social",    "https://www.reddit.com/search/?q=%s",
    "TikTok",                     "Social",    "https://www.tiktok.com/search?q=%s",
    "Douyin",                     "Social",    "https://www.douyin.com/search/%s",
    "Facebook (General)",         "Social",    "https://www.facebook.com/search/top/?q=",
    "Instagram (keyword)",        "Social",    "https://www.instagram.com/explore/search/keyword/?q=%s",
    "Telegram (channel/user search)","Social","https://t.me/s/%s",
    "VK",                         "Social",    "https://vk.com/search?c[q]=%s",
    # region-specific forum
    "Nairaland (forum)",          "Social",    "https://www.nairaland.com/search?q=%s&board=0",
    
    # ---------------- E-commerce / Classifieds ----------------
    # Global / generic
    "Amazon",                     "E-commerce","https://www.amazon.com/s?k=%s",
    "eBay",                       "E-commerce","https://www.ebay.com/sch/i.html?_nkw=%s",
    "Etsy",                       "E-commerce","https://www.etsy.com/search?q=%s",
    "Alibaba",                    "E-commerce","https://www.alibaba.com/trade/search?SearchText=%s",
    "AliExpress",                 "E-commerce","https://www.aliexpress.com/wholesale?SearchText=%s",
    "Shopee (global)",            "E-commerce","https://shopee.com/search?keyword=%s",
    "Tokopedia",                  "E-commerce","https://www.tokopedia.com/search?st=product&q=%s",
    "Lazada",                     "E-commerce","https://www.lazada.com.ph/catalog/?q=%s",
    
    # Latin America
    "MercadoLibre MX",            "E-commerce","https://listado.mercadolibre.com.mx/%s",
    "MercadoLibre BR",            "E-commerce","https://lista.mercadolivre.com.br/%s",
    "MercadoLibre CO",            "E-commerce","https://listado.mercadolibre.com.co/%s",
    "OLX Brazil",                 "E-commerce","https://www.olx.com.br/brasil?q=%s",
    "Enjoei (Brazil)",            "E-commerce","https://www.enjoei.com.br/pesquisar/s?q=%s",
    
    # Indonesia
    "Bukalapak",                  "E-commerce","https://www.bukalapak.com/products?search%%5Bkeyword%%5D=%s",
    "Jualo",                      "E-commerce","https://www.jualo.com/semua/q-%s/?search=true",
    
    # India
    "OLX India",                  "E-commerce","https://www.olx.in/en-in/items/q-%s",
    "Flipkart",                   "E-commerce","https://www.flipkart.com/search?q=%s",
    "Snapdeal",                   "E-commerce","https://www.snapdeal.com/search?keyword=%s",
    "Quikr",                      "E-commerce","https://www.quikr.com/all-results/%s",
    "IndiaMART",                  "E-commerce","https://dir.indiamart.com/search.mp?ss=%s",
    
    # Africa (Nigeria, South Africa, MENA, Cameroon region)
    "Jiji Nigeria",               "E-commerce","https://jiji.ng/search?query=%s",
    "Jumia Nigeria",              "E-commerce","https://www.jumia.com.ng/catalog/?q=%s",
    "Gumtree South Africa",       "E-commerce","https://www.gumtree.co.za/s-%s/v1q0p1",
    "Afribaba (Morocco/North Africa)","E-commerce","https://ma.afribaba.com/search.html?q=%s",
    
    # Czech Republic / Central Europe
    "Bazos.cz",                   "E-commerce","https://www.bazos.cz/search.php?hledat=%s",
    "Sbazar.cz",                  "E-commerce","https://www.sbazar.cz/hledej/%s",
    "HyperInzerce.cz",            "E-commerce","https://www.hyperinzerce.cz/inzeraty/Index?query=%s",
    
    # Existing B2B
    "Made-in-China",              "B2B",       "https://www.made-in-china.com/products-search/hot-china-products/%s.html",
    "GlobalSources",              "B2B",       "https://www.globalsources.com/SearchResult?query=%s",
    "HKTDC",                      "B2B",       "https://sourcing.hktdc.com/en/Search-Keyword/%s",
    "TradeKey",                   "B2B",       "https://www.tradekey.com/sd/%s.html",
    "EC21",                       "B2B",       "https://www.ec21.com/ks-%s.html",
    "21Food",                     "B2B",       "https://www.21food.com/show/%s.html",
    "Tradeford",                  "B2B",       "https://www.tradeford.com/products/",
    "ExportersIndia (Cameroon)",  "B2B",       "https://www.exportersindia.com/cm/search.php?srch_catg_ty=prod&ss_status=N&term=",
    "ExportersIndia (General)",   "B2B",       "https://www.exportersindia.com/cm/search.php?srch_catg_ty=prod&ss_status=N&term="
  ) %>%
    arrange(category, name)
  
  # ---------- Sign-in home pages ----------
  login_home <- c(
    # social
    "YouTube"                         = "https://www.youtube.com/",
    "X (Twitter)"                     = "https://x.com/",
    "Reddit"                          = "https://www.reddit.com/",
    "TikTok"                          = "https://www.tiktok.com/",
    "Douyin"                          = "https://www.douyin.com/",
    "Facebook (General)"              = "https://www.facebook.com/",
    "Instagram (keyword)"             = "https://www.instagram.com/",
    "Telegram (channel/user search)"  = "https://web.telegram.org/",
    "VK"                              = "https://vk.com/",
    "Nairaland (forum)"               = "https://www.nairaland.com/",
    
    # e-commerce / classifieds
    "Amazon"                          = "https://www.amazon.com/",
    "eBay"                            = "https://www.ebay.com/",
    "Etsy"                            = "https://www.etsy.com/",
    "Alibaba"                         = "https://www.alibaba.com/",
    "AliExpress"                      = "https://www.aliexpress.com/",
    "Shopee (global)"                 = "https://shopee.com/",
    "Tokopedia"                       = "https://www.tokopedia.com/",
    "Lazada"                          = "https://www.lazada.com/",
    
    "MercadoLibre MX"                 = "https://www.mercadolibre.com.mx/",
    "MercadoLibre BR"                 = "https://www.mercadolivre.com.br/",
    "MercadoLibre CO"                 = "https://www.mercadolibre.com.co/",
    "OLX Brazil"                      = "https://www.olx.com.br/",
    "Enjoei (Brazil)"                 = "https://www.enjoei.com.br/",
    
    "Bukalapak"                       = "https://www.bukalapak.com/",
    "Jualo"                           = "https://www.jualo.com/",
    
    "OLX India"                       = "https://www.olx.in/",
    "Flipkart"                        = "https://www.flipkart.com/",
    "Snapdeal"                        = "https://www.snapdeal.com/",
    "Quikr"                           = "https://www.quikr.com/",
    "IndiaMART"                       = "https://www.indiamart.com/",
    
    "Jiji Nigeria"                    = "https://jiji.ng/",
    "Jumia Nigeria"                   = "https://www.jumia.com.ng/",
    "Gumtree South Africa"            = "https://www.gumtree.co.za/",
    "Afribaba (Morocco/North Africa)" = "https://ma.afribaba.com/",
    
    "Bazos.cz"                        = "https://www.bazos.cz/",
    "Sbazar.cz"                       = "https://www.sbazar.cz/",
    "HyperInzerce.cz"                 = "https://www.hyperinzerce.cz/",
    
    # B2B
    "Made-in-China"                   = "https://www.made-in-china.com/",
    "GlobalSources"                   = "https://www.globalsources.com/",
    "HKTDC"                           = "https://sourcing.hktdc.com/",
    "TradeKey"                        = "https://www.tradekey.com/",
    "EC21"                            = "https://www.ec21.com/",
    "21Food"                          = "https://www.21food.com/",
    "Tradeford"                       = "https://www.tradeford.com/",
    "ExportersIndia (Cameroon)"       = "https://www.exportersindia.com/",
    "ExportersIndia (General)"        = "https://www.exportersindia.com/"
  )
  
  # ---------- Google dorking sites ----------
  dork_sites <- tribble(
    ~label,                   ~site_expr,
    "Facebook Groups",        "facebook.com/groups",
    "Facebook (all)",         "facebook.com",
    "Instagram",              "instagram.com",
    "TikTok",                 "tiktok.com",
    "X (Twitter)",            "x.com",
    "Reddit",                 "reddit.com",
    "YouTube",                "youtube.com",
    "Telegram",               "t.me",
    "VK",                     "vk.com",
    "Douyin",                 "douyin.com",
    # additional trade-relevant sites
    "MercadoLibre (all)",     "mercadolibre.com",
    "OLX (all)",              "olx.",
    "Jiji (Nigeria)",         "jiji.ng",
    "Jumia (Nigeria)",        "jumia.com.ng",
    "Gumtree (South Africa)", "gumtree.co.za",
    "Nairaland (forum)",      "nairaland.com",
    "Tokopedia",              "tokopedia.com",
    "Shopee",                 "shopee.com",
    "Bukalapak",              "bukalapak.com",
    "Jualo",                  "jualo.com",
    "Enjoei (Brazil)",        "enjoei.com.br",
    "Afribaba",               "afribaba.com",
    "Bazos.cz",               "bazos.cz",
    "Sbazar.cz",              "sbazar.cz"
  )
  # ---------- Helpers ----------
  enc <- function(q) curl_escape(q)
  escape_percents_for_sprintf <- function(tpl) gsub("%(?!s)", "%%", tpl, perl = TRUE)
  html_attr_escape <- function(x){
    x <- gsub("&","&amp;",x,fixed=TRUE); x <- gsub("\"","&quot;",x,fixed=TRUE)
    x <- gsub("'","&#39;",x,fixed=TRUE); x <- gsub("<","&lt;",x,fixed=TRUE); gsub(">","&gt;",x,fixed=TRUE)
  }
  
  sys_os <- function(){
    os <- Sys.info()[["sysname"]]
    if (is.null(os)) ifelse(.Platform$OS.type=="windows","Windows","Unix") else os
  }
  qarg <- function(x){ if (sys_os()=="Windows") shQuote(x, type="cmd") else shQuote(x, type="sh") }
  
  find_exe <- function(browser){
    os <- sys_os()
    if (browser=="chrome"){
      if (os=="Windows"){
        cands <- c(Sys.which("chrome.exe"),
                   "C:/Program Files/Google/Chrome/Application/chrome.exe",
                   "C:/Program Files (x86)/Google/Chrome/Application/chrome.exe")
        exe <- cands[nzchar(cands) & file.exists(cands)][1]; if (!is.na(exe)) return(exe)
      } else if (os=="Darwin"){ return("macOS_app") }
      else {
        exe <- Sys.which(c("google-chrome","google-chrome-stable","chromium","chromium-browser"))
        exe <- exe[nzchar(exe)][1]; if (!is.na(exe)) return(exe)
      }
    } else if (browser=="firefox"){
      if (os=="Windows"){
        cands <- c(Sys.which("firefox.exe"),
                   "C:/Program Files/Mozilla Firefox/firefox.exe",
                   "C:/Program Files (x86)/Mozilla Firefox/firefox.exe")
        exe <- cands[nzchar(cands) & file.exists(cands)][1]; if (!is.na(exe)) return(exe)
      } else if (os=="Darwin"){ return("macOS_app") }
      else { exe <- Sys.which("firefox"); if (nzchar(exe)) return(exe) }
    }
    NA_character_
  }
  
  open_in_browser <- function(urls, browser=c("current","chrome","firefox"), session=NULL){
    browser <- match.arg(browser); urls <- as.character(urls); os <- sys_os()
    if (browser=="current"){
      session$sendCustomMessage("openTabs", unname(urls))
      return(invisible(TRUE))
    }
    exe <- if (browser=="chrome") find_exe("chrome") else find_exe("firefox")
    if (is.na(exe)){
      showNotification(sprintf("Could not find %s.", ifelse(browser=="chrome","Chrome","Firefox")),
                       type="error", duration=6); return(invisible(FALSE))
    }
    if (os=="Darwin" && identical(exe,"macOS_app")){
      app <- if (browser=="chrome") "Google Chrome" else "Firefox"
      for (u in urls){ system2("open", c("-a", app, u), wait=FALSE, stdout=FALSE, stderr=FALSE) }
    } else {
      for (u in urls){
        args <- if (browser=="firefox") c("-new-tab", u) else u
        system2(exe, args = if (os=="Windows") qarg(args) else args,
                wait=FALSE, stdout=FALSE, stderr=FALSE)
      }
    }
    invisible(TRUE)
  }
  
  login_urls_for <- function(selected_platforms){
    keep <- intersect(selected_platforms, names(login_home))
    unique(unname(login_home[keep]))
  }
  
  grouped_choices <- function(cats){
    allowed_df <- platforms %>% filter(category %in% cats)
    split(allowed_df$name, allowed_df$category)
  }
  
  # ---------- Build regular (non-Google) links ----------
  build_links_common <- function(terms, selected_platforms){
    sel <- platforms %>% filter(name %in% selected_platforms)
    if (!length(terms) || !nrow(sel)) return(tibble())
    tidyr::crossing(term = terms, sel) %>%
      mutate(
        enc_term = dplyr::case_when(
          name == "EC21" ~ curl_escape(gsub("\\s+", "-", term)),
          TRUE           ~ enc(term)
        ),
        tpl  = ifelse(grepl("%s", template, fixed = TRUE), escape_percents_for_sprintf(template), template),
        url  = ifelse(grepl("%s", template, fixed = TRUE), sprintf(tpl, enc_term), paste0(tpl, enc_term))
      ) %>%
      transmute(Term = term, Platform = name, Category = category, URL = url)
  }
  
  # ---------- Google dorking ----------
  parse_terms <- function(x){
    x <- x %||% ""
    parts <- str_split(x, "[,\\n]")[[1]] %>% str_trim()
    unique(parts[parts != ""])
  }
  
  # Given vectors: sites (site_expr), geos, species, sales -> build Google search URLs
  build_dork_links <- function(sites, geos, species, sales){
    if (!length(sites)) return(tibble())
    geos    <- geos    %||% character(0)
    species <- species %||% character(0)
    sales   <- sales   %||% character(0)
    
    norm <- function(v) if (!length(v)) "" else v
    
    df <- tidyr::crossing(
      site   = sites,
      geo    = norm(geos),
      specie = norm(species),
      sale   = norm(sales)
    ) %>%
      mutate(
        query_text = str_squish(paste(
          paste0("site:", site),
          geo,
          specie,
          sale
        )),
        URL = sprintf("https://www.google.com/search?q=%s", enc(query_text)),
        Platform = paste0("Google dork (", site, ")"),
        Category = "Google"
      ) %>%
      transmute(Term = query_text, Platform, Category, URL)
    
    distinct(df, Term, Platform, Category, URL)
  }
  
  # ---------- UI ----------
  ui <- fluidPage(
    tags$head(
      tags$style(HTML("
      .control-panel {background:#0f172a; color:#e5e7eb; padding:16px; border-radius:12px; margin-bottom:12px;}
      .control-panel h3 {margin-top:0;}
      .btn-openall {margin-left:8px;}
      .note {font-size:12px; color:#64748b;}
    ")),
      tags$script("
      // Track selected browser on the client so we can open immediately for 'current'
      window.GMS_BROWSER = 'current';
      $(document).on('shiny:inputchanged', function(e){
        if (e.name === 'browser') { window.GMS_BROWSER = e.value; }
      });

      // Per-row Open:
      // - If 'current' -> let <a> open in new tab normally.
      // - Else -> prevent default and ask server to open via external browser.
      $(document).on('click', '.open-link', function(e){
        var url = $(this).attr('href') || $(this).data('url');
        if (window.GMS_BROWSER === 'current') {
          // allow default: browser opens target=_blank
          return true;
        } else {
          e.preventDefault();
          Shiny.setInputValue('open_url_external', { url: url, ts: Date.now() }, {priority:'event'});
        }
      });

      // Bulk open in current browser (server passes array -> this opens them)
      Shiny.addCustomMessageHandler('openTabs', function(urls){
        for (let i=0; i<urls.length; i++) { window.open(urls[i], '_blank'); }
      });
    ")
    ),
    titlePanel("GMS Search Router — multi-site native search launcher"),
    
    # STEP 0 — Browser
    div(class="control-panel",
        h3("Step 0) Choose browser"),
        radioButtons("browser","Open links in:",
                     choices=c("Current browser"="current","Google Chrome"="chrome","Mozilla Firefox"="firefox"),
                     selected="current", inline=TRUE),
        actionButton("test_browser","Test browser"),
        verbatimTextOutput("browser_status", placeholder = TRUE)
    ),
    
    # STEP 1 — Sign-in platforms (optional)
    div(class="control-panel",
        h3("Step 1) Sign in (optional)"),
        p("Pick the websites you want to sign in to first (separate from search selection)."),
        pickerInput(
          "login_plats", "Sign-in platforms (grouped)",
          choices = grouped_choices(c("Social","E-commerce","B2B")),
          selected = c("YouTube","X (Twitter)","Reddit","Facebook (General)","Instagram (keyword)"),
          multiple = TRUE,
          options = list(actionsBox = TRUE, liveSearch = TRUE, selectedTextFormat = "count > 3", size = 10)
        ),
        actionButton("login_prep","Open sign-in/home tabs")
    ),
    
    # STEP 2 — Google dorking
    div(class="control-panel",
        h3("Step 2) Google dorking"),
        p("Pick sites and enter comma/newline-separated terms. Each combination becomes a Google search:"),
        tags$code('site:facebook.com/groups Nigeria pangolin sale'),
        br(), br(),
        fluidRow(
          column(6,
                 pickerInput(
                   "dork_sites", "Sites to scan",
                   choices = setNames(dork_sites$site_expr, dork_sites$label),
                   selected = c("facebook.com/groups","instagram.com","tiktok.com","x.com","reddit.com"),
                   multiple = TRUE,
                   options = list(actionsBox = TRUE, liveSearch = TRUE, selectedTextFormat = "count > 3", size = 10)
                 )
          ),
          column(6,
                 textInput("dork_extra", "Extra fixed terms (optional; appended to every query)", value = "")
          )
        ),
        fluidRow(
          column(4, textAreaInput("geo_terms",    "Geographic terms", rows = 3,
                                  placeholder = "e.g., Nigeria, Brazil, Thailand")),
          column(4, textAreaInput("species_terms","Species terms",    rows = 3,
                                  placeholder = "e.g., pangolin, monkey, tiger cub")),
          column(4, textAreaInput("sales_terms",  "Sales terms",      rows = 3,
                                  placeholder = "e.g., available, selling, rehome, price"))
        )
    ),
    
    # STEP 3 — (Optional) Native-site searches
    div(class="control-panel",
        h3("Step 3) (Optional) Native-site search launcher"),
        p("Enter any direct search terms to build platform-native search URLs."),
        textAreaInput("terms", NULL, placeholder="Search terms separated by commas (e.g., pangolin, rhino horn, tiger cub)", rows=4, width="100%"),
        fluidRow(
          column(4, checkboxGroupInput("cat","Filter by category",
                                       choices=c("Social","E-commerce","B2B"),
                                       selected=c("Social","E-commerce","B2B"))),
          column(8,
                 pickerInput(
                   "plats","Platforms (grouped) for searching",
                   choices=grouped_choices(c("Social","E-commerce","B2B")),
                   selected=platforms$name, multiple=TRUE,
                   options=list(actionsBox=TRUE, liveSearch=TRUE, selectedTextFormat="count > 3", size=10)
                 ),
                 actionLink("select_all","Select all (filtered)"),
                 HTML("&nbsp;•&nbsp;"),
                 actionLink("clear_all","Clear")
          )
        ),
        actionButton("go","Generate links", class="btn btn-success"),
        actionButton("open_selected","Open selected rows", class="btn btn-secondary btn-openall"),
        actionButton("open_all","Open all results", class="btn btn-secondary btn-openall"),
        downloadButton("dl_csv","Export links (CSV)"),
        downloadButton("save_state_rds","Save inputs (RDS)"),
        fileInput("load_state_rds", NULL, accept = ".rds", buttonLabel = "Load inputs (RDS)"),
        
        div(class="note", br(),
            HTML("<b>Open</b> uses your chosen browser above. If 'Current', it opens via the link itself; if Chrome/Firefox, the app launches that browser."))
    ),
    
    DTOutput("tbl")
  )
  
  # ---------- Server ----------
  server <- function(input, output, session){
    
    # Sync picker with category filter
    observeEvent(input$cat, {
      allowed_df  <- platforms %>% filter(category %in% input$cat)
      allowed     <- allowed_df$name
      grouped     <- split(allowed_df$name, allowed_df$category)
      new_sel     <- intersect(isolate(input$plats %||% allowed), allowed)
      updatePickerInput(session, "plats", choices = grouped, selected = new_sel)
    }, ignoreInit = TRUE, priority = 1)
    
    observeEvent(input$select_all, {
      allowed <- platforms %>% filter(category %in% input$cat) %>% pull(name)
      updatePickerInput(session, "plats", selected = allowed)
    })
    observeEvent(input$clear_all, { updatePickerInput(session, "plats", selected = character(0)) })
    
    # Parse native terms
    parsed_terms <- reactive({
      txt <- input$terms %||% ""
      parts <- str_split(txt, "[,\\n]")[[1]] %>% str_trim()
      unique(parts[parts != ""])
    })
    
    # Build data
    results <- eventReactive(input$go, {
      # Google dorking
      sites   <- input$dork_sites %||% character(0)
      geos    <- parse_terms(input$geo_terms)
      species <- parse_terms(input$species_terms)
      sales   <- parse_terms(input$sales_terms)
      
      dorks <- build_dork_links(sites, geos, species, sales)
      if (nzchar(input$dork_extra %||% "")){
        dorks <- dorks %>%
          mutate(
            Term = paste(Term, input$dork_extra),
            URL  = sprintf("https://www.google.com/search?q=%s", enc(Term))
          )
      }
      
      # Native platform searches (optional)
      native <- build_links_common(parsed_terms(), input$plats %||% character(0))
      
      bind_rows(dorks, native) %>%
        arrange(Category, Platform, Term) %>%
        mutate(
          Open = sprintf(
            '<a class="btn btn-primary btn-sm open-link" href="%s" target="_blank" rel="noopener" data-url="%s">Open</a>',
            html_attr_escape(URL), html_attr_escape(URL)
          )
        )
    }, ignoreInit = TRUE)
    
    output$tbl <- renderDT({
      req(results())
      datatable(
        results() %>% select(Term, Platform, Category, Open),
        escape = FALSE, rownames = FALSE, selection = "multiple",
        options = list(pageLength = 50, dom = 'Bfrtip')
      )
    })
    
    # CSV export
    output$dl_csv <- downloadHandler(
      filename=function() paste0("search_links_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv"),
      content=function(file){
        req(results())
        out <- results() %>% select(Term, Platform, Category, URL)
        readr::write_csv(out, file)
      }
    )
    
    # Browser status + test
    output$browser_status <- renderText({
      paste0("Selected browser: ", input$browser)
    })
    observeEvent(input$test_browser, {
      open_in_browser(urls = "https://example.com/?gms_test=1",
                      browser = input$browser, session = session)
    })
    
    # Sign-in prep
    observeEvent(input$login_prep, {
      urls <- login_urls_for(input$login_plats); if (!length(urls)) return(NULL)
      open_in_browser(urls = urls, browser = input$browser, session = session)
    })
    
    # Per-row Open for external browsers (triggered by JS with a unique timestamp)
    observeEvent(input$open_url_external$ts, {
      url <- input$open_url_external$url
      req(url)
      open_in_browser(urls = url, browser = input$browser, session = session)
    }, ignoreInit = TRUE)
    
    # Bulk open
    observeEvent(input$open_selected, {
      tbl <- results(); req(tbl); idx <- input$tbl_rows_selected; if (!length(idx)) return(NULL)
      urls <- unname(tbl$URL[idx])
      open_in_browser(urls = urls, browser = input$browser, session = session)
    })
    observeEvent(input$open_all, {
      tbl <- results(); req(tbl)
      urls <- unname(tbl$URL)
      open_in_browser(urls = urls, browser = input$browser, session = session)
    })
    
    # Save / load UI state as RDS
    output$save_state_rds <- downloadHandler(
      filename = function() {
        paste0("gms_search_router_state_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".rds")
      },
      content = function(file) {
        state <- gms_router_state_capture(input)
        saveRDS(state, file = file)
      }
    )
    
    observeEvent(input$load_state_rds, {
      req(input$load_state_rds$datapath)
      state <- tryCatch(
        readRDS(input$load_state_rds$datapath),
        error = function(e) {
          showNotification("Could not read RDS file.", type = "error")
          return(NULL)
        }
      )
      if (is.null(state)) return(NULL)
      
      gms_router_state_apply(session, state)
      showNotification("Inputs restored from RDS.", type = "message")
    }, ignoreInit = TRUE)
    
  }
  
  shinyApp(ui, server)
}
