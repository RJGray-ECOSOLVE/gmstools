gms_kpi_paragraph <- function(data,
                              taxa_col     = "item_taxa",
                              platform_col = "platform_name",
                              cites_col    = "item_cites",  # cleaned col name
                              language_col = NULL,          # e.g., "language"
                              hub_col      = NULL) {        # e.g., "data_hub"
  # --- helpers ---
  n_fmt   <- function(x) formatC(as.integer(round(x, 0)), big.mark = ",", format = "d")
  pct     <- function(n, tot) if (tot > 0) round(100 * n / tot) else 0
  has_col <- function(df, col) !is.null(col) && col %in% names(df)
  
  # Fallback if user passes the old mixed-case name
  if (!has_col(data, cites_col) && has_col(data, "item_CITES")) {
    cites_col <- "item_CITES"
  }
  
  # Defensive: ensure the required columns exist
  if (!has_col(data, platform_col)) stop("Column '", platform_col, "' not found.")
  if (!has_col(data, taxa_col))     stop("Column '", taxa_col,     "' not found.")
  if (!has_col(data, cites_col))    stop("Column '", cites_col,    "' not found.")
  
  total_n <- nrow(data)
  
  # Robust top-N formatter that avoids vector recycling
  fmt_topn <- function(vec, n = 5, total_n = total_n) {
    v <- vec[!is.na(vec)]
    if (length(v) == 0L) return("n/a")
    tab <- sort(table(v), decreasing = TRUE)
    if (length(tab) == 0L) return("n/a")
    tab <- head(tab, n)
    
    # Build a small data frame to compute % correctly per-row
    df <- tibble::tibble(
      name = names(tab),
      n    = as.numeric(tab)
    )
    df$prc <- pct(df$n, total_n)
    
    bits <- sprintf("%s (%s%%)", df$name, df$prc)
    if (length(bits) == 1) return(bits)
    if (length(bits) == 2) return(paste(bits, collapse = " and "))
    paste0(paste(bits[-length(bits)], collapse = ", "), ", and ", bits[length(bits)])
  }
  
  # --- top 5s ---
  top5_platforms <- fmt_topn(data[[platform_col]], n = 5, total_n = total_n)
  top5_taxa      <- fmt_topn(data[[taxa_col]],      n = 5, total_n = total_n)
  
  # --- uniques / optional clauses ---
  n_platforms <- length(unique(na.omit(data[[platform_col]])))
  lang_clause <- if (has_col(data, language_col)) {
    paste0(", written in ", length(unique(na.omit(data[[language_col]]))), " different languages")
  } else ""
  hub_clause  <- if (has_col(data, hub_col)) {
    paste0(", across ", length(unique(na.omit(data[[hub_col]]))), " contributing data hubs")
  } else ""
  
  # --- CITES I% (treat any label that includes I, e.g., "I/II", as I) ---
  cites_vec <- as.character(data[[cites_col]])
  norm_cites_tokens <- function(x) {
    if (is.na(x) || !nzchar(x)) return(character(0))
    y <- tolower(x)
    y <- gsub("cites|appendix|appendices|app\\.|\\s", "", y, perl = TRUE)
    toks <- unlist(strsplit(y, "[^a-z]+", perl = TRUE))
    toks <- toks[nzchar(toks)]
    toks[toks %in% c("i","ii","iii")]
  }
  has_I <- vapply(cites_vec, function(z) {
    toks <- norm_cites_tokens(z)
    if (length(toks) == 0L) return(NA)
    "i" %in% toks
  }, logical(1))
  cites_I_pct <- if (all(is.na(has_I))) 0 else round(100 * mean(has_I, na.rm = TRUE))
  
  # --- paragraph ---
  paragraph <- paste0(
    "Since the start of our project, the Global Monitoring System (GMS) recorded ",
    n_fmt(total_n), " online wildlife trade ads. ",
    "Top 5 platforms by volume: ", top5_platforms, ". ",
    "Top 5 taxa: ", top5_taxa, ". ",
    "To date, the GMS has detected illegal wildlife advertisements on ",
    n_platforms, " platforms",
    lang_clause,
    hub_clause,
    ". ~", cites_I_pct,
    "% of listings involve CITES Appendix I species."
  )
  
  cat(paragraph, "\n")
  invisible(paragraph)
}
