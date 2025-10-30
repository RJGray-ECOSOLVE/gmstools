gms_kpi_paragraph <- function(data,
                              taxa_col     = "item_taxa",
                              platform_col = "platform_name",
                              cites_col    = "item_cites",
                              language_col = NULL,       # e.g., "language" if present
                              hub_col      = NULL) {     # e.g., "data_hub" if present
  # --- helpers ---
  n_fmt   <- function(x) formatC(as.integer(round(x, 0)), big.mark = ",", format = "d")
  pct     <- function(x, tot) ifelse(tot > 0, round(100 * x / tot), 0)
  has_col <- function(df, col) !is.null(col) && col %in% names(df)

  fmt_topn <- function(x, n = 5, total_n = nrow(data)) {
    # x is a vector; returns "A (10%), B (8%), C (5%) and D (3%)"
    tab  <- sort(table(x), decreasing = TRUE)
    if (length(tab) == 0) return("n/a")
    tab  <- head(tab, n)
    labs <- names(tab)
    prc  <- pct(as.numeric(tab), total_n)
    bits <- sprintf("%s (%s%%)", labs, prc)
    if (length(bits) == 1) return(bits)
    if (length(bits) == 2) return(paste(bits, collapse = " and "))
    paste0(paste(bits[-length(bits)], collapse = ", "), ", and ", bits[length(bits)])
  }

  # --- totals ---
  total_n <- nrow(data)

  # --- platforms: top 5 with % ---
  if (!platform_col %in% names(data)) stop("Column '", platform_col, "' not found.")
  top5_platforms <- fmt_topn(na.omit(data[[platform_col]]), n = 5, total_n = total_n)

  # --- taxa: top 5 with % ---
  if (!taxa_col %in% names(data)) stop("Column '", taxa_col, "' not found.")
  top5_taxa <- fmt_topn(na.omit(data[[taxa_col]]), n = 5, total_n = total_n)

  # --- unique platforms / languages / hubs ---
  n_platforms <- length(unique(na.omit(data[[platform_col]])))
  lang_clause <- if (has_col(data, language_col)) {
    paste0(", written in ", length(unique(na.omit(data[[language_col]]))), " different languages")
  } else ""
  hub_clause  <- if (has_col(data, hub_col)) {
    paste0(", across ", length(unique(na.omit(data[[hub_col]]))), " contributing data hubs")
  } else ""

  # --- CITES Appendix I % (treat any "I/II" or combos that include I as I) ---
  if (!cites_col %in% names(data)) stop("Column '", cites_col, "' not found.")
  cites_vec <- as.character(data[[cites_col]])

  norm_cites <- function(x) {
    # Lower, strip "cites" / "appendix" / punctuation-like; split to roman tokens
    if (is.na(x) || !nzchar(x)) return(NA_character_)
    y <- tolower(x)
    y <- gsub("cites|appendix|appendices|app\\.|\\s", "", y, perl = TRUE)
    # Split on any non-letter to find tokens like i, ii, iii
    toks <- unlist(strsplit(y, "[^a-z]+", perl = TRUE))
    toks <- toks[nzchar(toks)]
    # Keep only roman options we care about
    toks[toks %in% c("i","ii","iii")]
  }

  has_I <- vapply(cites_vec, function(z) {
    tok <- norm_cites(z)
    if (length(tok) == 0 || all(is.na(tok))) return(NA)
    # Count as Appendix I if *any* token is exactly "i" (so "i/ii" => I)
    "i" %in% tok
  }, logical(1))

  cites_I_pct <- if (all(is.na(has_I))) 0 else round(100 * mean(has_I, na.rm = TRUE))

  # --- paragraph assembly ---
  paragraph <- paste0(
    "Since the start of our project, the Global Monitoring System (GMS) recorded nearly ",
    n_fmt(total_n), " online wildlife trade ads. ",
    "Top 5 platforms by volume: ", top5_platforms, ". ",
    "Top 5 taxa: ", top5_taxa, ". ",
    "To date, the GMS has detected illegal wildlife advertisements on ",
    n_platforms, " platforms",
    lang_clause,
    hub_clause,
    ". Alarmingly, ~", cites_I_pct,
    "% of listings involve CITES Appendix I species (treating combined listings such as \"CITES I/II\" as Appendix I)."
  )

  cat(paragraph, "\n")
  invisible(paragraph)
}
