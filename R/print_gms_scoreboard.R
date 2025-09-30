#' Print GMS KPI Scoreboard to Console
#'
#' Outputs key summary statistics from a cleaned GMS dataset using cat(),
#' including counts of species, platforms, languages, and red flag indicators.
#'
#' @param data A cleaned GMS data frame.
#' @return NULL (prints summary to console).
#' @export
print_gms_scoreboard <- function(data) {
  library(dplyr)
  library(lubridate)
  library(stringr)
  
  # --- Date handling ----------------------------------------------------------
  record_dates <- suppressWarnings(lubridate::parse_date_time(
    data$record_date, orders = c("ymd", "dmy", "mdy", "Ymd")
  ))
  
  cat("========= GMS DATA SCOREBOARD =========\n")
  
  record_dates <- record_dates[!is.na(record_dates)]
  record_dates <- record_dates[record_dates >= as.Date("2024-04-01")]
  
  if (length(record_dates) > 0) {
    cat("Date Range:                   ",
        format(min(record_dates)), "to", format(max(record_dates)), "\n")
  } else {
    cat("Date Range:                   Not detected\n")
  }
  
  # --- Platforms & totals -----------------------------------------------------
  cat("Total records:                ", nrow(data), "\n")
  cat("Unique platforms:             ", dplyr::n_distinct(data$platform_name), "\n")
  
  # --- Languages: split multi-values and count unique clean tokens ------------
  # Split on comma/semicolon/slash/pipe/whitespace, keep 2–3 letter alpha codes
  lang_tokens <- data$ad_language %>%
    as.character() %>%
    replace(is.na(.), "") %>%
    paste(collapse = ",") %>%                                  # pool, then split once
    str_split(pattern = "[,;/|\\s]+", simplify = FALSE) %>%
    purrr::pluck(1) %>%
    str_trim() %>%
    str_to_lower() %>%
    str_replace_all("[^a-z]", "") %>%                          # drop non-letters
    { .[. != ""] } %>%
    { .[nchar(.) >= 2 & nchar(.) <= 3] }                       # keep 2–3 letter codes
  
  cat("Languages searched:           ", length(unique(lang_tokens)), "\n")
  
  # --- Species (taxa groups) --------------------------------------------------
  cat("Unique species (taxa groups): ", dplyr::n_distinct(data$item_taxa), "\n")
  
  # --- Species (scientific): keep only true binomials -------------------------
  # Rules: non-empty, not NA/"NA", no 'sp.'/'spp.', exactly two words,
  # pattern 'Genus species' (Genus capitalized, species lowercase, hyphen allowed).
  species_clean <- data$item_species %>%
    as.character() %>%
    replace(is.na(.), "") %>%
    str_squish()
  
  species_binomials <- species_clean %>%
    .[. != "" & !tolower(.) %in% c("na", "n/a")] %>%
    .[!str_detect(., "\\bsp\\.?\\b|\\bspp\\.?\\b")] %>%
    .[str_detect(., "^[A-Z][a-z]+\\s+[a-z]+(?:-[a-z]+)?$")]
  
  cat("Unique species (scientific):  ", length(unique(species_binomials)), "\n")
  
  # --- Geography / hubs / sellers --------------------------------------------
  cat("Unique countries:             ", dplyr::n_distinct(data$location_level0), "\n")
  cat("Number of Hubs Active:        ", dplyr::n_distinct(data$datahub), "\n")
  cat("Unique sellers (named):       ",
      dplyr::n_distinct(na.omit(trimws(as.character(data$item_seller_name)))), "\n")
  
  # --- Red flags --------------------------------------------------------------
  cat("\n--------- Red Flag Indicators ---------\n")
  cat("With delivery available:      ",
      sum(as.logical(data$is_delivery_available), na.rm = TRUE), "\n")
  cat("With price available:         ",
      sum(as.logical(data$price_available), na.rm = TRUE), "\n")
  cat("With seller contact:          ",
      sum(as.logical(data$seller_contact_known), na.rm = TRUE), "\n")
  cat("With images:                  ",
      sum(as.logical(data$item_image), na.rm = TRUE), "\n")
  cat("Multiple detections:          ",
      sum(as.logical(data$item_multiple_detections), na.rm = TRUE), "\n")
  cat("Potential cases of interest:  ",
      sum(nchar(trimws(as.character(data$note))) > 0, na.rm = TRUE), "\n")
  
  cat("=======================================\n")
  
  invisible(NULL)
}
