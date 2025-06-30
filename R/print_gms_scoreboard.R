#' Print GMS KPI Scoreboard to Console
#'
#' Outputs key summary statistics from a cleaned GMS dataset using cat(),
#' including counts of species, platforms, languages, and red flag indicators.
#'
#' @param data A cleaned GMS data frame.
#'
#' @return NULL (prints summary to console).
#' @export
#'
#' @examples
#' print_gms_scoreboard(dat)
print_gms_scoreboard <- function(data) {
  library(dplyr)
  library(lubridate)
  record_dates <- suppressWarnings(lubridate::parse_date_time(data$record_date,
                                                              orders = c("ymd", "dmy", "mdy", "Ymd")))

  cat("========= GMS DATA SCOREBOARD =========\n")
  # Drop NA dates and calculate min/max
  record_dates <- record_dates[!is.na(record_dates)]
  # Filter out NA and dates before 2024-04-01
  record_dates <- record_dates[!is.na(record_dates) & record_dates >= as.Date("2024-04-01")]

  if (length(record_dates) > 0) {
  cat("Date Range:                   ", format(min(record_dates)), "to", format(max(record_dates)), "\n")
  } else {
  cat("Date Range:                   Not detected\n")
  }
  cat("Total records:                ", nrow(data), "\n")
  cat("Unique platforms:             ", n_distinct(data$platform_name), "\n")
  cat("Languages searched:           ", n_distinct(data$ad_language), "\n")
  cat("Unique species (taxa groups): ", n_distinct(data$item_taxa), "\n")
  cat("Unique species (scientific):  ", n_distinct(data$item_species), "\n")
  cat("Unique countries:             ", n_distinct(data$location_level0), "\n")
  cat("Number of Hubs Active:        ", n_distinct(data$datahub), "\n")
  cat("Unique sellers (named):       ", sum(!is.na(data$item_seller_name)), "\n")

  cat("\n--------- Red Flag Indicators ---------\n")
  cat("With delivery available:      ", sum(as.logical(data$is_delivery_available), na.rm = TRUE), "\n")
  cat("With price available:         ", sum(as.logical(data$price_available), na.rm = TRUE), "\n")
  cat("With seller contact:          ", sum(as.logical(data$seller_contact_known), na.rm = TRUE), "\n")
  cat("With images:                  ", sum(as.logical(data$item_image), na.rm = TRUE), "\n")
  cat("Multiple detections:          ", sum(as.logical(data$item_multiple_detections), na.rm = TRUE), "\n")
  cat("Potential cases of interest:  ", sum(nchar(trimws(data$note)) > 0, na.rm = TRUE), "\n")

  cat("=======================================\n")
}

