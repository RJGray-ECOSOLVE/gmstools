#' Clean and Standardize GMS Wildlife Trade Data
#'
#' Prepares a raw GMS data export for analysis by converting field types,
#' standardizing formatting, and handling missing values.
#'
#' @param data A raw GMS dataset (as read from CSV or Excel).
#' @param fix_names Logical; whether to convert column names to snake_case.
#' @param date_format Character; format for parsing date columns (e.g., "dmy", "ymd", "mdy").
#'
#' @return A cleaned data frame with consistent data types and formats.
#' @export
#'
#' @examples
#' dat <- read.csv("adverts_export.csv")
#' clean <- clean_gms_data(dat, date_format = "dmy")
clean_gms_data <- function(data, fix_names = TRUE, date_format = "dmy") {
  library(dplyr)
  library(stringr)
  library(janitor)
  library(lubridate)

  if (fix_names) {
    data <- janitor::clean_names(data)
  }

  # Helper to parse dates using lubridate based on format string
  parse_date_dynamic <- function(x, fmt) {
    fn <- match.fun(fmt)  # converts "dmy" → lubridate::dmy, etc.
    suppressWarnings(fn(x))
  }

  data <- data %>%
    mutate(across(everything(), ~ ifelse(.x == "", NA, .x))) %>%
    mutate(across(where(is.character), str_trim)) %>%

    mutate(
      record_date = parse_date_dynamic(record_date, date_format),
      item_date_posted = parse_date_dynamic(item_date_posted, date_format)
    ) %>%

    mutate(across(
      c(
        was_recommended, is_case_of_interest, location_known, price_available,
        seller_contact_known, item_has_eggs, item_multiple_detections,
        item_image, image_text, is_group, is_delivery_available,
        is_seller_vulnerable_group
      ),
      ~ as.logical(.x)
    )) %>%

    mutate(
      platform_name = factor(platform_name),
      item_taxa = factor(item_taxa),
      item_type = factor(item_type),
      location_level0 = factor(location_level0),
      datahub = factor(datahub)
    )

  return(data)
}
