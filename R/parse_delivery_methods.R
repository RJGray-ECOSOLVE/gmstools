#' Parse and standardize delivery methods from free-text fields
#'
#' @description
#' Cleans messy delivery/shipping strings (multiple labels per cell, brand lists
#' in parentheses, mixed separators), standardizes them into a concise set of
#' delivery method categories, optionally deduplicates categories per row to
#' avoid overcounting (e.g., "courier, JNE, JNT"), and returns a frequency table.
#'
#' @details
#' The parser:
#' \itemize{
#'   \item Treats empty/NA-ish inputs (e.g., `""`, `"na"`, `"unknown"`) as **Unspecified**.
#'   \item Pulls brand names out of parentheses and splits on commas, slashes, `&`, and "and/or".
#'   \item Drops numeric/currency/time tokens (e.g., `"3 days"`, `"₦ 999"`).
#'   \item Maps tokens to categories in this order (first match wins):
#'     \itemize{
#'       \item \strong{Delivery App (Grab/Gojek)}
#'       \item \strong{Postal Service} (e.g., SEPOMEX, Postnet, Pos Indonesia, postage)
#'       \item \strong{Courier/Parcel} (e.g., DHL, FedEx, UPS, EMS, J\&T/JNT, JNE, SiCepat, Anteraja, Lion Parcel, Ninja Xpress/Express, Tiki, Interrapidisimo, Amazon Delivery, The Courier Guy)
#'       \item \strong{PUDO/Collection} (e.g., PUDO, lockers, collection)
#'       \item \strong{In-person/Meetup} (e.g., in-person, subway/metro meetups)
#'       \item \strong{National Shipping}
#'       \item \strong{International Shipping} (e.g., USA/UK/Canada/Australia mentions)
#'       \item \strong{Express Shipping/Overnight}
#'       \item \strong{Shipping Included/Free}
#'       \item \strong{Payment on Delivery (COD)} (COD, cash on delivery)
#'       \item \strong{Air Cargo} (airline/plane)
#'       \item \strong{Private Vehicle}
#'       \item \strong{Shipping (Generic)} (shipment/shipping with no further detail)
#'       \item \strong{Unspecified}
#'     }
#' }
#'
#' @param x A character vector (or list/atomic) containing delivery method strings.
#' @param dedup_per_row Logical, default `TRUE`. If `TRUE`, deduplicate categories
#'   per original row before counting (so "courier, JNE, JNT" counts once as Courier/Parcel).
#'
#' @return A tibble with columns:
#' \describe{
#'   \item{delivery_method}{Standardized delivery category.}
#'   \item{n}{Count (frequency).}
#'   \item{pct}{Percentage of total (0–100).}
#' }
#'
#' @examples
#' delivery_vec <- c(
#'   "Courier (JNT, JNE, SiCepat)", "DHL shipment", "Free shipping",
#'   "In person delivery in Mexico City or national shipment", "Gojek/ Grab",
#'   "SEPOMEX", "Payment on delivery", "Unknown", NA_character_
#' )
#' parse_delivery_methods(delivery_vec)
#'
#' @export
#' @encoding UTF-8
#' @importFrom dplyr mutate filter count arrange desc distinct rename
#' @importFrom stringr str_squish str_to_lower str_detect str_replace_all
#' @importFrom tidyr separate_rows
#' @importFrom tibble tibble
parse_delivery_methods <- function(x, dedup_per_row = TRUE) {
  stopifnot(is.atomic(x) || is.list(x))
  x <- as.character(x)
  
  # -- normalize whole-cell strings
  x[is.na(x)] <- ""
  x <- stringr::str_squish(x)
  
  # map empty/NA-like to Unspecified
  empty_like <- stringr::str_detect(
    stringr::str_to_lower(x),
    "^(|na|n|none|null|unspecified|unspecifies|unknown|unkown|un|y)$"
  )
  x[empty_like] <- "Unspecified"
  
  # bring parentheses content out as tokens; unify separators
  flat <- stringr::str_replace_all(x, "\\(([^)]*)\\)", ", \\1")
  flat <- stringr::str_replace_all(flat, "\\band\\b|\\bor\\b|&|/", ",")
  flat <- stringr::str_replace_all(flat, ";|\\|", ",")
  flat <- stringr::str_replace_all(flat, ",\\s*,+", ",")   # collapse multi-commas
  
  # split to tokens with row ids
  df <- tibble::tibble(row_id = seq_along(flat), raw = flat) |>
    tidyr::separate_rows(raw, sep = ",") |>
    dplyr::mutate(
      tok = stringr::str_squish(raw),
      tl  = stringr::str_to_lower(tok)
    ) |>
    dplyr::filter(tl != "")
  
  # drop numeric/currency/time junk tokens
  df <- df |>
    dplyr::filter(!stringr::str_detect(tl, "^[0-9]+$")) |>
    dplyr::filter(!stringr::str_detect(tl, "₦|ngn|\\$|€|£|\\b\\d+\\s*(day|days|hrs?|hours)\\b"))
  
  # standardize to delivery categories (order matters)
  df <- df |>
    dplyr::mutate(category = dplyr::case_when(
      # Delivery apps
      stringr::str_detect(tl, "\\bgrab\\b|\\bgojek\\b") ~ "Delivery App (Grab/Gojek)",
      stringr::str_detect(tl, "delivery apps?") ~ "Delivery App (Grab/Gojek)",
      
      # Postal service (national posts / postage)
      stringr::str_detect(tl, "sepomex|\\bpostnet\\b|\\bpostage\\b|\\bpostal\\b|\\bpost\\b(?!net)") ~ "Postal Service",
      stringr::str_detect(tl, "pos indonesia") ~ "Postal Service",
      
      # Major couriers / parcel companies (intl & local)
      stringr::str_detect(tl, "dhl|fedex|ups|ems|interrapidisimo|zoologistica|the courier guy|amazon delivery") ~ "Courier/Parcel",
      stringr::str_detect(tl, "j\\s*&?\\s*t|\\bjnt\\b|\\bjne\\b|sicepat|si\\s*cepat|anter\\s*aja|lion\\s*parcel|ninja\\s*xpress|ninja\\s*express|tiki") ~ "Courier/Parcel",
      stringr::str_detect(tl, "courier|courrier|delivery partner|ขนส่ง") ~ "Courier/Parcel",
      
      # PUDO / locker / collection
      stringr::str_detect(tl, "\\bpudo\\b|locker|\\bcollect(ion)?\\b") ~ "PUDO/Collection",
      
      # In-person / meetup
      stringr::str_detect(tl, "in[- ]?person|meet(ing)?\\b|subway|metro|in mexico city(?!.*shipment)") ~ "In-person/Meetup",
      
      # Shipping scope
      stringr::str_detect(tl, "national\\s+ship") ~ "National Shipping",
      stringr::str_detect(tl, "international|usa ship|uk|canada|australia") ~ "International Shipping",
      
      # Speed / cost flags
      stringr::str_detect(tl, "express|overnight") ~ "Express Shipping/Overnight",
      stringr::str_detect(tl, "free shipping|shipment included|price include(s)?\\s+shipment|included with the price") ~ "Shipping Included/Free",
      
      # COD phrasing often appears as delivery method in ads
      stringr::str_detect(tl, "payment on delivery|\\bcod\\b|cash on delivery") ~ "Payment on Delivery (COD)",
      
      # Transport modes
      stringr::str_detect(tl, "airline|by plane|shipment by plane|air cargo") ~ "Air Cargo",
      stringr::str_detect(tl, "private (passenger )?vehicle|private passenger transportation") ~ "Private Vehicle",
      
      # Generic shipping catch-all (if nothing more specific)
      stringr::str_detect(tl, "\\bshipment\\b|\\bshipping\\b|\\bshipments\\b") ~ "Shipping (Generic)",
      
      # Unspecified bucket
      stringr::str_detect(tl, "^unspecified$|^unknown$|^unkown$|^un$") ~ "Unspecified",
      
      TRUE ~ NA_character_
    )) |>
    dplyr::filter(!is.na(category))
  
  # optional: de-duplicate categories per row to prevent overcount
  if (isTRUE(dedup_per_row)) {
    df <- df |>
      dplyr::distinct(row_id, category)
  }
  
  # frequency table
  out <- df |>
    dplyr::count(category, name = "n") |>
    dplyr::arrange(dplyr::desc(n)) |>
    dplyr::mutate(pct = n / sum(n) * 100) |>
    dplyr::rename(delivery_method = category)
  
  out
}

# Silence R CMD check notes for NSE columns created within dplyr pipelines
utils::globalVariables(c("raw", "tok", "tl", "category", "row_id"))
