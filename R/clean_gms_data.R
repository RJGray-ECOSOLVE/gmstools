#' Clean and Standardize GMS Wildlife Trade Data (with canonical aggregation)
#'
#' - Normalizes whitespace/case; handles non-breaking spaces
#' - item_taxa canonicalization (incl. Sharks/Rays + Marine -> Freshwater/Marine)
#' - item_common_name harmonization via explicit alias map + safe generic→"(Unspecified)" rules
#' - Flexible date parsing; future-date capping; pre-2024-04-14 filtering
#'
#' @param data data.frame/tibble
#' @param fix_names logical; snake_case columns
#' @param prefer_date vector of orders for lubridate::parse_date_time
#' @param keep_primates_separate logical; if TRUE leaves "Primates" as a distinct taxa
#' @return tibble with attr "removed_before_2024_04_14"
#' @export


clean_gms_data <- function(
    data,
    fix_names = TRUE,
    prefer_date = c("Y-m-d","d/m/Y","m/d/Y","d-b-Y","b d Y","d.m.Y","m.d.Y","Y/m/d","dmY","mdY","Ymd"),
    keep_primates_separate = FALSE
) {
  suppressPackageStartupMessages({
    library(dplyr); library(stringr); library(janitor)
    library(lubridate); library(tidyr); library(purrr); library(tibble)
  })
  
  if (fix_names) data <- janitor::clean_names(data)
  
  n2na <- function(x) ifelse(x == "", NA, x)
  squish_nbsp <- function(x) {
    if (!is.character(x)) return(x)
    x %>% str_replace_all("\u00A0", " ") %>% str_replace_all("\\s+", " ") %>% str_trim()
  }
  titleish <- function(x) if (is.character(x)) str_to_title(str_to_lower(x)) else x
  parse_date_flexible <- function(x) {
    dt <- suppressWarnings(lubridate::parse_date_time(x, orders = prefer_date, quiet = TRUE))
    as.Date(dt)
  }
  as_bool_loose <- function(x) {
    if (is.logical(x)) return(x)
    z <- tolower(trimws(as.character(x)))
    dplyr::case_when(
      z %in% c("1","true","t","yes","y","si","sí") ~ TRUE,
      z %in% c("0","false","f","no","n")           ~ FALSE,
      TRUE                                         ~ NA
    )
  }
  
  # ---- generic hygiene across character columns ----
  data <- data %>%
    mutate(across(everything(), ~ if (is.character(.x)) n2na(.x) else .x)) %>%
    mutate(across(where(is.character), squish_nbsp))
  
  # ---- dates ----
  today_sys <- lubridate::today()
  if ("record_date" %in% names(data)) data <- data %>% mutate(record_date = parse_date_flexible(record_date)) else data$record_date <- as.Date(NA)
  if ("item_date_posted" %in% names(data)) data <- data %>% mutate(item_date_posted = parse_date_flexible(item_date_posted)) else data$item_date_posted <- as.Date(NA)
  
  data <- data %>%
    mutate(
      record_date      = if_else(!is.na(record_date)      & record_date      > today_sys, today_sys, record_date),
      item_date_posted = if_else(!is.na(item_date_posted) & item_date_posted > today_sys, today_sys, item_date_posted)
    )
  
  cutoff <- as.Date("2024-04-14")
  pre_n <- nrow(data)
  data <- data %>%
    mutate(ref_date = coalesce(record_date, item_date_posted)) %>%
    filter(is.na(ref_date) | ref_date >= cutoff) %>%
    select(-ref_date)
  attr(data, "removed_before_2024_04_14") <- pre_n - nrow(data)
  
  # ---- item_taxa canonicalization ----
  if ("item_taxa" %in% names(data)) {
    std_taxa <- function(x) {
      y <- x %>% as.character() %>% str_replace_all("\u00A0", " ") %>% str_squish() %>% str_to_lower()
      out <- dplyr::case_when(
        str_detect(y, "^mamm(al|als)?$")                         ~ "Mammals",
        str_detect(y, "^bird(s)?$|\\baves\\b")                   ~ "Birds",
        str_detect(y, "^reptil(e|es)$|squamata|testudines")      ~ "Reptiles",
        str_detect(y, "^amphib(ian|ians)?$|anura|caudata")       ~ "Amphibians",
        str_detect(y, "^fish$|^fishes$|actinopterygii|chondric") ~ "Freshwater/Marine",
        str_detect(y, "invertebr|arthropod|mollusc|mollusk")     ~ "Invertebrates",
        str_detect(y, "plant|flora")                             ~ "Plants",
        str_detect(y, "coral|cnidaria|scleractinia")             ~ "Corals",
        str_detect(y, "^sharks?\\s*and\\s*rays$")                ~ "Freshwater/Marine",
        str_detect(y, "^marine$")                                ~ "Freshwater/Marine",
        str_detect(y, "^freshwater/?marine$")                    ~ "Freshwater/Marine",
        str_detect(y, "^primates?$") & !keep_primates_separate   ~ "Mammals",
        TRUE                                                     ~ str_to_title(y)
      )
      out
    }
    data <- data %>% mutate(item_taxa = std_taxa(item_taxa))
  }
  
  # ---- item_common_name canonicalization ----
  if ("item_common_name" %in% names(data)) {
    # coerce to character to avoid factor issues in nzchar/regex
    data <- data %>% mutate(item_common_name = as.character(item_common_name))
    
    # Use a named character vector, but do SAFE lookup with [ ]
    alias_map <- c(
      # Macaws
      "Blue-And-Yellow Macaw" = "Blue-and-yellow Macaw",
      "Blue And Gold Macaw"   = "Blue-and-yellow Macaw",
      # African grey parrot normalization (aggregate variants)
      "Grey Parrot"           = "African grey parrot",
      "African Grey Parrot"   = "African grey parrot",
      "African Grey parrot"   = "African grey parrot"
    )
    
    canon_common <- function(x) {
      if (is.na(x) || !nzchar(x)) return(x)
      s <- x %>% squish_nbsp() %>% titleish()
      
      # SAFE aliasing for vectors: alias_map[s] returns NA if not present (no subscript error)
      ali <- alias_map[s]
      if (!is.na(ali)) s <- ali
      
      s_low <- tolower(s)
      
      # Generic → (Unspecified), with your exceptions
      if (str_detect(s_low, "^pangolin(s)?(\\s*spp\\.?|\\s*\\(.*\\))?$") ||
          s_low %in% c("pangolin","pangolins","pangolin spp.","pangolin (unspecified)")) {
        s <- "Pangolin (Unspecified)"
      }
      if (s_low %in% c("ray","rays","rays (unspecified)","ray (unspecified)")) {
        s <- "Rays (Unspecified)"
      }
      if (s_low %in% c("parrots","parrot (unspecified)","parrots (unspecified)","parrot")) {
        s <- "Parrots (Unspecified)"
      }
      if (s_low %in% c("elephant","elephants")) {
        s <- "Elephant (Unspecified)"
      }
      if (s_low %in% c("monitor","monitor lizard","monitor lizard (unspecified)")) {
        s <- "Monitor Lizard (Unspecified)"
      }
      if (s_low %in% c("seahorse","seahorses")) {
        s <- "Seahorse (Unspecified)"
      }
      if (s_low %in% c("falcon","falcons")) {
        s <- "Falcon (Unspecified)"
      }
      if (s_low %in% c("whale","whales")) {
        s <- "Whale (Unspecified)"
      }
      if (s_low %in% c("crocodile","crocodiles")) {
        s <- "Crocodile (Unspecified)"
      }
      if (s_low %in% c("tortoise","tortoises")) {
        s <- "Tortoise (Unspecified)"
      }
      if (s_low %in% c("python","pythons","python (unspecified)")) {
        s <- "Python (Unspecified)"
      }
      if (s_low %in% c("parakeet","parakeets")) {
        s <- "Parakeet (Unspecified)"
      }
      
      s
    }
    
    data <- data %>%
      mutate(item_common_name = if_else(
        is.na(item_common_name),
        NA_character_,
        vapply(item_common_name, canon_common, character(1))
      ))
  }
  
  # ---- website common name: basic title-case/trim only ----
  if ("item_common_name_website" %in% names(data)) {
    data <- data %>% mutate(item_common_name_website = as.character(item_common_name_website) %>% titleish())
  }
  
  # ---- platform tidy ----
  if ("platform_name" %in% names(data)) {
    data <- data %>% mutate(platform_name = str_squish(platform_name))
  }
  
  # ---- booleans ----
  bool_cols <- c(
    "was_recommended","is_case_of_interest","location_known","price_available",
    "seller_contact_known","item_has_eggs","item_multiple_detections",
    "item_image","image_text","is_group","is_delivery_available",
    "is_seller_vulnerable_group"
  )
  bool_cols <- intersect(bool_cols, names(data))
  if (length(bool_cols)) {
    data <- data %>% mutate(across(all_of(bool_cols), as_bool_loose))
  }
  
  as_tibble(data)
}
