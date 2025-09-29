# Dependencies:
# install.packages(c("sf","dplyr","stringr"))
library(sf)
library(dplyr)
library(stringr)

#' Rank level-1 regions within each country into Low/Medium/High risk and return sf
#'
#' @param adverts_df data.frame with columns for country and level-1 region
#' @param shp_level1 either an sf object OR a path (folder/file) readable by sf::st_read()
#' @param country_col name of country column in adverts_df (character)
#' @param region_col  name of level-1 column in adverts_df (character)
#' @param sf_country_col name of country column in the sf layer (defaults to country_col)
#' @param sf_region_col  name of level-1 column in the sf layer (defaults to region_col)
#' @param unify if TRUE, union geometries per (country, region) to guarantee one feature
#' @param drop_empty_region if TRUE, drop rows where region is NA/blank before ranking
#' @return sf MULTIPOLYGON with columns: Country, Province_State, n, risk_category, geometry
rank_level1_risk <- function(adverts_df,
                             shp_level1,
                             country_col = "location_level0",
                             region_col  = "location_level1",
                             sf_country_col = NULL,
                             sf_region_col  = NULL,
                             unify = TRUE,
                             drop_empty_region = TRUE) {
  
  # -------- helpers --------
  # robust 3-class quantile binning with safe fallbacks
  qclass3 <- function(x) {
    x <- as.numeric(x)
    if (length(x) < 3 || dplyr::n_distinct(x) == 1L) return(rep(2L, length(x))) # all Medium
    brks <- stats::quantile(x, probs = c(0, 1/3, 2/3, 1), na.rm = TRUE, type = 7)
    if (length(unique(brks)) < 4L) dplyr::ntile(x, 3) else
      as.integer(cut(x, breaks = brks, include.lowest = TRUE, labels = FALSE))
  }
  
  `%||%` <- function(a, b) if (!is.null(a)) a else b
  
  # -------- read / prep sf layer --------
  sf_layer <- if (inherits(shp_level1, "sf")) {
    shp_level1
  } else {
    # quiet read; relies on sf::st_read
    suppressMessages(sf::st_read(shp_level1, quiet = TRUE))
  }
  
  sf_country_col <- sf_country_col %||% country_col
  sf_region_col  <- sf_region_col  %||% region_col
  
  # normalize name fields and keep only what we need
  sf_pre <- sf_layer %>%
    mutate(
      country = .data[[sf_country_col]],
      region  = .data[[sf_region_col]]
    ) %>%
    select(country, region, geometry, everything())
  
  # ensure one geometry per (country, region)
  if (unify) {
    sf_pre <- sf_pre %>%
      group_by(country, region) %>%
      summarise(geometry = sf::st_union(geometry), .groups = "drop")
  } else {
    sf_pre <- sf_pre %>% distinct(country, region, .keep_all = TRUE)
  }
  
  # -------- aggregate adverts --------
  ads_sum <- adverts_df %>%
    mutate(
      country = .data[[country_col]],
      region  = .data[[region_col]],
      country = ifelse(!is.na(country), str_trim(country), country),
      region  = ifelse(!is.na(region),  str_trim(region),  region)
    ) %>%
    group_by(country, region) %>%
    summarise(n = dplyr::n(), .groups = "drop")
  
  # optional: drop blanks
  if (isTRUE(drop_empty_region)) {
    ads_sum <- ads_sum %>%
      mutate(region = na_if(region, "")) %>%
      filter(!is.na(region))
  }
  
  # -------- join and rank within country --------
  # join from the sf side so we keep sf class; then drop regions with no adverts
  joined <- sf_pre %>%
    left_join(ads_sum, by = c("country","region")) %>%
    filter(!is.na(n)) %>%
    mutate(n = as.numeric(n))
  
  ranked <- joined %>%
    group_by(country) %>%
    mutate(
      risk_score = qclass3(n),
      risk_category = factor(c("Low","Medium","High")[risk_score],
                             levels = c("Low","Medium","High"))
    ) %>%
    ungroup() %>%
    select(Country = country,
           Province_State = region,
           n,
           risk_category,
           geometry)
  
  # -------- enforce MULTIPOLYGON --------
  # Cast where possible; if needed, extract polygons then cast
  ranked <- suppressWarnings({
    tryCatch(
      sf::st_cast(ranked, "MULTIPOLYGON"),
      error = function(e) {
        sf::st_cast(sf::st_collection_extract(ranked, "POLYGON"), "MULTIPOLYGON")
      }
    )
  })
  
  ranked
}

# ---------------- Example usage ----------------
# result <- rank_level1_risk(
#   adverts_df   = dat,                 # your adverts data.frame
#   shp_level1   = "mapdata_level1",    # directory/file readable by sf::st_read(), OR an sf object
#   country_col  = "location_level0",
#   region_col   = "location_level1",
#   sf_country_col = "location_level0",
#   sf_region_col  = "location_level1",
#   unify = TRUE
# )
# result
# plot(result["risk_category"])
