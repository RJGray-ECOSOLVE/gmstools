#' Summarize Wildlife Trade Activity by Taxa
#'
#' Provides a summary of IWT posts grouped by item_taxa, including number of ads,
#' number of unique platforms, and optionally species richness and country coverage.
#'
#' @param data A cleaned GMS dataset.
#' @param include_platforms Logical; if TRUE, includes number of platforms per taxa.
#' @param include_species Logical; if TRUE, includes number of species per taxa.
#' @param include_countries Logical; if TRUE, includes number of countries per taxa.
#'
#' @return A data frame summarizing post counts and characteristics by taxa.
#' @export
#'
#' @examples
#' # Summarize ads by taxa with platform and country stats
#' summarize_by_taxa(dat,
#'                   include_platforms = TRUE,
#'                   include_species = TRUE,
#'                   include_countries = TRUE)
summarize_by_taxa <- function(data,
                              include_platforms = TRUE,
                              include_species = TRUE,
                              include_countries = TRUE) {
  library(dplyr)

  if (!"item_taxa" %in% names(data)) {
    stop("Missing 'item_taxa' field in the data.")
  }

  out <- data %>%
    group_by(item_taxa) %>%
    summarise(
      ad_count = n(),
      .groups = "drop"
    )

  if (include_platforms && "platform_name" %in% names(data)) {
    platform_counts <- data %>%
      group_by(item_taxa) %>%
      summarise(n_platforms = n_distinct(platform_name), .groups = "drop")
    out <- left_join(out, platform_counts, by = "item_taxa")
  }

  if (include_species && "item_common_name_website" %in% names(data)) {
    species_counts <- data %>%
      group_by(item_taxa) %>%
      summarise(n_species = n_distinct(item_common_name_website), .groups = "drop")
    out <- left_join(out, species_counts, by = "item_taxa")
  }

  if (include_countries && "location_level0" %in% names(data)) {
    country_counts <- data %>%
      group_by(item_taxa) %>%
      summarise(n_countries = n_distinct(location_level0), .groups = "drop")
    out <- left_join(out, country_counts, by = "item_taxa")
  }

  return(out)
}
