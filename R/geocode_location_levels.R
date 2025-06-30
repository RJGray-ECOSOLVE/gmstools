#' Geocode Location Fields Using World Map Boundaries
#'
#' Joins GMS hierarchical location fields with geographic polygons from rnaturalearth (level 0)
#' or GADM administrative boundaries (level 1–2) to assign geographic coordinates or full geometries.
#' This allows mapping of GMS monitoring data using country or subnational locations.
#'
#' @param data A GMS dataset with location fields.
#' @param levels Character vector of location field names to use for join. Default is
#'   \code{c("location_level0", "location_level1")}.
#' @param return_sf Logical; if TRUE, returns an sf object with full geometry column. If FALSE,
#'   returns a data frame with `latitude` and `longitude` from centroids.
#' @param gadm_level Integer; administrative boundary level to use from GADM. 0 = country, 1 = province/state, 2 = district.
#'
#' @return Either a data frame with `latitude` and `longitude` columns (if `return_sf = FALSE`),
#'   or an `sf` object with full polygon geometry (if `return_sf = TRUE`).
#'
#' @examples
#' \dontrun{
#' # Load and clean data
#' dat <- clean_gms_data(read.csv("adverts_export.csv"), date_format = "dmy")
#'
#' # Add centroid coordinates using country + region fields
#' dat_geo <- geocode_location_levels(dat,
#'                                    levels = c("location_level0", "location_level1"),
#'                                    return_sf = FALSE,
#'                                    gadm_level = 1)
#'
#' # Return full geometries for mapping
#' dat_sf <- geocode_location_levels(dat,
#'                                   levels = c("location_level0", "location_level1"),
#'                                   return_sf = TRUE,
#'                                   gadm_level = 1)
#' }
#' @export
geocode_location_levels <- function(data,
                                    levels = c("location_level0", "location_level1"),
                                    return_sf = FALSE,
                                    gadm_level = 1) {
  library(dplyr)
  library(sf)
  library(rnaturalearth)
  library(rnaturalearthdata)
  library(geodata)

  # Clean location fields
  data <- data %>%
    mutate(across(all_of(levels), ~ as.character(.x))) %>%
    mutate(location_full = do.call(paste, c(across(all_of(levels)), sep = ", ")))

  # Load world polygons by country
  if (gadm_level == 0) {
    world <- ne_countries(scale = "medium", returnclass = "sf") %>%
      st_make_valid()
    data_geo <- left_join(data, world, by = c(location_level0 = "admin"))
  } else {
    message("Downloading GADM boundaries (level ", gadm_level, ")...")
    countries <- unique(data$location_level0)

    gadm_list <- lapply(countries, function(cty) {
      tryCatch({
        gadm(country = cty, level = gadm_level, path = tempdir())
      }, error = function(e) NULL)
    })

    gadm_combined <- do.call(rbind, gadm_list) %>% st_make_valid()

    # Create join key
    gadm_combined$location_full <- do.call(paste, gadm_combined[, 1:length(levels)], sep = ", ")
    data_geo <- left_join(data, gadm_combined, by = "location_full")
  }

  # Return sf or lat/lon
  if (return_sf) {
    return(data_geo)
  } else {
    centroids <- st_centroid(st_geometry(data_geo))
    coords <- st_coordinates(centroids)
    data_geo$longitude <- coords[, 1]
    data_geo$latitude <- coords[, 2]
    data_geo <- st_drop_geometry(data_geo)
    return(data_geo)
  }
}
