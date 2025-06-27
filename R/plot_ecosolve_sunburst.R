#' Create a Nested Donut (Sunburst) Plot for ECO-SOLVE Data
#'
#' Generates a 3-level sunburst plot from ECO-SOLVE monitoring data using platform, taxa, and species fields.
#' Facebook is locked to blue tones; other platforms use a fixed ECO-SOLVE palette with auto-shading.
#'
#' @param data A cleaned data frame with platform, taxa, and species fields
#' @param platform_field Name of the platform field (character)
#' @param taxa_field Name of the taxa field (character)
#' @param species_field Name of the species field (character)
#' @param top_n_platforms Integer; how many top platforms to retain individually (others become "Other")
#' @param output_file Optional file name (e.g., "sunburst.html") to save the widget
#' @param png_file Optional PNG file path (requires webshot)
#'
#' @return A `plotly` sunburst object (and optionally saves HTML/PNG)
#' @export
plot_ecosolve_sunburst <- function(data,
                                   platform_field = "platform_name",
                                   taxa_field = "item_taxa",
                                   species_field = "item_common_name_website",
                                   top_n_platforms = 3,
                                   output_file = NULL,
                                   png_file = NULL) {

  library(dplyr)
  library(scales)
  library(colorspace)
  library(plotly)
  library(purrr)

  # Basic validation
  stopifnot(all(c(platform_field, taxa_field, species_field) %in% names(data)))

  dat <- data %>%
    filter(!is.na(.data[[platform_field]]),
           !is.na(.data[[taxa_field]]),
           !is.na(.data[[species_field]])) %>%
    mutate(platform = as.character(.data[[platform_field]]),
           taxa = factor(.data[[taxa_field]]),
           species = factor(.data[[species_field]]))

  # Collapse non-top platforms
  top_platforms <- dat %>%
    count(platform, sort = TRUE) %>%
    slice_head(n = top_n_platforms) %>%
    pull(platform)

  dat$platform <- ifelse(dat$platform %in% top_platforms, dat$platform, "Other")
  dat$platform <- factor(dat$platform)

  # ECO-SOLVE color palette
  base_palette <- c("#008D69", "#092E25", "#9E3B06", "#E4B001", "#DB002A", "#D9A3A6", "#E5EEE9")
  generate_shades <- function(hex) {
    c(
      dark = darken(hex, amount = 0.1),
      mid = hex,
      light = lighten(hex, amount = 0.2)
    )
  }
  shade_palette <- lapply(base_palette, generate_shades)

  # Facebook locked colors
  facebook_shades <- c(
    dark = "#0047CB",
    mid = "#1FCBFF",
    light = lighten("#1FCBFF", amount = 0.2)
  )

  all_platforms <- levels(dat$platform)
  other_platforms <- setdiff(all_platforms, "Facebook")
  color_assignments <- rep(seq_along(shade_palette), length.out = length(other_platforms))
  names(color_assignments) <- other_platforms

  platform_shades <- setNames(vector("list", length(all_platforms)), all_platforms)
  platform_shades[["Facebook"]] <- facebook_shades

  for (plat in other_platforms) {
    pal_index <- color_assignments[[plat]]
    platform_shades[[plat]] <- shade_palette[[pal_index]]
  }

  # Build levels
  platforms <- dat %>%
    group_by(platform) %>%
    summarise(value = n(), .groups = "drop") %>%
    mutate(
      id = platform,
      label = platform,
      parent = "",
      color = map_chr(platform, ~ platform_shades[[.x]]["dark"])
    )

  taxa <- dat %>%
    group_by(platform, taxa) %>%
    summarise(value = n(), .groups = "drop") %>%
    mutate(
      id = paste(platform, taxa, sep = "-"),
      label = taxa,
      parent = platform,
      color = map_chr(platform, ~ platform_shades[[.x]]["mid"])
    )

  species <- dat %>%
    group_by(platform, taxa, species) %>%
    summarise(value = n(), .groups = "drop") %>%
    mutate(
      id = paste(platform, taxa, species, sep = "-"),
      label = species,
      parent = paste(platform, taxa, sep = "-"),
      color = map_chr(platform, ~ platform_shades[[.x]]["light"])
    )

  sunburst_data <- bind_rows(platforms, taxa, species)

  # Build sunburst
  p <- plot_ly(
    data = sunburst_data,
    ids = ~id,
    labels = ~label,
    parents = ~parent,
    values = ~value,
    type = 'sunburst',
    branchvalues = 'total',
    marker = list(colors = sunburst_data$color)
  )

  if (!is.null(output_file)) {
    htmlwidgets::saveWidget(p, file = output_file, selfcontained = TRUE)
  }

  if (!is.null(png_file)) {
    if (!requireNamespace("webshot", quietly = TRUE)) {
      stop("The 'webshot' package is required to save PNG output. Please install it.")
    }
    webshot::webshot(output_file, file = png_file, vwidth = 900, vheight = 900, zoom = 2)
  }

  return(p)
}
