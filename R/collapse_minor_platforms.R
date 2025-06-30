#' Collapse Minor Platforms into "Other"
#'
#' Reclassifies all but the top N platforms into "Other", for simplified reporting and visualization.
#'
#' @param data A cleaned GMS data frame.
#' @param platform_field Character. The name of the platform column (e.g., "platform_name").
#' @param n Integer. Number of top platforms to keep.
#'
#' @return A data frame with the modified platform field.
#' @export
#'
#' @examples
#' dat <- clean_gms_data(raw)
#' dat <- collapse_minor_platforms(dat, platform_field = "platform_name", n = 3)
collapse_minor_platforms <- function(data, platform_field = "platform_name", n = 3) {
  library(dplyr)

  if (!platform_field %in% names(data)) {
    stop("The specified platform field does not exist in the data.")
  }

  platform_vec <- data[[platform_field]]
  top_platforms <- names(sort(table(platform_vec), decreasing = TRUE))[1:n]

  data[[platform_field]] <- ifelse(platform_vec %in% top_platforms, platform_vec, "Other")
  data[[platform_field]] <- factor(data[[platform_field]])

  return(data)
}
