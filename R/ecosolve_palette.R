#' ECO-SOLVE Color Palette
#'
#' Returns a custom color palette used by the Global Monitoring System (GMS).
#' Can be used directly in plots or passed to ggplot2 scales.
#'
#' @param n Number of colors to return (for interpolated palette). If NULL, returns full base palette.
#' @param type Either "discrete" or "continuous". Ignored if n is NULL.
#'
#' @return A character vector of hex color codes.
#'
#' @examples
#' # Get the full base palette
#' ecosolve_palette()
#'
#' # Get the first 5 discrete colors
#' ecosolve_palette(5, type = "discrete")
#'
#' # Get a continuous color gradient of 10 colors
#' ecosolve_palette(10, type = "continuous")
#'
#' # Use in ggplot2 (manual fill scale)
#' \dontrun{
#' library(ggplot2)
#' ggplot(mtcars, aes(factor(cyl), fill = factor(cyl))) +
#'   geom_bar() +
#'   scale_fill_manual(values = ecosolve_palette(3, type = "discrete"))
#' }
#'
#' @export
ecosolve_palette <- function(n = NULL, type = c("discrete", "continuous")) {
  base_palette <- c(
    "#008D69", "#092E25", # greens
    "#9E3B06", "#E4B001", # brown/yellow
    "#DB002A", "#D9A3A6", # red/pink
    "#E5EEE9"             # pale grey
  )

  if (is.null(n)) {
    return(base_palette)
  }

  type <- match.arg(type)

  if (type == "discrete") {
    if (n > length(base_palette)) {
      stop("Requested more colors than are available in the discrete palette.")
    }
    return(base_palette[1:n])
  }

  if (type == "continuous") {
    return(grDevices::colorRampPalette(base_palette)(n))
  }
}
