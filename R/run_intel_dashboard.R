#' Launch the Online IWT Intelligence Dashboard
#'
#' This will open the flexdashboard (Shiny runtime) in the user's default browser.
#' Make sure the Rmd is stored at:
#'   inst/intel_dashboard/Intel_App.Rmd
#' inside the gmstools package source.
#'
#' @param launch.browser Logical, open in system browser (default TRUE).
#' @param port Optional port to use for the Shiny app. If NULL, Shiny picks one.
#' @export
run_intel_dashboard <- function(launch.browser = TRUE, port = NULL) {

  required_packages <- c(
  "flexdashboard",
  "shiny",
  "shinyjs",
  "tidyverse",
  "leaflet",
  "plotly",
  "visNetwork",
  "DT",
  "stringr",
  "tidyr",
  "sf",
  "rnaturalearth",
  "rnaturalearthdata",
  "htmltools",
  "scales",
  "gmstools",
  "geodata",
  "tidygeocoder",
  "rgeoboundaries",
  "readr",
  "lubridate",
  "zip",
  "echarts4r" 
)

install_if_missing <- function(pkgs) {
  ip <- rownames(installed.packages())
  missing_pkgs <- pkgs[!pkgs %in% ip]
  if (length(missing_pkgs) > 0) {
    message("Installing missing packages: ", paste(missing_pkgs, collapse = ", "))
    install.packages(missing_pkgs, dependencies = TRUE)
  }
}

# If you ever want automatic updates as well, uncomment this and the call below.
update_if_desired <- function(pkgs) {
  op <- old.packages()
  if (is.null(op)) return(invisible())
  old_names <- rownames(op)
  to_update <- pkgs[pkgs %in% old_names]
  if (length(to_update) > 0) {
    message("Updating packages: ", paste(to_update, collapse = ", "))
    update.packages(ask = FALSE, oldPkgs = to_update)
  }
}

# Install any missing packages
install_if_missing(required_packages)

# Optionally update them (commented out by default)
# update_if_desired(required_packages)

  # Locate the Rmd file inside the installed package
  app_file <- system.file(
    "intel_dashboard",
    "IWT_Intel_App.Rmd",
    package = "gmstools"
  )
  
  if (app_file == "") {
    stop(
      "Could not find 'IWT_Intel_App.Rmd' inside the gmstools package.\n",
      "Make sure it is stored in inst/intel_dashboard/ before installing.",
      call. = FALSE
    )
  }
  
  # Build shiny_args for rmarkdown::run -> shiny::runApp
  shiny_args <- list(
    launch.browser = launch.browser
  )
  if (!is.null(port)) {
    shiny_args$port <- as.integer(port)
  }
  
  # Run the flexdashboard with Shiny runtime
  rmarkdown::run(
    file       = app_file,
    shiny_args = shiny_args
  )
}


