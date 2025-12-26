# In your gmstools package, put this in R/run_intel_dashboard.R
# And put your flexdashboard Rmd in inst/intel_dashboard/Intel_App.Rmd

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
