#' Setup gmstools dependencies (install missing packages)
#'
#' @description
#' Installs/validates the packages needed to run gmstools apps on a fresh machine.
#' Includes GitHub installation for rgeoboundaries to avoid CRAN/version issues.
#'
#' @param scope Character. One of "hub", "apps", "all".
#' @param update Logical. If TRUE, attempts to update installed packages (CRAN).
#' @param ask Logical. Passed to install.packages().
#' @param repos Character. CRAN repos.
#' @param quiet Logical. Reduce messages.
#'
#' @return Invisibly returns list(success, missing, scope).
#' @export
gms_setup <- function(scope = c("all", "apps", "hub"),
                      update = FALSE,
                      ask = FALSE,
                      repos = getOption("repos"),
                      quiet = FALSE) {

  scope <- match.arg(scope)

  is_installed <- function(pkg) requireNamespace(pkg, quietly = TRUE)

  install_cran <- function(pkgs) {
    pkgs <- unique(pkgs)
    pkgs <- pkgs[!vapply(pkgs, is_installed, logical(1))]
    if (length(pkgs) == 0) return(invisible(TRUE))
    if (!quiet) message("Installing CRAN packages: ", paste(pkgs, collapse = ", "))
    install.packages(pkgs, dependencies = TRUE, ask = ask, repos = repos)
    invisible(TRUE)
  }

  if (is.null(repos) || length(repos) == 0 || any(is.na(repos))) {
    stop("CRAN repos not set. Example:\noptions(repos = c(CRAN='https://cloud.r-project.org'))", call. = FALSE)
  }

  # Minimal hub deps (process spawning + UI)
  deps_hub <- c(
    "shiny", "bslib", "shinyjs", "httpuv", "processx",
    "tibble", "htmltools"
  )

  # Common app deps (Shiny + Flexdashboard + data + GIS + export)
  deps_apps <- c(
    # Rmd / Flexdashboard toolchain (needed if you render or run Rmd dashboards)
    "rmarkdown", "knitr", "flexdashboard", "htmlwidgets",

    # Shiny + widgets
    "shiny", "bslib", "shinyjs", "DT", "shinyWidgets", "shinycssloaders", "htmltools",

    # Wrangling
    "dplyr", "tidyr", "purrr", "tibble", "stringr", "readr", "glue", "lubridate",

    # Web / parsing / IO
    "rvest", "curl", "httr", "jsonlite", "zip",

    # Viz
    "plotly", "scales", "RColorBrewer",

    # Networks
    "igraph", "visNetwork",

    # GIS
    "sf", "leaflet", "leaflet.extras",
    "rnaturalearth", "rnaturalearthdata",
    "geodata", "tidygeocoder",

    # Export
    "writexl"
  )

  deps <- switch(
    scope,
    hub  = deps_hub,
    apps = unique(c(deps_hub, deps_apps)),
    all  = unique(c(deps_hub, deps_apps))
  )

  # 1) Install CRAN deps
  install_cran(deps)

  # 2) Ensure devtools (as you requested) + optional remotes
  if (!is_installed("devtools")) {
    if (!quiet) message("Installing devtools (needed for rgeoboundaries GitHub install)...")
    install.packages("devtools", dependencies = TRUE, ask = ask, repos = repos)
  }
  if (!is_installed("remotes")) {
    install.packages("remotes", dependencies = TRUE, ask = ask, repos = repos)
  }

  # 3) Install rgeoboundaries from GitHub (exactly as you specified)
  if (!is_installed("rgeoboundaries")) {
    if (!quiet) message("Installing rgeoboundaries from GitHub: wmgeolab/rgeoboundaries")
    tryCatch(
      {
        require("devtools")
        devtools::install_github("wmgeolab/rgeoboundaries", quiet = quiet)
      },
      error = function(e) {
        stop(
          "Failed to install rgeoboundaries from GitHub.\n",
          "This is usually a system dependency/build-tools problem (often triggered by sf/GDAL/GEOS/PROJ).\n\n",
          "Original error:\n", conditionMessage(e),
          call. = FALSE
        )
      }
    )
  }

  # 4) Optional updates
  if (isTRUE(update)) {
    if (!quiet) message("Updating installed CRAN packages...")
    try(utils::update.packages(ask = ask, repos = repos), silent = TRUE)
  }

  # 5) Final validation
  final_missing <- deps[!vapply(deps, is_installed, logical(1))]
  if (!is_installed("rgeoboundaries")) final_missing <- unique(c(final_missing, "rgeoboundaries"))
  ok <- length(final_missing) == 0

  if (!ok) {
    warning(
      "gmstools setup incomplete. Missing packages:\n",
      paste(final_missing, collapse = ", "),
      "\nIf sf-related packages are missing, you likely need system libraries/build tools.",
      call. = FALSE
    )
  } else {
    if (!quiet) message("gmstools setup complete.")
  }

  invisible(list(scope = scope, success = ok, missing = final_missing))
}
