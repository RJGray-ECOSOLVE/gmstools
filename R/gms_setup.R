#' Setup gmstools dependencies (install missing packages)
#'
#' @description
#' Ensures required R packages for gmstools apps are installed on a new machine.
#' Designed to be run once before launching the GMS App Hub or any child app.
#'
#' @param scope Character. One of:
#'   - "hub" : minimal dependencies to run the hub UI and spawn child apps
#'   - "apps": dependencies for the common gmstools Shiny/Flexdashboard apps
#'   - "all" : union of all known dependencies (recommended for new machines)
#' @param update Logical. If TRUE, attempts to update installed packages.
#' @param ask Logical. Passed to install.packages(). If FALSE, installs without prompts.
#' @param repos Character. CRAN repo; defaults to getOption("repos").
#' @param quiet Logical. If TRUE, reduces messages.
#'
#' @return Invisibly returns a list with installed packages and any failures.
#' @export
gms_setup <- function(scope = c("all", "apps", "hub"),
                      update = FALSE,
                      ask = FALSE,
                      repos = getOption("repos"),
                      quiet = FALSE) {
  
  scope <- match.arg(scope)
  
  # ---- Define dependencies by scope ----
  # Keep these explicit so it's deterministic across machines.
  deps_hub <- c(
    "shiny", "bslib", "shinyjs", "httpuv", "processx",
    "tibble"  # used in hub registry
  )
  
  # Common dependencies across the gmstools apps you've been running.
  # Add to this list as you add more app modules.
  deps_apps <- c(
    # Shiny UI + widgets
    "shiny", "bslib", "shinyjs", "DT", "shinyWidgets", "shinycssloaders", "htmltools",
    
    # Data wrangling / IO
    "dplyr", "tidyr", "purrr", "tibble", "stringr", "readr", "glue",
    
    # Web + parsing
    "rvest", "curl", "httr", "jsonlite",
    
    # Mapping / GIS
    "sf", "leaflet", "leaflet.extras", "rnaturalearth", "rnaturalearthdata",
    "geodata", "tidygeocoder", "rgeoboundaries",
    
    # Viz + dashboards
    "flexdashboard", "plotly", "visNetwork", "igraph", "RColorBrewer", "scales",
    
    # Export
    "writexl",
    
    # Hub child-process helper
    "httpuv", "processx"
  )
  
  deps_all <- unique(c(deps_hub, deps_apps))
  
  deps <- switch(
    scope,
    hub  = deps_hub,
    apps = unique(c(deps_hub, deps_apps)),
    all  = deps_all
  )
  
  # ---- Helpers ----
  is_installed <- function(pkg) requireNamespace(pkg, quietly = TRUE)
  
  install_cran <- function(pkgs) {
    if (length(pkgs) == 0) return(invisible(TRUE))
    if (!quiet) message("Installing missing CRAN packages: ", paste(pkgs, collapse = ", "))
    install.packages(pkgs, dependencies = TRUE, ask = ask, repos = repos)
  }
  
  update_cran <- function() {
    if (!quiet) message("Updating installed packages (CRAN)...")
    try(utils::update.packages(ask = ask, repos = repos), silent = TRUE)
  }
  
  # ---- Basic sanity check on repos ----
  if (is.null(repos) || length(repos) == 0 || any(is.na(repos))) {
    stop("No CRAN repos are set. Set options(repos=...) and retry.", call. = FALSE)
  }
  
  # ---- Compute missing packages ----
  missing <- deps[!vapply(deps, is_installed, logical(1))]
  
  # ---- Install missing ----
  if (length(missing) > 0) {
    install_cran(missing)
  } else {
    if (!quiet) message("All required packages already installed for scope='", scope, "'.")
  }
  
  # ---- Optional update ----
  if (isTRUE(update)) {
    update_cran()
  }
  
  # ---- Final validation ----
  still_missing <- deps[!vapply(deps, is_installed, logical(1))]
  ok <- length(still_missing) == 0
  
  if (!ok) {
    msg <- paste0(
      "Setup incomplete: still missing packages: ",
      paste(still_missing, collapse = ", "),
      "\nThis is usually due to system dependencies (e.g., GDAL/GEOS for sf) or build tools.",
      "\nTry installing system prerequisites then rerun gms_setup()."
    )
    warning(msg, call. = FALSE)
  } else {
    if (!quiet) message("gmstools setup complete.")
  }
  
  invisible(list(
    scope = scope,
    required = deps,
    installed_now = setdiff(missing, still_missing),
    still_missing = still_missing,
    success = ok
  ))
}