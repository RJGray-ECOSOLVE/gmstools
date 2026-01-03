#' Setup gmstools dependencies (install missing packages)
#'
#' @description
#' Installs/validates the packages needed to run gmstools apps on a fresh machine.
#' Handles CRAN dependencies in a robust way (including vctrs/rlang/cli),
#' and installs non-CRAN packages such as rgeoboundaries via devtools::install_github().
#'
#' @param scope Character. One of "hub", "apps", "all".
#'   - "hub": minimal dependencies to launch gms_app_hub only.
#'   - "apps": dependencies required by the internal Shiny / flexdashboard apps.
#'   - "all": everything.
#' @param update Logical. If TRUE, attempts to update installed CRAN packages.
#' @param ask Logical. Passed to update.packages() and install.packages().
#' @param repos Character. CRAN repos. If NULL/empty, a default CRAN mirror is set.
#' @param quiet Logical. Reduce messages.
#'
#' @return Invisibly returns list(scope, success, missing, failed_install).
#' @export
gms_setup <- function(scope  = c("all", "apps", "hub"),
                      update = FALSE,
                      ask    = FALSE,
                      repos  = getOption("repos"),
                      quiet  = FALSE) {

  scope <- match.arg(scope)

  # ---------------------------------------------------------------------------
  # Helper utilities
  # ---------------------------------------------------------------------------
  is_installed <- function(pkg) {
    requireNamespace(pkg, quietly = TRUE)
  }

  # Ensure we have a usable CRAN mirror for totally fresh R installs
  if (is.null(repos) || length(repos) == 0 ||
      all(is.na(repos)) ||
      isTRUE(repos["CRAN"] == "@CRAN@") ||
      is.na(repos["CRAN"])) {
    repos <- c(CRAN = "https://cloud.r-project.org")
    options(repos = repos)
    if (!quiet) message("CRAN repos not set; using default: ", repos["CRAN"])
  }

  # Safely install a single CRAN package; never stop the whole script
  safe_install_one <- function(pkg) {
    if (is_installed(pkg)) return(TRUE)
    if (!quiet) message("Installing CRAN package: ", pkg)

    ok <- tryCatch(
      {
        install.packages(
          pkg,
          dependencies = TRUE,
          ask          = ask,
          repos        = repos,
          quiet        = quiet
        )
        TRUE
      },
      error = function(e) {
        warning(
          "Failed to install ", pkg,
          " (non-zero exit status likely). Reason: ",
          conditionMessage(e),
          call. = FALSE
        )
        FALSE
      }
    )
    ok
  }

  # Safely install a group of CRAN packages
  safe_install_cran_group <- function(pkgs) {
    pkgs <- unique(pkgs)
    pkgs <- pkgs[!vapply(pkgs, is_installed, logical(1))]
    if (!length(pkgs)) return(character(0))

    failed <- character(0)
    for (p in pkgs) {
      if (!safe_install_one(p)) {
        failed <- c(failed, p)
      }
    }
    unique(failed)
  }

  # ---------------------------------------------------------------------------
  # Dependency lists
  # ---------------------------------------------------------------------------

  # Minimal hub deps (process spawning + UI)
  deps_hub <- c(
    "shiny",
    "bslib",
    "shinyjs",
    "httpuv",
    "processx",
    "tibble",
    "htmltools"
  )

  # Common app deps (Shiny + flexdashboard + wrangling + viz + GIS + export)
  # Based on all gmstools apps we’ve built: intel dashboard, search router,
  # network app V2, data model builder, admin_panel, hash_overlap_hub, etc.
  deps_apps_core <- c(
    # Rmd / flexdashboard toolchain
    "rmarkdown", "knitr", "flexdashboard", "htmlwidgets",

    # Shiny + widgets
    "shiny", "bslib", "shinyjs", "DT", "shinyWidgets",
    "shinycssloaders",

    # Wrangling / tidy
    "dplyr", "tidyr", "purrr", "tibble", "stringr", "readr", "readxl",
    "glue", "lubridate",

    # Web / parsing / IO
    "rvest", "curl", "httr", "jsonlite", "zip",

    # Hashing / crypto utilities (for shared hash DB apps)
    "digest",

    # Google Sheets backend for shared hash DB
    "googlesheets4",

    # Viz
    "ggplot2", "scales", "RColorBrewer", "echarts4r",

    # Networks
    "igraph", "visNetwork",

    # GIS
    "sf", "leaflet", "leaflet.extras",
    "rnaturalearth", "rnaturalearthdata",
    "geodata", "tidygeocoder",

    # Export
    "writexl",

    # Config / misc
    "yaml", "forcats"
  )

  # Low-level core deps that often break first (compile issues / non-zero exit)
  # We install these early so their failures are clearly reported but do not
  # prevent installing the rest.
  deps_low_level <- c(
    "vctrs",
    "rlang",
    "cli",
    "pillar",
    "lifecycle"
  )

  deps <- switch(
    scope,
    hub  = unique(c(deps_low_level, deps_hub)),
    apps = unique(c(deps_low_level, deps_hub, deps_apps_core)),
    all  = unique(c(deps_low_level, deps_hub, deps_apps_core))
  )

  # ---------------------------------------------------------------------------
  # 1) Install low-level core deps first (vctrs, rlang, cli, etc.)
  # ---------------------------------------------------------------------------
  failed_low <- safe_install_cran_group(deps_low_level)

  # ---------------------------------------------------------------------------
  # 2) Install the rest of the CRAN dependencies
  # ---------------------------------------------------------------------------
  remaining_deps <- setdiff(deps, deps_low_level)
  failed_main <- safe_install_cran_group(remaining_deps)

  # ---------------------------------------------------------------------------
  # 3) Ensure devtools for GitHub installs
  # ---------------------------------------------------------------------------
  if (!is_installed("devtools")) {
    if (!quiet) message("Installing devtools (needed for GitHub installs)...")
    safe_install_cran_group("devtools")
  }

  # Optionally remotes (sometimes lighter than full devtools)
  if (!is_installed("remotes")) {
    safe_install_cran_group("remotes")
  }

  # ---------------------------------------------------------------------------
  # 4) Install non-CRAN deps via GitHub (rgeoboundaries, etc.)
  # ---------------------------------------------------------------------------
  failed_github <- character(0)

  # rgeoboundaries from GitHub to avoid CRAN/version issues
  if (!is_installed("rgeoboundaries")) {
    if (!quiet) message("Installing rgeoboundaries from GitHub: wmgeolab/rgeoboundaries")
    ok_rg <- tryCatch(
      {
        # prefer devtools, fall back to remotes if needed
        if (is_installed("devtools")) {
          devtools::install_github("wmgeolab/rgeoboundaries", quiet = quiet)
        } else if (is_installed("remotes")) {
          remotes::install_github("wmgeolab/rgeoboundaries", quiet = quiet)
        } else {
          stop("Neither devtools nor remotes is available.")
        }
        TRUE
      },
      error = function(e) {
        warning(
          "Failed to install rgeoboundaries from GitHub.\n",
          "This is usually a system dependency/build-tools problem (often sf/GDAL/GEOS/PROJ).\n",
          "Original error: ", conditionMessage(e),
          call. = FALSE
        )
        FALSE
      }
    )
    if (!ok_rg) failed_github <- c(failed_github, "rgeoboundaries")
  }

  # ---------------------------------------------------------------------------
  # 5) Optional CRAN updates
  # ---------------------------------------------------------------------------
  if (isTRUE(update)) {
    if (!quiet) message("Updating installed CRAN packages...")
    try(
      utils::update.packages(
        ask  = ask,
        repos = repos,
        quiet = quiet
      ),
      silent = TRUE
    )
  }

  # ---------------------------------------------------------------------------
  # 6) Final validation and summary
  # ---------------------------------------------------------------------------
  final_missing <- deps[!vapply(deps, is_installed, logical(1))]
  if (!is_installed("rgeoboundaries")) {
    final_missing <- unique(c(final_missing, "rgeoboundaries"))
  }

  failed_install <- unique(c(failed_low, failed_main, failed_github))
  ok <- length(final_missing) == 0

  if (!ok) {
    warning(
      "gmstools setup incomplete.\n",
      "Missing or failed packages:\n  ",
      paste(final_missing, collapse = ", "),
      "\n\nIf sf/vctrs/low-level packages are missing, you likely need system\n",
      "libraries and build tools (e.g. Rtools on Windows or sf deps on Linux).\n",
      call. = FALSE
    )
  } else {
    if (!quiet) message("gmstools setup complete. All required packages are installed.")
  }

  invisible(list(
    scope          = scope,
    success        = ok,
    missing        = final_missing,
    failed_install = failed_install
  ))
}
