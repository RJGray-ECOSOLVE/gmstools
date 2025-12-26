#' Update gmstools from GitHub
#'
#' Convenience wrapper so you can just run:
#'   gmstools::gmstools_update()
#'
#' This will install the latest version of gmstools from
#'   RJGray-ECOSOLVE/gmstools
#' using remotes (preferred) or devtools if remotes is not available.
#'
#' @param ref Git ref to install (branch, tag, or commit). Default "main".
#' @param ... Additional arguments passed to remotes::install_github() /
#'   devtools::install_github().
#'
#' @export
gmstools_update <- function(ref = "main", ...) {
  repo <- "RJGray-ECOSOLVE/gmstools"

  # Helper to ensure a package is installed
  ensure_pkg <- function(pkg) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      install.packages(pkg, dependencies = TRUE)
    }
  }

  # Prefer remotes, fall back to devtools
  if (!requireNamespace("remotes", quietly = TRUE) &&
      !requireNamespace("devtools", quietly = TRUE)) {
    # Neither installed: install remotes
    ensure_pkg("remotes")
  }

  message("Updating gmstools from GitHub (", repo, "@", ref, ") ...")

  if (requireNamespace("remotes", quietly = TRUE)) {
    remotes::install_github(
      repo,
      ref     = ref,
      upgrade = "never",
      ...
    )
  } else if (requireNamespace("devtools", quietly = TRUE)) {
    devtools::install_github(
      repo,
      ref = ref,
      ...
    )
  } else {
    stop(
      "Neither 'remotes' nor 'devtools' is available and could not be installed.\n",
      "Please install one of them manually and try again."
    )
  }

  message("gmstools update complete. You may need to restart R for everything to pick up cleanly.")
}
