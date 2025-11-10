#' DMS weekly audit by Data Hub
#'
#' Aggregates cleaned DMS records to ISO weeks (Mon-start), returns a faceted
#' weekly bar plot and a wide summary table (weeks × hubs + Total).
#'
#' @param dat A data.frame/tibble already cleaned by \code{clean_gms_data()} (or equivalent).
#' @param date_col Character. Name of the date column (default "record_date").
#' @param hub_col Character or NULL. If NULL, tries c("datahub","data_hub") in that order.
#' @param week_start Integer 1–7. Week start day (1 = Monday). Default 1.
#' @param fill_missing_weeks Logical. Fill absent hub-week combos with 0s. Default TRUE.
#' @param free_y Logical. Use free y-scale per facet. Default TRUE.
#' @param flip Logical. Flip coordinates. Default TRUE.
#'
#' @return list(plot = ggplot, table = tibble)
#'
#' @importFrom dplyr %>% mutate coalesce if_else count distinct arrange select
#' @importFrom dplyr left_join across where filter rename
#' @importFrom tidyr crossing pivot_wider replace_na
#' @importFrom tibble tibble
#' @importFrom lubridate as_date floor_date
#' @importFrom ggplot2 ggplot aes geom_col facet_wrap scale_x_date labs theme_minimal
#' @importFrom ggplot2 theme element_blank element_text coord_flip margin
#' @importFrom rlang .data
#' @export
dms_audit <- function(dat,
                      date_col = "record_date",
                      hub_col  = NULL,
                      week_start = 1,
                      fill_missing_weeks = TRUE,
                      free_y = TRUE,
                      flip = TRUE) {
  
  # -- resolve hub column --
  if (is.null(hub_col)) {
    if ("datahub" %in% names(dat)) {
      hub_col <- "datahub"
    } else if ("data_hub" %in% names(dat)) {
      hub_col <- "data_hub"
    } else {
      stop("dms_audit(): Provide `hub_col` or include `datahub`/`data_hub` in data.")
    }
  }
  if (!date_col %in% names(dat)) {
    stop(sprintf("dms_audit(): date_col '%s' not found.", date_col))
  }
  
  # -- normalize fields (coerce to character before coalesce) --
  dat <- dat %>%
    dplyr::mutate(
      !!hub_col  := dplyr::coalesce(as.character(.data[[hub_col]]), ""),
      !!hub_col  := dplyr::if_else(.data[[hub_col]] == "", "Unknown", .data[[hub_col]]),
      !!date_col := lubridate::as_date(.data[[date_col]])
    )
  
  if (all(is.na(dat[[date_col]]))) {
    empty_tbl  <- tibble::tibble(week_start = as.Date(character()))
    empty_plot <- ggplot2::ggplot() + ggplot2::labs(title = "No valid dates in input data")
    return(list(plot = empty_plot, table = empty_tbl))
  }
  
  # -- weekly aggregation (Mon-start weeks) --
  dat_weekly <- dat %>%
    dplyr::mutate(week_start = lubridate::floor_date(.data[[date_col]], unit = "week", week_start = week_start)) %>%
    dplyr::filter(!is.na(week_start)) %>%
    dplyr::count(!!rlang::sym(hub_col), week_start, name = "n") %>%
    dplyr::rename(datahub = !!rlang::sym(hub_col))
  
  if (nrow(dat_weekly) == 0) {
    empty_tbl  <- tibble::tibble(week_start = as.Date(character()))
    empty_plot <- ggplot2::ggplot() + ggplot2::labs(title = "No weekly records to plot")
    return(list(plot = empty_plot, table = empty_tbl))
  }
  
  # -- fill missing hub-week combos (optional) --
  if (isTRUE(fill_missing_weeks)) {
    all_weeks <- tibble::tibble(
      week_start = seq(min(dat_weekly$week_start), max(dat_weekly$week_start), by = "1 week")
    )
    hubs <- dplyr::distinct(dat_weekly, datahub)
    dat_weekly_complete <- hubs %>%
      tidyr::crossing(all_weeks) %>%
      dplyr::left_join(dat_weekly, by = c("datahub", "week_start")) %>%
      dplyr::mutate(n = tidyr::replace_na(n, 0L)) %>%
      dplyr::arrange(week_start, datahub)
  } else {
    dat_weekly_complete <- dat_weekly %>% dplyr::arrange(week_start, datahub)
  }
  
  # -- plot (use margin(), avoid .data in tidyselect contexts) --
  p <- ggplot2::ggplot(dat_weekly_complete, ggplot2::aes(x = week_start, y = n)) +
    ggplot2::geom_col() +
    ggplot2::facet_wrap(~ datahub, scales = if (free_y) "free_y" else "fixed") +
    ggplot2::scale_x_date(date_breaks = "1 month", date_labels = "%b %Y",
                          expand = ggplot2::expansion(mult = c(0.01, 0.01))) +
    ggplot2::labs(
      title = "Weekly record entries by Data Hub",
      x = "Week starting (Mon)",
      y = "Records per week"
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      axis.title.x = ggplot2::element_text(margin = ggplot2::margin(t = 6)),
      axis.title.y = ggplot2::element_text(margin = ggplot2::margin(r = 6))
    )
  
  if (isTRUE(flip)) p <- p + ggplot2::coord_flip()
  
  # -- summary table (avoid .data in tidyselect verbs) --
  weekly_table <- dat_weekly_complete %>%
    dplyr::select(week_start, datahub, n) %>%
    tidyr::pivot_wider(names_from = datahub, values_from = n, values_fill = 0) %>%
    dplyr::arrange(week_start) %>%
    dplyr::mutate(Total = rowSums(dplyr::across(where(is.numeric))))
  
  list(plot = p, table = weekly_table)
}
