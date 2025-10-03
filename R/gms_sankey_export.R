#' Export a platform→species Sankey diagram (networkD3)
#'
#' @param data A data.frame with at least two columns: platform and species.
#' @param platform_col Column name for platform (default "platform_name").
#' @param species_col  Column name for species/common name (default "item_common_name").
#' @param top_n_platforms Integer or Inf. Keep top-N platforms by total links (default 8).
#' @param top_n_species   Integer or Inf. Keep top-N species by total links (default 20).
#' @param out_dir Output directory (created if it doesn't exist). Default ".".
#' @param file_stub Base file name without extension. Default "sankey".
#' @param formats Character vector in c("png","pdf"). Default c("png","pdf").
#' @param vwidth,vheight Pixel viewport size passed to webshot2 (defaults 1000×800).
#' @param sinks_right, font_size, node_width networkD3 layout options.
#' @param palette Optional vector of hex colors (one per platform). If NULL, Set2-based palette is generated.
#' @param recode_platform Optional named vector for exact-value recodes on platform, e.g. c("YouTube"="Youtube").
#' @param recode_species  Optional named vector for exact-value recodes on species,  e.g. c("Pangolin spp."="Pangolin").
#' @param save_html Logical; also save the interactive HTML widget (default TRUE).
#'
#' @return Invisibly, a list with `widget` (htmlwidget) and `files` (written paths).
#'
#' @examples
#' # Example: export full GMS Sankey with custom size
#' gms_sankey_export(
#'   gms_data,
#'   top_n_platforms = 20,
#'   top_n_species   = 20,
#'   out_dir   = "D:/user/folder",
#'   file_stub = "gms_full",
#'   formats  = c("png","pdf"),
#'   vwidth   = 400,
#'   vheight  = 600
#' )
#' 
gms_sankey_export <- function(
    data,
    platform_col      = "platform_name",
    species_col       = "item_common_name",
    top_n_platforms   = 8,
    top_n_species     = 20,
    out_dir           = ".",
    file_stub         = "sankey",
    formats           = c("png","pdf"),
    vwidth            = 1000,
    vheight           = 800,
    sinks_right       = TRUE,
    font_size         = 12,
    node_width        = 28,
    palette           = NULL,
    recode_platform   = NULL,
    recode_species    = NULL,
    save_html         = TRUE
) {
  # ---- checks ----
  req_pkgs <- c("dplyr","stringr","forcats","networkD3","RColorBrewer","jsonlite","htmlwidgets","webshot2")
  missing_pkgs <- req_pkgs[!vapply(req_pkgs, requireNamespace, quietly = TRUE, FUN.VALUE = logical(1))]
  if (length(missing_pkgs)) stop("Missing required packages: ", paste(missing_pkgs, collapse=", "), call. = FALSE)
  if (!all(formats %in% c("png","pdf"))) stop("formats must be in c('png','pdf')", call. = FALSE)
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  if (!all(c(platform_col, species_col) %in% names(data))) {
    stop("platform_col or species_col not found in data.", call. = FALSE)
  }
  
  # ---- small helper: exact recode with a named vector ----
  recode_named <- function(x, map) {
    if (is.null(map)) return(x)
    m <- match(x, names(map))
    out <- x
    out[!is.na(m)] <- unname(map[m[!is.na(m)]])
    out
  }
  
  # ---- prep columns & optional recodes ----
  df <- data
  df[[platform_col]] <- stringr::str_squish(df[[platform_col]])
  df[[species_col ]] <- stringr::str_squish(df[[species_col ]])
  df[[platform_col]] <- recode_named(df[[platform_col]], recode_platform)
  df[[species_col ]] <- recode_named(df[[species_col ]],  recode_species )
  
  # ---- aggregate counts ----
  df2 <- data.frame(
    platform = df[[platform_col]],
    species  = df[[species_col]],
    stringsAsFactors = FALSE
  )
  edges_raw <- df2 |>
    dplyr::filter(!is.na(platform), !is.na(species)) |>
    dplyr::count(platform, species, name = "value")
  
  if (nrow(edges_raw) == 0) stop("No (platform, species) pairs found after filtering.", call. = FALSE)
  
  # ---- top-N filters ----
  tmp_p <- edges_raw |>
    dplyr::group_by(platform) |>
    dplyr::summarise(total = sum(value), .groups = "drop") |>
    dplyr::arrange(dplyr::desc(total))
  if (is.finite(top_n_platforms)) tmp_p <- dplyr::slice_head(tmp_p, n = top_n_platforms)
  top_platforms <- tmp_p |> dplyr::pull(platform)
  
  tmp_s <- edges_raw |>
    dplyr::group_by(species) |>
    dplyr::summarise(total = sum(value), .groups = "drop") |> 
    dplyr::arrange(dplyr::desc(total))
  if (is.finite(top_n_species)) tmp_s <- dplyr::slice_head(tmp_s, n = top_n_species)
  top_species <- tmp_s |> dplyr::pull(species)
  
  edges <- edges_raw |>
    dplyr::filter(platform %in% top_platforms, species %in% top_species)
  if (nrow(edges) == 0) edges <- edges_raw
  
  # ---- order factors ----
  edges <- edges |>
    dplyr::mutate(
      platform = forcats::fct_reorder(platform, value, .fun = sum, .desc = TRUE),
      species  = forcats::fct_reorder(species,  value, .fun = sum, .desc = TRUE)
    )
  platforms <- levels(edges$platform)
  species   <- levels(edges$species)
  
  # ---- nodes/links ----
  nodes_tbl <- data.frame(name = c(platforms, species), stringsAsFactors = FALSE)
  links_tbl <- edges |>
    dplyr::mutate(
      source = as.integer(platform) - 1,
      target = as.integer(species)  - 1 + length(platforms),
      group  = as.character(platform)
    ) |>
    dplyr::select(source, target, value, group)
  
  # ---- colors ----
  if (is.null(palette)) {
    pal <- RColorBrewer::brewer.pal(n = max(3, min(8, length(platforms))), name = "Set2")
    if (length(platforms) > length(pal)) pal <- grDevices::colorRampPalette(pal)(length(platforms))
  } else {
    pal <- palette
    if (length(pal) < length(platforms)) {
      stop("Provided palette has fewer colors than platforms (", length(platforms), ").", call. = FALSE)
    }
  }
  names(pal) <- platforms
  domain_js <- jsonlite::toJSON(platforms, auto_unbox = TRUE)
  range_js  <- jsonlite::toJSON(unname(pal), auto_unbox = TRUE)
  color_js  <- paste0("d3.scaleOrdinal().domain(", domain_js, ").range(", range_js, ")")
  
  # ---- build widget ----
  widget <- networkD3::sankeyNetwork(
    Links = links_tbl,
    Nodes = nodes_tbl,
    Source = "source",
    Target = "target",
    Value  = "value",
    NodeID = "name",
    LinkGroup = "group",
    sinksRight = sinks_right,
    fontSize   = font_size,
    nodeWidth  = node_width,
    colourScale = color_js
  )
  
  # ---- export ----
  files_out <- character(0)
  html_path <- file.path(out_dir, paste0(file_stub, ".html"))
  if (isTRUE(save_html)) {
    htmlwidgets::saveWidget(widget, html_path, selfcontained = TRUE)
    files_out <- c(files_out, html_path)
  } else {
    html_path <- file.path(out_dir, paste0(file_stub, "_tmp.html"))
    htmlwidgets::saveWidget(widget, html_path, selfcontained = TRUE)
  }
  
  if ("png" %in% formats) {
    png_path <- file.path(out_dir, paste0(file_stub, ".png"))
    webshot2::webshot(html_path, file = png_path,
                      vwidth = vwidth, vheight = vheight, cliprect = "viewport")
    files_out <- c(files_out, png_path)
  }
  if ("pdf" %in% formats) {
    pdf_path <- file.path(out_dir, paste0(file_stub, ".pdf"))
    webshot2::webshot(html_path, file = pdf_path,
                      vwidth = vwidth, vheight = vheight, cliprect = "viewport")
    files_out <- c(files_out, pdf_path)
  }
  
  if (!save_html && file.exists(html_path)) unlink(html_path, force = TRUE)
  
  # show in Viewer too
  print(widget)
  
  invisible(list(widget = widget, files = files_out))
}
