#' Anonymize seller, contact, and group fields for safe app demos
#'
#' Deterministically anonymizes key identifier fields so records remain linkable
#' (the same seller/group stays the same across all rows), while removing
#' identifying information in the app.
#'
#' - Seller names become "seller 1", "seller 2", ... in order of first appearance.
#' - Group names become "group 1", "group 2", ... in order of first appearance.
#' - Contact strings are masked to keep a short prefix (e.g., "+64") and replace
#'   the remainder with asterisks (or another mask character).
#'
#' This function is designed for demonstration / sharing of dashboards and
#' reports where identifiers must not be visible.
#'
#' @param df A data.frame (or tibble) containing the fields to anonymize.
#' @param seller_col Name of the seller column. Default: "item_seller_name".
#' @param contact_col Name of the seller contact column. Default: "item_seller_contact".
#' @param group_col Name of the group column. Default: "group_name".
#' @param seller_label Label prefix for anonymized sellers. Default: "seller ".
#' @param group_label Label prefix for anonymized groups. Default: "group ".
#' @param phone_keep_prefix Integer. Number of leading characters to keep in the
#'   contact field before masking the remainder. Default: 3 (e.g., "+64").
#' @param mask_char Single character used for masking. Default: "*".
#' @param mask_len Integer. Number of mask characters to show after the prefix.
#'   Default: 6, producing e.g., "+64 ******".
#' @param keep_original Logical. If TRUE, original values are stored as
#'   "<col>_raw" columns before anonymization. Default: FALSE.
#' @param overwrite Logical. If TRUE, overwrites the original columns. If FALSE,
#'   writes anonymized values into "<col>_anon" columns. Default: TRUE.
#' @param return_keymap Logical. If TRUE, returns a list with $data and $keymap
#'   mapping original seller/group values to anonymized labels. Default: FALSE.
#'
#' @return If return_keymap is FALSE, returns a data.frame with anonymized fields.
#' If return_keymap is TRUE, returns a list:
#' \describe{
#'   \item{data}{The anonymized data.frame}
#'   \item{keymap}{A data.frame mapping originals to anonymized labels (seller and group)}
#' }
#'
#' @examples
#' # Basic usage (overwrite columns)
#' # demo_df <- anonymize_demo_fields(df, phone_keep_prefix = 3)
#'
#' # Keep originals and also get a mapping table for sellers and groups
#' # res <- anonymize_demo_fields(df, keep_original = TRUE, return_keymap = TRUE)
#' # demo_df <- res$data
#' # keymap  <- res$keymap
#'
#' # Write to *_anon columns instead of overwriting
#' # demo_df <- anonymize_demo_fields(df, overwrite = FALSE)
#'
#' @export
anonymize_PII_fields <- function(
    df,
    seller_col  = "item_seller_name",
    contact_col = "item_seller_contact",
    group_col   = "group_name",
    seller_label = "seller ",
    group_label  = "group ",
    phone_keep_prefix = 3,
    mask_char = "*",
    mask_len = 6,
    keep_original = FALSE,
    overwrite = TRUE,
    return_keymap = FALSE
) {
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  
  # ---- helpers ----
  .as_chr <- function(x) if (is.factor(x)) as.character(x) else as.character(x)
  
  .trim_to_na <- function(x) {
    x <- .as_chr(x)
    x <- trimws(x)
    x[nchar(x) == 0] <- NA_character_
    x
  }
  
  .stable_id_map <- function(x, label) {
    # Map unique values to label + incremental id, stable by first appearance
    x <- .trim_to_na(x)
    out <- rep(NA_character_, length(x))
    ok <- !is.na(x)
    
    if (!any(ok)) {
      return(list(
        out = out,
        map = data.frame(original = character(0), anonymized = character(0), stringsAsFactors = FALSE)
      ))
    }
    
    u <- unique(x[ok])                 # preserves first appearance order
    ids <- seq_along(u)
    anon <- paste0(label, ids)
    map_vec <- setNames(anon, u)
    
    out[ok] <- unname(map_vec[x[ok]])
    
    list(
      out = out,
      map = data.frame(original = u, anonymized = anon, stringsAsFactors = FALSE)
    )
  }
  
  .mask_contact <- function(x, keep_prefix = 3, mask_char = "*", mask_len = 6) {
    x <- .trim_to_na(x)
    out <- x
    ok <- !is.na(x)
    if (!any(ok)) return(out)
    
    # Normalize whitespace (keeps the rest as-is; we are masking anyway)
    y <- gsub("\\s+", " ", x[ok])
    
    masked <- vapply(y, function(s) {
      s0 <- trimws(s)
      if (nchar(s0) <= keep_prefix) return(s0)
      
      prefix <- substr(s0, 1, keep_prefix)
      paste0(prefix, " ", paste(rep(mask_char, mask_len), collapse = ""))
    }, FUN.VALUE = character(1))
    
    out[ok] <- masked
    out
  }
  
  # ---- validate columns ----
  needed <- c(seller_col, contact_col, group_col)
  missing_cols <- needed[!needed %in% names(df)]
  if (length(missing_cols) > 0) {
    stop("Missing column(s) in df: ", paste(missing_cols, collapse = ", "))
  }
  
  # ---- optionally keep originals ----
  if (isTRUE(keep_original)) {
    for (cc in needed) {
      raw_name <- paste0(cc, "_raw")
      if (raw_name %in% names(df)) {
        stop("Column already exists: ", raw_name, ". Set keep_original=FALSE or rename/drop that column.")
      }
      df[[raw_name]] <- df[[cc]]
    }
  }
  
  # ---- build mappings ----
  seller_res <- .stable_id_map(df[[seller_col]], seller_label)
  group_res  <- .stable_id_map(df[[group_col]],  group_label)
  contact_anon <- .mask_contact(df[[contact_col]], keep_prefix = phone_keep_prefix, mask_char = mask_char, mask_len = mask_len)
  
  # ---- write results ----
  if (isTRUE(overwrite)) {
    df[[seller_col]]  <- seller_res$out
    df[[contact_col]] <- contact_anon
    df[[group_col]]   <- group_res$out
  } else {
    df[[paste0(seller_col, "_anon")]]  <- seller_res$out
    df[[paste0(contact_col, "_anon")]] <- contact_anon
    df[[paste0(group_col, "_anon")]]   <- group_res$out
  }
  
  # ---- optional keymap ----
  if (isTRUE(return_keymap)) {
    keymap <- rbind(
      data.frame(column = seller_col, seller_res$map, stringsAsFactors = FALSE),
      data.frame(column = group_col,  group_res$map,  stringsAsFactors = FALSE)
    )
    rownames(keymap) <- NULL
    return(list(data = df, keymap = keymap))
  }
  
  df
}
