#' Extract Keywords from Ad Text
#'
#' Tokenizes and extracts keywords from free-text fields such as item_title and item_text.
#' Returns either a long-format keyword table or appends a keyword list per ad.
#'
#' @param data A GMS data frame.
#' @param text_fields Character vector of text field names (e.g., c("item_title", "item_text")).
#' @param min_nchar Integer. Minimum character length of keywords (default 4).
#' @param output Character. "long" for long format (one row per keyword), or "wide" for list-column.
#'
#' @return Either a long-format data frame (id + keyword) or original data with keyword list-column.
#' @export
#'
#' @examples
#' keyword_df <- extract_keywords(dat, text_fields = c("item_title", "item_text"))
extract_keywords <- function(data,
                             text_fields = c("item_title", "item_text"),
                             min_nchar = 4,
                             output = c("long", "wide")) {
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(tibble)

  output <- match.arg(output)
  if (!all(text_fields %in% names(data))) {
    stop("One or more text fields not found in the dataset.")
  }

  # Create a unified text column
  data$text_combined <- apply(data[, text_fields], 1, function(x) {
    paste(na.omit(x), collapse = " ")
  })

  # Tokenize
  keyword_list <- str_to_lower(data$text_combined) %>%
    str_replace_all("[^a-zA-Z0-9\\s]", " ") %>%
    str_squish() %>%
    str_split("\\s+") %>%
    lapply(function(tokens) tokens[nchar(tokens) >= min_nchar])

  if (output == "wide") {
    data$keywords <- keyword_list
    return(select(data, -text_combined))
  }

  # Long format
  long_df <- tibble(id = seq_along(keyword_list), keyword = keyword_list) %>%
    unnest(keyword) %>%
    filter(!keyword %in% stopwords::stopwords("en")) %>%
    distinct()

  return(long_df)
}
