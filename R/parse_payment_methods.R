# Dependencies: dplyr, stringr, tidyr (tidyverse)
parse_payment_methods <- function(x) {
  stopifnot(is.atomic(x) || is.list(x))
  # convert to character, keep length
  x <- as.character(x)
  
  # 1) Normalize whole-cell entries first (so blanks/NA become 'Unspecified')
  x_norm <- x
  x_norm[is.na(x_norm)] <- ""
  x_trim <- stringr::str_squish(x_norm)
  
  # cells that are effectively empty/NA-ish -> label as 'Unspecified'
  empty_like <- stringr::str_detect(stringr::str_to_lower(x_trim), "^(|na|n|none|null)$")
  x_trim[empty_like] <- "Unspecified"
  
  # 2) Bring parenthetical brands out as tokens, unify separators, and split
  #    e.g., "Digital payment (DANA, OVO)" -> "Digital payment, DANA, OVO"
  x_flat <- stringr::str_replace_all(x_trim, "\\(([^)]*)\\)", ", \\1")
  # turn " and " & slashes into commas
  x_flat <- stringr::str_replace_all(x_flat, "\\band\\b", ",")
  x_flat <- stringr::str_replace_all(x_flat, "[/;|]", ",")
  # collapse multiple commas
  x_flat <- stringr::str_replace_all(x_flat, ",\\s*,+", ",")
  
  # split into tokens
  toks <- unlist(strsplit(x_flat, ",", fixed = TRUE), use.names = FALSE)
  toks <- stringr::str_squish(toks)
  toks <- toks[toks != ""]                       # drop empty tokens
  
  # 3) Standardize tokens
  tl <- stringr::str_to_lower(toks)
  
  # drop numeric-only junk (e.g., "1","4","5")
  keep <- !stringr::str_detect(tl, "^[0-9]+$")
  tl <- tl[keep]
  
  std <- dplyr::case_when(
    # specific brands FIRST (avoid 'cash' matching 'cash app', etc.)
    stringr::str_detect(tl, "cash\\s*app") ~ "Cash App",
    stringr::str_detect(tl, "paypal") ~ "PayPal",
    stringr::str_detect(tl, "western\\s*union") ~ "Western Union",
    stringr::str_detect(tl, "google\\s*pay|\\bgpay\\b|g\\s*pay") ~ "Google Pay",
    
    # explicit rails
    stringr::str_detect(tl, "\\bupi\\b") ~ "UPI",
    stringr::str_detect(tl, "escrow") ~ "Escrow",
    stringr::str_detect(tl, "\\bonline( payment)?\\b") ~ "Online Payment",
    stringr::str_detect(tl, "\\b(card|credit)\\b|\\bcards\\b") ~ "Card",
    stringr::str_detect(tl, "bank|transfer|\\beft\\b|netbanking|wire") ~ "Bank Transfer",
    stringr::str_detect(tl, "wallet|dana|ovo|gopay|paytm|m[- ]?pesa|momo") ~ "Digital Wallet",
    
    # cash and COD variants (after Cash App)
    stringr::str_detect(tl, "cash|cash on delivery|\\bcod\\b|payment on delivery") ~ "Cash",
    
    # unspecified/bucket
    stringr::str_detect(tl, "^unspecified$|^unknown$") ~ "Unspecified",
    
    TRUE ~ NA_character_  # anything else = drop as noise
  )
  
  std <- std[!is.na(std)]
  if (length(std) == 0L) {
    return(dplyr::tibble(payment_method = character(0), n = integer(0), pct = numeric(0)))
  }
  
  # 4) Frequency table
  dplyr::tibble(payment_method = std) |>
    dplyr::count(payment_method, name = "n") |>
    dplyr::arrange(dplyr::desc(n)) |>
    dplyr::mutate(pct = n / sum(n)*100)
}

# ---------------- Example ----------------
# freq_tbl <- parse_payment_methods(dat$payment_method)
# freq_tbl
