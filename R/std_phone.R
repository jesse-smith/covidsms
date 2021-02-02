#' Standardize Phone Numbers
#'
#' `std_phone()` standardizes North American phone numbers to a 10-digit
#' national format. Invalid or non-North American numbers are converted to `NA`.
#'
#' @param x A vector convertible to `character` format containing phone numbers
#'
#' @return A `character` vector of standardized phone numbers
#'
#' @export
std_phone <- function(x) {

  extraction_pattern <- "^([+]?0?1)?[0-9]{10}"
  removal_pattern <- "^([+]?0?1)(?=[0-9]{10})"

  x %>%
    as.character() %>%
    stringr::str_remove_all(pattern = "[^0-9+]") %>%
    stringr::str_extract(pattern = extraction_pattern) %>%
    stringr::str_remove(pattern = removal_pattern)
}
