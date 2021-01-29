#' Standardize Addresses
#'
#' `std_addr()` transliterates all characters to ASCII, replaces all symbols
#' except dashes (`-`) and apostrophes (`'`) with a space, removes apostrophes,
#' removes extraneous whitespace, and converts to title case.
#'
#' @param x Character-like vector. This will be converted to character before
#'   standardizing.
#'
#' @return A character vector with standardized addresses
#'
#' @export
std_addr <- function(x) {
  x %>%
    as.character() %>%
    stringi::stri_trans_general(id = "Any-Latin;Latin-ASCII") %>%
    stringr::str_replace_all("[^a-zA-Z0-9']", replacement = " ") %>%
    stringr::str_remove_all("[']+") %>%
    stringr::str_squish() %>%
    stringr::str_to_upper() %>%
    as_chr_ascii()
}

std_city <- function(string) {
  string %>%
    as.character() %>%
    stringi::stri_trans_general(id = "Any-Latin;Latin-ASCII") %>%
    stringr::str_replace_all("[^a-zA-Z0-9']", replacement = " ") %>%
    stringr::str_remove_all("[']+") %>%
    stringr::str_squish() %>%
    stringr::str_to_upper() %>%
    as_chr_ascii()
}

std_state <- function(string) {
  string %>%
    stringi::stri_trans_general(id = "Any-Latin;Latin-ASCII") %>%
    stringr::str_replace_all("[^a-zA-Z0-9']", replacement = " ") %>%
    stringr::str_remove_all("[']+") %>%
    stringr::str_squish() %>%
    dplyr::as_tibble() %>%
    dplyr::rename(fips = "value") %>%
    dplyr::left_join(states, by = "fips") %>%
    dplyr::transmute(s = dplyr::coalesce(.data[["state"]], .data[["fips"]])) %>%
    dplyr::pull(1L) %>%
    stringr::str_squish() %>%
    stringr::str_to_upper() %>%
    as_chr_ascii()
}

std_zip <- function(string) {
  string %>%
    stringi::stri_trans_general(id = "Any-Latin;Latin-ASCII") %>%
    stringr::str_extract("[0-9]{5}") %>%
    as_chr_ascii()
}
