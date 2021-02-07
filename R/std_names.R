#' Re-Define `std_names()` to Remove Trailing Initials
#'
#' `std_names()` uses `coviData::std_names()` to parse `string`, then removes
#' trailing single letters preceded by whitespace.
#'
#' @param string A character vector
#'
#' @return A standardized character vector
#'
#' @export
std_names <- function(string) {
  string %>%
    coviData::std_names() %>%
    stringr::str_remove("(?<=[^\\s])\\s+[^\\s]$")
}
