#' State Abbreviations and FIPS Codes
#'
#' `states` contains two letter state abbreviations and the matching FIPS code
#' for each state.
#'
#' @format A `tibble` with 57 rows and 2 columns:
#' \describe{
#'   \item{state}{Character. Two-letter abbreviations for each state.}
#'   \item{fips}{Character. Two-digit FIPS code for each state.}
#' }
#'
#' @source This is a subset of the
#'   \href{https://walker-data.com/tidycensus/reference/fips_codes.html}{fips_codes}
#'   dataset in the tidycensus package
"states"

# Suppress "no visible binding for global variable" when using `states`
if (getRversion() >= "2.15.1") utils::globalVariables("states")
