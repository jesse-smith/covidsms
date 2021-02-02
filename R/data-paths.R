#' Paths to Data on COVID-19 ("V:") Drive
#'
#' @description
#' These variables contain paths to datasets housed on the Shelby County
#' COVID-19 ("V:") drive.
#'
#' `ltcf_addr_path` returns the path to an `fst` file containing parsed
#' address information for long-term care facilities
#'
#' `ltcf_phone_path` returns the path to an `fst` file containing parsed phone
#' numbers for long-term care facilities
#'
#' @name data-paths
#'
#' @aliases ltcf_addr_path ltcf_phone_path
NULL

#' @rdname data-paths
"ltcf_addr_path"

#' @rdname data-paths
"ltcf_phone_path"

# Suppress "no visible binding for global variable" when using paths
if (getRversion() >= "2.15.1") {
  utils::globalVariables(c("ltcf_addr_path", "ltcf_phone_path"))
}
