#' Filter Deaths from Positive NBS Tests
#'
#' `filter_by_death()` removes deaths from positive test results to avoid
#' attempts to text a deceased individual.
#'
#' @param .data Data from which to remove deaths
#'
#' @return `.data` with known deaths removed
#'
#' @export
filter_by_death <- function(.data) {
  dplyr::filter(
    .data,
    .data[["die_from_illness_ind"]] != "Y"
      | is.na(.data[["die_from_illness_ind"]])
  )
}
