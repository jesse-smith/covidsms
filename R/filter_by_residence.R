#' Filter Positive Records by Jurisdiction/County/State Residence
#'
#' `filter_by_residence()` filters out non-Shelby-County residents, while
#' retaining individuals for whom residence is verified or unsure.
#'
#' @param .data The NBS data to filter; should be the result of joining a
#'   PCR and investigations file
#'
#' @export
filter_by_residence <- function(.data) {

  juri_pattern <- "Memphis(/|[ ])Shelby County"

    dplyr::filter(
      .data,
      stringr::str_detect(.data[["jurisdiction_nm"]], pattern = juri_pattern),
      .data[["alt_county"]] %in% c("Shelby County", NA_character_),
      .data[["patient_state"]] %in% c("47", "TN", NA_character_)
    )
}
