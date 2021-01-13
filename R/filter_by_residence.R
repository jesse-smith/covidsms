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

  NAs <- c("NA", "na", "Na", "nA", "N/A", "n/a", "N/a", "n/A")

  missings <- c(NA_character_, NAs, "", " ", ".")

  .data %>%
    dplyr::filter(
      stringr::str_detect(
        .data[["jurisdiction_nm"]],
        pattern = "Memphis(/|[ ])Shelby County"
      ),
      .data[["alt_county"]] %in% c("Shelby County", missings),
      .data[["patient_state"]] %in% c("47", "TN", missings)
    )
}
