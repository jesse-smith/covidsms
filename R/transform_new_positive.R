#' Transform NBS Data to Format Necessary for Exporting to ACNS File
#'
#' `transform_new_positive()` transforms data from NBS snapshot files to the
#' format needed for exports to the ACNS process.
#'
#' @param .data Data from NBS snapshot file
#'
#' @param date The date to use when creating the `DATE_ADDED` variable
#'
#' @return A `date_tbl`, which is a `tibble` with a `date` attribute
#'
#' @export
transform_new_positive <- function(.data, date = attr(.data, "date")) {
  .data %>%
    dplyr::transmute(
      DATE_ADDED = date %>% format("%Y-%m-%d"),
      RESULT = "POSITIVE",
      TEST_DATE = std_dates(.data[["spec_date_only"]]) %>% format("%m/%d/%Y"),
      FIRST_NAME = .data[["patient_first_name"]] %>%
        stringr::str_to_upper() %>%
        stringr::str_squish(),
      LAST_NAME = .data[["patient_last_name"]] %>%
        stringr::str_to_upper() %>%
        stringr::str_squish(),
      DATE_OF_BIRTH = std_dates(.data[["patient_dob"]]) %>% format("%Y-%m-%d"),
      SEX = .data[["patient_current_sex"]],
      PNUMBER = dplyr::coalesce(
        .data[["patient_tel_cell"]],
        .data[["patient_tel_home"]],
        .data[["patient_phone_work"]]
      )
    ) %>%
    as_date_tbl(date = date)
}
