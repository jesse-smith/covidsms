#' Load New NBS Positive PCRs for a Given Date
#'
#' `load_new_positive()` loads newly reported positive tests on a given date.
#' It identifies newly reported tests on that date, subsets to those tests, and
#' adds information from the investigations file to the identified records.
#'
#' @param date Optional. Character (formatted as "YYYY-MM-DD") or `Date`.
#'
#' @export
load_new_positive <- function(date = NULL) {

  # Get date of latest NBS PCR file
  if (rlang::is_empty(date)) {
    date <- path_pcr() %>%
      stringr::str_extract(pattern = "[0-9]{8}") %>%
      lubridate::mdy()
  } else {
    date <- lubridate::as_date(date)
  }

  # Load positives tests in latest and second-latest files
  pcr_new <- load_pcr_positive(date = date) %>%
    janitor::clean_names()
  pcr_old <- load_pcr_positive(date = date - 1L) %>%
    janitor::clean_names() %>%
    dplyr::select("lab_local_id")

  # Join data from investigations file
  inv_new <- load_inv_positive(date = date)

  pcr_new %>%
    # Get rid of duplicates in PCR files
    dplyr::anti_join(pcr_old, by = "lab_local_id") %>%
    # Add data from investigations file
    dplyr::left_join(inv_new, by = "inv_local_id", suffix = c("", "_inv")) %>%
    dplyr::select(-dplyr::ends_with("_inv")) %>%
    as_date_tbl(date = date)
}

#' Load Positive Tests or Cases From NBS Snapshot File
#'
#' `load_pcr_positive()` loads positive tests from the latest PCR snapshot file,
#' or the one specified by `date`, if provided. `load_inv_positive()` does the
#' same for the latest investigations snapshot file.
#'
#' @param date Optional. Character (formatted as "YYYY-MM-DD") or `Date`.
#'
#' @name load_nbs_positive
#'
#' @aliases load_inv_positive load_pcr_positive
NULL

#' @rdname load_nbs_positive
#'
#' @export
load_pcr_positive <- function(date = NULL) {

  path_pcr(date = date) %>%
    read_file_delim() %>%
    janitor::clean_names() %>%
    dplyr::filter(
      .data[["inv_case_status"]] %in% c("C", "P"),
      .data[["lab_result"]] %in% c("Positive", "Presumptive Positive")
    )
}

#' @rdname load_nbs_positive
#'
#' @export
load_inv_positive <- function(date = NULL) {
  path_inv(date = date) %>%
    read_file_delim() %>%
    janitor::clean_names() %>%
    dplyr::filter(.data[["inv_case_status"]] %in% c("C", "P"))
}
