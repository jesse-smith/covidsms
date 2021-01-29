#' Archive ACNS Upload
#'
#' `archive_positive()` creates a backup of the ACNS data upload.
#'
#' @param .data A data frame or data fram extension (e.g. a `tibble`)
#'
#' @param dir The archive directory
#'
#' @param date The date to use as a timestamp
#'
#' @return `.data` (invisibly)
#'
#' @export
archive_positive <- function(.data, dir, date = attr(.data, "date")) {
  # Check that `.data` is a data frame
  coviData::assert_dataframe(.data)

  # Check that `.data` is formatted correctly
  validate_sms_data(.data)

  # Create save path
  path <- path_create(dir, paste0("acns_data_", date, ext = "fst"))

  # Save
  fst::write_fst(.data, path = path, compress = 100L)
}
