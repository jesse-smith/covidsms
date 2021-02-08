#' Check for New ACNS SMS File (ACNS_DAILY_OUT)
#'
#' `check_new_sms()` downloads the SMS file on the SFTP server and compares it
#' to the latest local SMS file. If these two are not identical, it returns
#' `TRUE`; otherwise, `FALSE`.
#'
#' @return `logical`
#'
#' @export
check_new_sms <- function() {
  # Define column types
  col_types <- vroom::cols(
    DATE_ADDED = vroom::col_date(format = "%m/%d/%Y"),
    RESULT = vroom::col_character(),
    TEST_DATE = vroom::col_date(format = "%m/%d/%Y"),
    FIRST_NAME = vroom::col_character(),
    LAST_NAME = vroom::col_character(),
    DATE_OF_BIRTH = vroom::col_date(format = "%m/%d/%Y"),
    SEX = vroom::col_character(),
    PNUMBER = vroom::col_character()
  )

  old <- path_sms() %>%
    coviData::read_file_delim(col_types = col_types) %>%
    janitor::clean_names()

  !identical(download_sms(archive = FALSE), old)
}
