load_sms_all <- function(
  dir = "V:/EPI DATA ANALYTICS TEAM/Mass Texting/ACNS_DAILY_OUT",
  excl_last = FALSE
) {

  # Define column types
  col_types <- vroom::cols(
    PKEY = vroom::col_character(),
    DATE_ADDED = vroom::col_date(format = "%m/%d/%Y"),
    RESULT = vroom::col_character(),
    TEST_DATE = vroom::col_date(format = "%m/%d/%Y"),
    FIRST_NAME = vroom::col_character(),
    LAST_NAME = vroom::col_character(),
    DATE_OF_BIRTH = vroom::col_date(format = "%m/%d/%Y"),
    SEX = vroom::col_character(),
    PNUMBER = vroom::col_character()
  )
  # Standardize directory path
  dir <- path_create(dir)

  # Get ACNS files from directory
  file_regex <- paste0(
    "/ACNS_DAILY_OUT/ACNS_DAILY_OUT_[0-9]{4}-[0-9]{2}-[0-9]{2}[.]txt"
  )
  files <- fs::dir_ls(dir, type = "file", regexp = file_regex)

  # Get latest date in file names
  date <- files %>%
    fs::path_file() %>%
    fs::path_ext_remove() %>%
    stringr::str_extract("[0-9]{1,4}-[0-9]{1,2}-[0-9]{1,4}") %>%
    lubridate::as_date() %>%
    max(na.rm = TRUE) %>%
    purrr::when(
      abs(as.double(.)) == Inf ~ lubridate::NA_Date_,
      ~ lubridate::as_date(.)
    )

  files %>%
    # Remove empty files
    stringr::str_subset("EMPTY", negate = TRUE) %>%
    # Sort (will be by date)
    sort(decreasing = TRUE) %>%
    # Exclude the latest file if `excl_last = TRUE`
    purrr::when(excl_last ~ extract(., -1L), ~ .) %>%
    # Read all
    coviData::read_file_delim(col_types = col_types) %>%
    janitor::clean_names() %>%
    as_date_tbl(date = date)
}

std_sms <- function(.data, date = attr(.data, "date")) {
  .data %>%
    janitor::clean_names() %>%
    std_acns() %>%
    std_acns_phone() %>%
    remove_temp() %>%
    as_date_tbl(date = date)
}
