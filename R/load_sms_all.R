load_sms_all <- function(
  dir = "V:/EPI DATA ANALYTICS TEAM/Mass Texting/ACNS_DAILY_OUT/",
  excl_last = FALSE
) {

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
  # Standardize directory path
  path_create(dir) %>%
    # Get ACNS files from directory
    fs::dir_ls(
      type = "file",
      regexp = ".*/ACNS_DAILY_OUT.*"
    ) %>%
    # Sort (will be by date)
    sort(decreasing = TRUE) %>%
    # Exclude the latest file if `excl_last = TRUE`
    purrr::when(excl_last ~ extract(., -1L), ~ .) %>%
    # Read all
    coviData::read_file_delim(col_types = col_types) %>%
    janitor::clean_names() %>%
    as_date_tbl(acns_date_added(.))
}

std_sms <- function(.data, date = attr(.data, "date")) {
  .data %>%
    janitor::clean_names() %>%
    std_acns() %>%
    std_acns_phone() %>%
    remove_temp() %>%
    as_date_tbl(date = date)
}
