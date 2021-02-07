#' Download Current ACNS Data
#'
#' `download_acns()` downloads data from both the address and texting ACNS
#' files and merges the two using common columns. It uses the address data as
#' reference; any data in the SMS dataset not matching the address data is
#' dropped. This is because address data is necessary for some downstream
#' functions.
#'
#' @param primary Which data source should be used as the primary table? Choices
#'   are `"sms"` or `"addr"`; defaults to `"sms"`.
#'
#' @param addr_creds Character. A length 2 vector containing the credentials for
#'   accessing the ACNS address data. The username should appear first, then the
#'   password.
#'
#' @param sms_creds Character. A length 2 vector containing the credentials for
#'   accessing the ACNS SMS data. The username should appear first, then the
#'   password.
#'
#' @return A `tibble` containing the joined data
#'
#' @export
download_acns <- function(
  primary = c("sms", "addr"),
  addr_creds = Sys.getenv(c("acns_usr", "acns_pwd")),
  sms_creds = Sys.getenv(c("sftp_usr", "sftp_pwd"))
) {

  primary <- rlang::arg_match(primary)[[1L]]

  addr <- download_addr(usr = addr_creds[[1L]], pwd = addr_creds[[2L]])
  sms  <- download_sms(usr = sms_creds[[1]], pwd = sms_creds[[2L]])

  by_cols <- dplyr::intersect(colnames(addr), colnames(sms))

  purrr::when(
    primary,
    . == "sms"  ~ dplyr::right_join(addr, sms, by = by_cols),
    . == "addr" ~ dplyr::left_join(addr, sms, by = by_cols),
    ~ rlang::abort("`primary` must be 'sms' or 'addr'")
  ) %>%
    dplyr::relocate(dplyr::matches("^date_added$"), .before = 1L) %>%
    as_date_tbl(date = acns_date_added(.))
}

#' Download ACNS_DAILY_OUT File from SFTP Server
#'
#' `download_sms()` downloads the file used to send text messages. Date columns
#' are returned as dates; all other are character. All column names are passed
#' through `janitor::clean_names()`.
#'
#' @param path Path to file to download on SFTP server
#'
#' @inheritParams download_sftp
#'
#' @param archive Should the data be archived before loading in-memory? This
#'   will only write data to an archive file that does not already exist.
#'
#' @param archive_dir The archive directory path
#'
#' @param ... Additional arguments to pass to `read_file()`
#'
#' @return A `tibble` containing the data from the ACNS sms file
#'
#' @export
download_sms <- function(
  path = "ACNS/ACNS_DAILY_OUT.txt",
  usr = Sys.getenv("sftp_usr"),
  pwd = Sys.getenv("sftp_pwd"),
  server = "xfer.shelbycountytn.gov",
  archive = TRUE,
  archive_dir = "V:/EPI DATA ANALYTICS TEAM/Mass Texting/ACNS_DAILY_OUT/",
  ...
) {

  # Standardize path
  path <- path %>% fs::path_norm() %>% fs::path_tidy()

  # Download data to temp directory
  path_local <- download_sftp(
    file = fs::path_file(path),
    dir_remote = fs::path_dir(path),
    server = server,
    usr = usr,
    pwd = pwd
  )
  on.exit(try(fs::dir_delete(path_local), silent = TRUE), add = TRUE)

  # Save to data archive if `archive = TRUE`
  if (archive) {
    archive_sms(path = path_local)
  }

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

  # Load data
  read_file(path_local, col_types = col_types, ...) %>%
    janitor::clean_names()
}

#' Download Address File from SFTP Server
#'
#' `download_addr()` downloads and loads the address file from the SFTP server.
#' Date columns are returned as dates; all others are returned as character. All
#' column names are passed through `janitor::clean_names()`.
#'
#' @inheritParams download_sms
#'
#' @return A `tibble` containing data from the ACNS address file
#'
#' @export
download_addr <- function(
  path = "ACNS_ADDRESS_SAMPLE.csv",
  usr = Sys.getenv("acns_usr"),
  pwd = Sys.getenv("acns_pwd"),
  server = "xfer.shelbycountytn.gov",
  ...
) {

  # Standardize path
  path <- path %>% fs::path_norm() %>% fs::path_tidy()

  # Download data to temp directory
  path_local <- download_sftp(
    file = fs::path_file(path),
    dir_remote = fs::path_dir(path),
    server = server,
    usr = usr,
    pwd = pwd
  )
  on.exit(try(fs::dir_delete(path_local), silent = TRUE), add = TRUE)

  # Define column types
  col_types <- vroom::cols(
    PKEY = vroom::col_character(),
    RESULT = vroom::col_character(),
    TEST_DATE = vroom::col_date(format = "%m/%d/%Y"),
    FIRST_NAME = vroom::col_character(),
    LAST_NAME = vroom::col_character(),
    DOB = vroom::col_date(format = "%m/%d/%Y"),
    SEX = vroom::col_character(),
    PNUMBER = vroom::col_character(),
    ADDR1 = vroom::col_character(),
    ADDR2 = vroom::col_character(),
    CITY = vroom::col_character(),
    STATE = vroom::col_character(),
    ZIP = vroom::col_character()
  )

  # Load data and assign date
  read_file(path_local, col_types = col_types, ...) %>%
    dplyr::rename(DATE_OF_BIRTH = .data[["DOB"]]) %>%
    janitor::clean_names()
}


#' Read Delimited/Fixed-Width/Excel File
#'
#' @param file File to read
#'
#' @param col_types readr/vroom column specification
#'
#' @param ... Additional arguments to pass to `vroom()` or `read_excel()`
#'
#' @return A `tibble` containing the file data
read_file <- function(
  file,
  col_types = vroom::cols(.default = vroom::col_character()),
  ...
) {
  file <- path_create(file)
  ext  <- fs::path_ext(file)

  if (tolower(ext) %in% c("xlsx", "xls")) {
    col_types <- vroom_to_readxl(col_types)
    coviData::read_file_excel(file = file, col_types = col_types, ...)
  } else {
    coviData::read_file_delim(file = file, col_types = col_types, ...)
  }
}

# Helpers ----------------------------------------------------------------------

#' Download File from SFTP Server
#'
#' @param file File name to pull from the SFTP server. This should
#'   not include directories.
#'
#' @param usr Username for SFTP server
#'
#' @param pwd Password for SFTP server
#'
#' @param dir_remote Directory containing file in SFTP server
#'
#' @param dir_local Directory to save file to on local computer
#'
#' @param server Server address
#'
#' @return Path where the file is downloaded
download_sftp <- function(
  file,
  usr,
  pwd,
  dir_remote = "",
  dir_local = fs::file_temp("dir"),
  server = "xfer.shelbycountytn.gov"
) {

  file <- file %>% fs::path_norm() %>% fs::path_tidy() %>% fs::path_file()
  ext <- fs::path_ext(file)
  dir_remote <- dir_remote %>% fs::path_norm() %>% fs::path_tidy()
  dir_local <- path_create(dir_local)

  # Create SFTP connection object
  sftp_con <- coviData::sftp_connect(
    server = server,
    folder = dir_remote,
    username = usr,
    password = pwd
  )

  # Create `dir_local` if it doesn't exist
  if (!fs::dir_exists(dir_local)) {
    dir_local <- fs::dir_create(dir_local)
  }

  # Download from `server`
  coviData::sftp_download(
    file = file,
    tofolder = dir_local,
    sftp_connection = sftp_con,
    verbose = FALSE
  )

  invisible(path_create(dir_local, file))
}

#' Pull Latest `date_added` from ACNS Data
#'
#' @param .data ACNS data with a `date_added` column
#'
#' @return The latest `date_added` in `Date` format, or `NA_Date_` if not
#'   available
acns_date_added <- function(.data) {

  # If no `date_added` column (in lowercase), returns missing date
  if (!"date_added" %in% colnames(.data)) {
    return(lubridate::NA_Date_)
  }

  is_dt_dttm <- any(
    lubridate::is.Date(.data[["date_added"]]),
    lubridate::is.POSIXt(.data[["date_added"]])
  )

  # Get maximum date from `date_added`
  if (is_dt_dttm) {
    date <- suppressWarnings(max(.data[["date_added"]], na.rm = TRUE))
  } else {
    date <- suppressWarnings(
      .data[["date_added"]] %>% std_dates() %>% max(na.rm = TRUE)
    )
  }

  # Handle all missing `date_added`
  if (rlang::is_empty(date) || abs(as.integer(date)) %in% c(Inf, NA_integer_)) {
    date <- lubridate::NA_Date_
  }

  # Return in `Date` format (even if datetime)
  lubridate::as_date(date)
}

#' Convert readr/vroom Column Specification to readxl Format
#'
#' @param col_types A `col_types` specification for vroom/readr
#'
#' @return A `col_types` specification for readxl
vroom_to_readxl <- function(col_types) {

  map_col_types <- purrr::as_mapper(
    ~ dplyr::case_when(
      identical(.x, vroom::col_logical()) ~ "logical",
      identical(.x, vroom::col_integer()) ~ "numeric",
      identical(.x, vroom::col_big_integer()) ~ "numeric",
      identical(.x, vroom::col_double()) ~ "numeric",
      identical(.x, vroom::col_number()) ~ "numeric",
      identical(.x, vroom::col_guess()) ~ "guess",
      identical(.x, vroom::col_skip()) ~ "skip",
      identical(no_fmt(.x), vroom::col_date()) ~ "date",
      identical(no_fmt(.x), vroom::col_time()) ~ "date",
      identical(no_fmt(.x), vroom::col_datetime()) ~ "date",
      TRUE ~ "text"
    )
  )

  if (rlang::is_empty(col_types[["cols"]])) {
    map_col_types(col_types[["default"]])
  } else {
    purrr::map_chr(col_types[["cols"]], map_col_types)
  }
}

#' Standardize vroom/readr `col_spec`s with No Format
#'
#' @param col_spec A column specification from vroom/readr
#'
#' @return The column specification with format set to `""`
no_fmt <- function(col_spec) {
  if (!is.null(col_spec[["format"]])) col_spec[["format"]] <- ""
  col_spec
}
