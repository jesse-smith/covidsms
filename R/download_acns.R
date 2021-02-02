#' Download Current ACNS Data
#'
#' `download_acns()` downloads data from both the address and texting ACNS
#' files and merges the two using common columns. It uses the address data as
#' reference; any data in the SMS dataset not matching the address data is
#' dropped. This is because address data is necessary for some downstream
#' functions.
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
  addr_creds = Sys.getenv(c("acns_usr", "acns_pwd")),
  sms_creds = Sys.getenv(c("sftp_usr", "sftp_pwd"))
) {

  addr <- download_addr(usr = addr_creds[[1L]], pwd = addr_creds[[2L]])
  sms  <- download_sms(usr = sms_creds[[1]], pwd = sms_creds[[2L]])

  by_cols <- dplyr::intersect(colnames(addr), colnames(sms))

  dplyr::left_join(addr, sms, by = by_cols)
}

acns_date <- function(.data) {

  if (!"DATE_ADDED" %in% colnames(.data)) {
    return(
      suppressMessages(download_sms()) %>%
        dplyr::pull(.data[["DATE_ADDED"]]) %>%
        max(na.rm = TRUE)
    )
  }

  is_dt_dttm <- any(
    lubridate::is.Date(.data[["DATE_ADDED"]]),
    lubridate::is.POSIXt(.data[["DATE_ADDED"]])
  )

  if (is_dt_dttm) {
    date <- suppressWarnings(max(.data[["DATE_ADDED"]], na.rm = TRUE))
  } else {
    date <- suppressWarnings(
      .data[["DATE_ADDED"]] %>%
        std_dates() %>%
        max(na.rm = TRUE)
    )
  }

  if (rlang::is_empty(date) || abs(as.integer(date)) %in% c(Inf, NA_integer_)) {
    date <- lubridate::today()
  }

  lubridate::as_date(date)
}

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

download_sms <- function(
  path = "ACNS/ACNS_DAILY_OUT.txt",
  usr = Sys.getenv("sftp_usr"),
  pwd = Sys.getenv("sftp_pwd"),
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
    as_date_tbl(date = acns_date(.))
}

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
    dplyr::mutate(DATE_ADDED = acns_date(.), .before = 1L) %>%
    as_date_tbl(date = acns_date(.))
}

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

no_fmt <- function(col_spec) {
  if (!is.null(col_spec[["format"]])) col_spec[["format"]] <- ""
  col_spec
}
