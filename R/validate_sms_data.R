#' Validate Data for Inclusion in the ACNS File
#'
#' @param .data Data to validate
#'
#' @param type Type of data; either `"positive"` or `"acns"` data
#'
#' @return The input data if validation passes, otherwise throws an error
#'
#' @export
validate_sms_data <- function(.data, type = c("positive", "acns")) {
  type <- rlang::arg_match(type)[[1L]]

  if (type == "positive") {
    validate_sms_data_positive(.data)
  } else {
    validate_sms_data_acns(.data)
  }
}

validate_sms_data_positive <- function(.data) {

  # Validate that `.data` is a data frame
  assert_dataframe(.data)

  # Validate columns of `.data`
  assert_cols(
    .data,
    "DATE_ADDED",
    "PKEY",
    "RESULT",
    "TEST_DATE",
    "FIRST_NAME",
    "LAST_NAME",
    "DATE_OF_BIRTH",
    "SEX",
    "PNUMBER",
    "ADDR1",
    "ADDR2",
    "CITY",
    "STATE",
    "ZIP",
    ptype = character(),
    n = 14L
  )

  coviData::assert_all(
    is_chr_ascii(.data, na_rm = TRUE),
    message = "All columns of `.data` must be ASCII character encoded"
  )

  date_fmt <- "(0[1-9]|1[0-2])/(0[1-9]|1[0-9]|2[0-9]|3[01])/[0-9]{4}"
  pkey <- "NBS-[0-9]{9}"
  result <- "POSITIVE"
  phone_number <- "^([+]?[0-9]{1,2})?[0-9]{10}"

  assert_string(.data[["DATE_ADDED"]], pattern = date_fmt, na_rm = TRUE)
  assert_string(.data[["PKEY"]], pattern = pkey, na_rm = FALSE)
  assert_string(.data[["RESULT"]], pattern = result, na_rm = TRUE)
  assert_string(.data[["TEST_DATE"]], pattern = date_fmt, na_rm = TRUE)
  assert_string(.data[["DATE_OF_BIRTH"]], pattern = date_fmt, na_rm = TRUE)
  assert_string(.data[["PNUMBER"]], pattern = phone_number, na_rm = TRUE)

  .data
}

validate_sms_data_acns <- function(.data) {
  # Validate that `.data` is a data frame
  assert_dataframe(.data)

  # Validate columns of `.data`
  assert_cols(
    .data,
    "DATE_ADDED",
    "RESULT",
    "TEST_DATE",
    "FIRST_NAME",
    "LAST_NAME",
    "DATE_OF_BIRTH",
    "SEX",
    "PNUMBER",
    ptype = character(),
    n = 8L
  )

  coviData::assert_all(
    is_chr_ascii(.data, na_rm = TRUE),
    message = "All columns of `.data` must be ASCII character encoded"
  )

  date_fmt <- "(0[1-9]|1[0-2])/(0[1-9]|1[0-9]|2[0-9]|3[01])/[0-9]{4}"
  result <- "[A-Z]+"
  phone_number <- "^[0-9]{10}$"

  assert_string(.data[["DATE_ADDED"]], pattern = date_fmt, na_rm = TRUE)
  assert_string(.data[["RESULT"]], pattern = result, na_rm = TRUE)
  assert_string(.data[["TEST_DATE"]], pattern = date_fmt, na_rm = TRUE)
  assert_string(.data[["DATE_OF_BIRTH"]], pattern = date_fmt, na_rm = TRUE)
  assert_string(.data[["PNUMBER"]], pattern = phone_number, na_rm = TRUE)

  .data
}
