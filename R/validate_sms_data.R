validate_sms_data <- function(.data) {

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

  iso_date <- "[0-9]{4}-(0[1-9]|1[0-2])-(0[1-9]|1[0-9]|2[0-9]|3[01])"
  pkey <- "NBS-[0-9]{9}"
  result <- "^POSITIVE$"
  test_date <- "([1-9]|1[0-2])/([1-9]|1[0-9]|2[0-9]|3[01])/[0-9]{4}"
  phone_number <- "^([+]?[0-9]{1,2})?[0-9]{10}"

  assert_string(.data[["DATE_ADDED"]], pattern = iso_date, na_rm = TRUE)
  assert_string(.data[["PKEY"]], pattern = pkey, na_rm = FALSE)
  assert_string(.data[["RESULT"]], pattern = result, na_rm = TRUE)
  assert_string(.data[["TEST_DATE"]], pattern = test_date, na_rm = TRUE)
  assert_string(.data[["DATE_OF_BIRTH"]], pattern = iso_date, na_rm = TRUE)
  assert_string(.data[["PNUMBER"]], pattern = phone_number, na_rm = TRUE)

  .data
}
