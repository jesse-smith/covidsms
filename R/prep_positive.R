#' Transform NBS Data to Format Necessary for Exporting to ACNS File
#'
#' `prep_positive()` transforms data from NBS snapshot files to the
#' format needed for exports to the ACNS process.
#'
#' @param .data Data from NBS snapshot file
#'
#' @param date The date to use when creating the `DATE_ADDED` variable
#'
#' @return A `date_tbl`, which is a `tibble` with a `date` attribute
#'
#' @export
prep_positive <- function(
  .data = load_positive(),
  date = attr(.data, "date"),
  phone = c("cell", "home", "work")
) {

  rlang::arg_match(phone)

  phone_cols <- phone %>%
    stringr::str_replace(
      pattern = "cell",
      replacement = "patient_tel_cell"
    ) %>%
    stringr::str_replace(
      pattern = "home",
      replacement = "patient_tel_home"
    ) %>%
    stringr::str_replace(
      pattern = "work",
      replacement = "patient_phone_work"
    )

  .data %>%
    filter_by_residence() %>%
    filter_by_death() %>%
    dplyr::transmute(
      DATE_ADDED = dplyr::coalesce(
        .data[["report_date"]],
        vctrs::vec_rep(date, dplyr::n())
      ) %>%
        format("%Y-%m-%d"),
      PKEY = pkey_nbs(.data[["lab_local_id"]]),
      RESULT = "POSITIVE",
      TEST_DATE = std_dates(.data[["spec_date_only"]]) %>%
        as_single_digit_date(),
      FIRST_NAME = .data[["patient_first_name"]] %>%
        stringr::str_to_upper() %>%
        stringr::str_squish(),
      LAST_NAME = .data[["patient_last_name"]] %>%
        stringr::str_to_upper() %>%
        stringr::str_squish(),
      DATE_OF_BIRTH = std_dates(.data[["patient_dob"]]) %>% format("%Y-%m-%d"),
      SEX = .data[["patient_current_sex"]],
      PNUMBER = dplyr::across({{ phone_cols}}, std_phone) %>%
        dplyr::transmute(ph = coviData::coalesce_across({{ phone_cols }})) %>%
        dplyr::pull("ph"),
      ADDR1 = std_addr(.data[["patient_street_addr_1"]]),
      ADDR2 = std_addr(.data[["patient_street_addr_2"]]),
      CITY = std_city(.data[["patient_city"]]),
      STATE = std_state(.data[["patient_state"]]),
      ZIP = std_zip(.data[["patient_zip"]]),
      .inv_id_tmp_ = .data[["inv_local_id"]],
      .lab_tmp_ = .data[["perform_facility_name"]] %>%
        std_lab_names() %>%
        std_labs_ael() %>%
        std_labs_baptist() %>%
        std_labs_poplar() %>%
        std_labs_ut() %>%
        std_labs_labcorp() %>%
        std_labs_quest() %>%
        std_labs_methodist() %>%
        std_labs_mm() %>%
        std_labs_mmg()
    ) %>%
    distinct_investigation() %>%
    distinct_test_date() %>%
    filter_by_lab() %>%
    remove_temp() %>%
    as_chr_ascii() %>%
    as_date_tbl(date = date) %>%
    validate_sms_data()
}

#' Convert `Date` Objects to "M/D/YYYY" Format
#'
#' `as_single_digit_date()` converts `Date` vectors to "M/D/YYYY" `character`
#' format, where month and day do not contain leading zeros (i.e. April 1, 2020
#' would be 4/1/2020, not 04/01/2020).
#'
#' @param x A `Date` vector
#'
#' @return A `character` vector of formatted dates
#'
#' @export
as_single_digit_date <- function(x) {
  # Don't want to import globally, so assigning
  year  <- function(x) lubridate::year(x)
  month <- function(x) lubridate::month(x)
  day   <- function(x) lubridate::day(x)

  dplyr::if_else(
    is.na(x),
    NA_character_,
    as.character(stringr::str_glue("{month(x)}/{day(x)}/{year(x)}"))
  )
}

pkey_nbs <- function(string) {

  # Extract ID numbers from local id
  id <- stringr::str_extract(string, "[0-9]{8}")

  # Create unique prefix for any duplicates (up to n = 10)
  dupes <- tibble::tibble(
    i = vctrs::vec_seq_along(id),
    d = vctrs::vec_duplicate_id(id)
  ) %>%
    dplyr::filter(.data[["i"]] != .data[["d"]]) %>%
    dplyr::group_by(.data[["d"]]) %>%
    dplyr::summarize(.data[["i"]], n = seq_len(dplyr::n())) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(n = as.character(.data[["n"]])) %>%
    dplyr::select(-"d")

  # Create unique IDs
  vctrs::vec_assign(
    vctrs::vec_rep("0", times = vctrs::vec_size(id)),
    i = dupes[["i"]],
    value = dupes[["n"]]
  ) %>%
    {paste0("NBS-", ., id)}
}

std_nbs_id <- function(string) {
  string %>%
    stringr::str_extract("[0-9]{8}") %>%
    stringr::str_remove("^[0-9]0*")
}

remove_temp <- function(.data) {
  dplyr::select(.data, -dplyr::matches(c("^[.].*_tmp_$", "^[.]tmp_$")))
}
