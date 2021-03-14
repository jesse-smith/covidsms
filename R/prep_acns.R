#' Prepare ACNS Data for Use in Case Assignment
#'
#' @description
#' `prep_acns()` prepares ACNS data for use in case assignment and NBS data
#' management processes. In addition to standardizing the dataset, it adds 3
#' variables:
#' \describe{
#'   \item{school_age}{A logical indicating whether a case is school-aged}
#'   \item{long_term_care}{A logical indicating whether a case is housed in a
#'     long-term care facility}
#'   \item{duplicate}{A logical indicating whether a case is a duplicate}
#' }
#'
#' @param .data Data frame or data frame extension containing ACNS data. If none
#'   is supplied, the latest data will be downloaded.
#'
#' @param incl_positive Should the output include new positives from NBS?
#'
#' @param filter_acns Should duplicated data from the previous ACNS files be
#'   filtered out?
#'
#' @param filter_lab Should labs in the current ACNS data be filtered from NBS?
#'
#' @param assign Is this data being used for assignment purposes (`TRUE`) or
#'   sms notification (`FALSE`)? The default is `FALSE`.
#'
#' @param date The date that the data was reported. This is extracted from the
#'   `date` attribute of a `date_tbl`.
#'
#' @return Prepared data in `tibble` format
#'
#' @export
prep_acns <- function(
  .data = download_acns(),
  incl_positive = TRUE,
  filter_acns = TRUE,
  filter_lab = TRUE,
  assign = FALSE,
  date = attr(.data, "date")
) {

  if (filter_lab) {
    lab_date <- if (assign) date else date - lubridate::days(1L)
    labs <- path_sms(date = lab_date) %>%
      read_file_delim(col_select = "PKEY") %>%
      acns_labs()
  } else {
    labs = character()
  }

  .data %>%
    janitor::clean_names() %>%
    std_acns() %>%
    filter_by_acns(filter = filter_acns, excl_last = TRUE, date = date) %>%
    purrr::when(
      incl_positive ~ bind_acns_positive(
        .,
        date = date,
        assign = assign,
        filter_acns = filter_acns,
        filter_lab = filter_lab,
        labs = labs
      ),
      ~ .
    ) %>%
    distinct_acns() %>%
    add_acns_school_age() %>%
    add_acns_long_term_care() %>%
    add_acns_duplicate(date = date, assign = assign) %>%
    std_acns_phone() %>%
    remove_temp() %>%
    as_date_tbl(date = date)
}

acns_labs <- function(.data) {
  if (!"pkey" %in% colnames(.data)) return(character())

  labs <- c("AEL", "BAPT", "CCHS", "POPH", "UT")

  lab_pattern <- paste0("^(", paste0(labs, collapse = "|"), ")")

  .data[["pkey"]] %>%
    stringr::str_extract(lab_pattern) %>%
    vctrs::vec_unique() %>%
    stats::na.omit() %>%
    as.vector(mode = "character") %>%
    stringr::str_replace("BAPT", replacement = "BAPTIST") %>%
    stringr::str_replace("POPH", replacement = "POPLAR")
}

#' Bind Positive NBS Data to ACNS
#'
#' `bind_acns_positive()` binds new data from `prep_positive()` to the input
#' ACNS data.
#'
#' @param .acns ACNS data
#'
#' @param assign Is this call for case assignment purposes (`TRUE`) or
#'   SMS purposes (`FALSE`)? Default is `FALSE`.
#'
#' @param date The `date` attribute for the `date_tbl` output
#'
#' @param ... Additional arguments to pass to
#'   \code{\link[covidsms:prep_positive]{prep_positive()}}; at time of creation,
#'   these include `filter_lab` and `filter_acns`
bind_acns_positive <- function(
  .acns,
  assign = FALSE,
  date = attr(.acns, "date"),
  ...
) {

  date_pos <- if (assign) date else date - lubridate::days(1L)

  dplyr::bind_rows(
    load_positive(date_pos) %>% prep_positive(filter_new = TRUE, ...),
    janitor::clean_names(.acns)
  ) %>%
    as_date_tbl(date = date)
}

distinct_acns <- function(.data) {
  .data %>%
    dplyr::mutate(
      .phone_zip_tmp_ = coviData::coalesce_across(c("pnumber",  "zip"))
    ) %>%
    coviData::coalesce_dupes(
      .data[["first_name"]],
      .data[["last_name"]],
      .data[["date_of_birth"]],
      .data[[".phone_zip_tmp_"]]
    )
}

std_acns <- function(.data) {

  other_cols <- c("pkey", "nbs", "addr1", "addr2", "city", "state", "zip")
  cols_to_add <- other_cols[!other_cols %in% tolower(colnames(.data))]

  na_col_chr <- rep(NA_character_, times = NROW(.data))

  cols_loc <- c(
    "date_added", "pkey", "nbs", "result", "test_date",
    "first_name", "last_name", "date_of_birth", "sex", "pnumber",
    "addr1", "addr2", "city", "state", "zip"
  )

  .data %>%
    purrr::when(
      length(cols_to_add) == 0L ~ .,
      ~ dplyr::bind_cols(
        .,
        purrr::map_dfc(cols_to_add, ~ tibble::as_tibble_col(na_col_chr, .x))
      )
    ) %>%
    dplyr::relocate({{ cols_loc }}) %>%
    dplyr::mutate(
      date_added = std_dates(.data[["date_added"]], force = "dt") %>%
        dplyr::na_if(std_dates("1900-01-01")),
      pkey = as.character(.data[["pkey"]]),
      nbs = as.character(.data[["nbs"]]),
      result = std_names(.data[["result"]]),
      test_date = std_dates(.data[["test_date"]], force = "dt") %>%
        dplyr::na_if(std_dates("1900-01-01")),
      first_name = std_names(.data[["first_name"]]),
      last_name = std_names(.data[["last_name"]]),
      date_of_birth = std_dates(.data[["date_of_birth"]], force = "dt")  %>%
        dplyr::na_if(std_dates("1900-01-01")),
      sex = std_names(.data[["sex"]]),
      pnumber = std_phone(.data[["pnumber"]]),
      addr1 = std_addr(.data[["addr1"]]),
      addr2 = std_addr(.data[["addr2"]]),
      city = std_city(.data[["city"]]),
      state = std_state(.data[["state"]]),
      zip = std_zip(.data[["zip"]])
    )
}

add_acns_school_age <- function(.data) {
  dplyr::mutate(
    .data,
    .current_date_tmp_ = dplyr::coalesce(
      .data[["test_date"]],
      .data[["date_added"]]
    ),
    school_age = acns_school_age(
      birth_date = .data[["date_of_birth"]],
      current_date = .data[[".current_date_tmp_"]]
    )
  )
}

add_acns_long_term_care <- function(.data) {
  dplyr::mutate(
    .data,
    long_term_care = acns_long_term_care(
      pnumber = .data[["pnumber"]],
      addr1 = .data[["addr1"]],
      zip = .data[["zip"]]
    )
  )
}

add_acns_duplicate <- function(
  .data,
  date = attr(.data, "date"),
  max_dist = 90L,
  assign = FALSE
) {

  date_pos <- if (assign) date else date - lubridate::days(1L)

  all_positive <- load_positive(date_pos) %>%
    prep_positive(filter_lab = FALSE) %>%
    # Remove new NBS from reference data
    dplyr::anti_join(
      dplyr::filter(.data, is_nbs(.data[["pkey"]])),
      by = c("first_name", "last_name", "date_of_birth", "test_date")
    ) %>%
    # Combine phone and ZIP code for matching
    dplyr::mutate(
      .phone_zip_tmp_ = coviData::coalesce_across(c("pnumber", "zip"))
    )

  .data %>%
    # Combine phone and ZIP code for matching
    dplyr::mutate(
      .row_id_tmp_ = dplyr::row_number(),
      .phone_zip_tmp_ = coviData::coalesce_across(c("pnumber", "zip"))
    ) %>%
    # Get records in data with a match in the reference
    dplyr::left_join(
      all_positive,
      by = c("first_name", "last_name", "date_of_birth", ".phone_zip_tmp_"),
      suffix = c("", "_ref_")
    ) %>%
    dplyr::mutate(
      .dupe_tmp_ = (.data[["test_date"]] - .data[["test_date_ref_"]]) %>%
        as.integer() %>%
        is_weakly_less_than(max_dist) %>%
        tidyr::replace_na(FALSE)
    ) %>%
    dplyr::group_by(.data[[".row_id_tmp_"]]) %>%
    dplyr::mutate(duplicate = any(.data[[".dupe_tmp_"]])) %>%
    dplyr::ungroup() %>%
    dplyr::distinct(.data[[".row_id_tmp_"]], .keep_all = TRUE) %>%
    dplyr::select(-dplyr::ends_with("_ref_")) %>%
    as_date_tbl(date = date)
}

is_nbs <- function(pkey) {
  stringr::str_starts(pkey, "NBS") %>%
    tidyr::replace_na(FALSE)
}

acns_school_age <- function(birth_date, current_date, cutoff = "09/30") {

  # Set minimum bound on legal birth dates
  min_date <- lubridate::as_date("1900-01-01")
  # Get invalid `birth_date`
  not_valid <- !((min_date <= birth_date) & (birth_date <= current_date))

  # Define `NA_Date_`
  NA_Date_ <- lubridate::NA_Date_

  # Create `birth_date` with invalid dates replaced with `NA_Date_`
  bdate_valid <- vctrs::vec_assign(birth_date, i = not_valid, value = NA_Date_)

  # Get school starting dates
  school_date <- acns_school_date(current_date, cutoff = cutoff)

  lubridate::interval(bdate_valid, school_date) %>%
    lubridate::as.period(unit = "year") %>%
    lubridate::year() %>%
    as.integer() %>%
    dplyr::between(5L, 18L)
}

acns_school_date <- function(date, cutoff = "09/30") {

  date <- lubridate::as_date(date)

  year <- lubridate::year(date)

  school_date_current <- suppressWarnings(
    paste0(year, cutoff) %>% lubridate::ymd()
  )
  school_date_previous <- suppressWarnings(
    paste0(year-1L, cutoff) %>% lubridate::ymd()
  )

  vctrs::vec_assign(
    school_date_current,
    i = date < school_date_current,
    value = school_date_previous[date < school_date_current]
  )
}

acns_long_term_care <- function(pnumber, addr1, zip) {

  addr <- paste(std_ltcf_addr1(addr1), zip)

  # Reference addresses - {house number} {street} {zip}
  address <- fst::fst(covidsms::ltcf_addr_path) %>%
    extract(c("pm.house", "pm.street", "pm.zip")) %>%
    tidyr::unite("addr", c("pm.house", "pm.street", "pm.zip"), sep = " ") %>%
    dplyr::pull(1L)

  # Reference phone number
  phone <- fst::fst(covidsms::ltcf_phone_path)[[1L]] %>% std_phone()

  # Check whether phone matches long-term care facility
  phone_is_ltcf <- pnumber %in% phone

  # Check whether address matches long-term care facility
  addr1_is_ltcf <- addr %in% address

  # Return whether match was found
  as.logical(phone_is_ltcf + addr1_is_ltcf) %>% tidyr::replace_na(FALSE)
}

std_acns_phone <- function(.data) {
  dplyr::mutate(
    .data,
    pnumber = std_phone(.data[["pnumber"]])
  )
}
