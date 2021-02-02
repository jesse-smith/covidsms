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
#' @param date The date that the data was reported. This is extracted from the
#'   `date` attribute of a `date_tbl`.
#'
#' @return Prepared data in `tibble` format
#'
#' @export
prep_acns <- function(.data = download_acns(), date = attr(.data, "date")) {
  .data %>%
    janitor::clean_names() %>%
    dplyr::mutate(
      school_age = acns_school_age(),
      long_term_care = acns_long_term_care(),
      duplicate = acns_duplicate()
    ) %>%
    janitor::clean_names(case = "screaming_snake") %>%
    as_date_tbl(date = date)
}

acns_school_age <- function(data = dplyr::cur_data_all(), cutoff = "09/30") {

  # Set minimum bound on legal birth dates
  min_date <- lubridate::as_date("1900-01-01")

  # Get data
  data <- rlang::eval_tidy(data, env = rlang::env_parent()) %>%
    dplyr::transmute(
      birth_date = std_dates(.data[["date_of_birth"]]) %>% lubridate::as_date(),
      current_date = dplyr::coalesce(
        std_dates(.data[["test_date"]]) %>% lubridate::as_date(),
        std_dates(.data[["date_added"]]) %>% lubridate::as_date(),
        lubridate::today()
      )
    ) %>%
    # Convert invalid birth dates to missing
    dplyr::mutate(
      bdate_test = ({{ min_date }} <= .data[["birth_date"]]) &
        (.data[["birth_date"]] <= .data[["current_date"]]),
      birth_date = dplyr::if_else(
        .data[["bdate_test"]],
        .data[["birth_date"]],
        lubridate::as_date(NA)
      ),
      school_date = acns_school_date(.data[["current_date"]], {{ cutoff }})
    ) %>%
    dplyr::select("birth_date", "school_date")

  # Determine whether is between 5-18 on defined cutoff date
  lubridate::interval(data[["birth_date"]], data[["school_date"]]) %>%
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

acns_long_term_care <- function(data = dplyr::cur_data_all()) {

  # Get data
  data <- rlang::eval_tidy(data, env = rlang::env_parent()) %>%
    dplyr::transmute(
      pnumber = std_phone(.data[["pnumber"]]),
      addr1   = std_ltcf_addr1(.data[["addr1"]]) %>%
        paste(std_zip(.data[["zip"]])) %>%
        std_addr()
    )

  # Reference addresses - {house number} {street} {zip}
  address <- fst::fst(covidsms::ltcf_addr_path) %>%
    extract(c("pm.house", "pm.street", "pm.zip")) %>%
    dplyr::transmute(
      addr1 = paste(
        .data[["pm.house"]],
        .data[["pm.street"]],
        .data[["pm.zip"]]
      ) %>% std_addr()
    ) %>%
    dplyr::pull(1L)

  # Reference phone number
  phone <- fst::fst(covidsms::ltcf_phone_path)[[1L]] %>% std_phone()

  # Check whether phone matches long-term care facility
  phone_is_ltcf <- data[["pnumber"]] %in% phone

  # Check whether address matches long-term care facility
  addr1_is_ltcf <- data[["addr1"]] %in% address

  # Return whether match was found
  as.logical(phone_is_ltcf + addr1_is_ltcf) %>% tidyr::replace_na(FALSE)
}

acns_duplicate <- function(data = dplyr::cur_data_all()) {

  input_data <- rlang::eval_tidy(data, env = rlang::env_parent()) %>%
    janitor::clean_names()

  # Load NBS data
  rlang::inform("Loading and parsing reference data...")
  data <- prep_positive() %>%
    janitor::clean_names() %>%
    dplyr::mutate(
      dplyr::across(
        dplyr::contains("date"),
        ~ std_dates(.x) %>% lubridate::as_date()
      )
    ) %T>%
    {rlang::inform("Binding with input data...")} %>%
    dplyr::bind_rows(input_data, .id = "id") %>%
    # Create and standardize variables
    dplyr::transmute(
      .data[["id"]],
      row_id = dplyr::row_number(),
      first_name = std_names(.data[["first_name"]]),
      last_name = std_names(.data[["last_name"]]),
      birth_date = std_dates(.data[["date_of_birth"]]) %>% lubridate::as_date(),
      test_date = dplyr::coalesce(
        std_dates(.data[["test_date"]]) %>% lubridate::as_date(),
        std_dates(.data[["date_added"]]) %>% lubridate::as_date(),
        lubridate::today()
      ),
      pnumber = std_phone(.data[["pnumber"]]),
      zip = std_zip(.data[["zip"]]),
      phone_zip = dplyr::coalesce(.data[["pnumber"]], .data[["zip"]])
    )  %>%
    dplyr::select(-c("pnumber", "zip")) %T>%
    # Group
    {rlang::inform("Grouping observations...")} %>%
    dplyr::group_by(
      .data[["first_name"]],
      .data[["last_name"]],
      .data[["birth_date"]],
      .data[["phone_zip"]]
    ) %>%
    # Remove groups with no entries from input
    dplyr::filter(any(.data[["id"]] == 2L)) %>%
    # Arrange by group and test date
    dplyr::arrange(.data[["test_date"]], .by_group = TRUE) %>%
    # Get group size
    dplyr::mutate(n_grp = dplyr::n())

  rlang::inform("Searching for duplicates...")
  dupe2 <- data %>%
    dplyr::ungroup() %>%
    dplyr::filter(.data[["n_grp"]] == 2L) %>%
    purrr::when(
      vctrs::vec_size(.) == 0L ~ .,
      ~ dplyr::mutate(., dupe = dist_dupe2(.data[["test_date"]], dist = 180L))
    )

  dupe_n <- data %>%
    dplyr::filter(.data[["n_grp"]] > 2L) %>%
    purrr::when(
      vctrs::vec_size(.) == 0L ~ .,
      ~ dplyr::mutate(., dupe = dist_dupe(.data[["test_date"]], dist = 180L))
    ) %>%
    dplyr::ungroup()

  data %>%
    dplyr::ungroup() %>%
    dplyr::filter(.data[["n_grp"]] <= 1L) %>%
    dplyr::mutate(dupe = FALSE) %>%
    dplyr::bind_rows(dupe2, dupe_n) %>%
    dplyr::filter(.data[["id"]] == 2L) %>%
    dplyr::arrange(.data[["row_id"]]) %>%
    dplyr::pull(.data[["dupe"]])
}

dist_dupe2 <- function(x, dist) {
  x %>%
    as.numeric() %>%
    diff() %>%
    append(NA) %>%
    vctrs::vec_assign(i = seq.int(2L, vctrs::vec_size(x), 2L), value = NA) %>%
    is_weakly_less_than(dist) %>%
    vctrs::vec_fill_missing(direction = "down")
}

dist_dupe <- function(x, dist) {
  purrr::accumulate(as.numeric(x), ~ (if ((.y - .x) > dist) .y else .x)) %>%
    equals(dplyr::lag(.)) %>%
    vctrs::vec_assign(1L, FALSE)
}
