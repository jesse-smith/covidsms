prep_acns <- function(.data, date = attr(.data, "date")) {
  .data %>%
    janitor::clean_names() %>%
    dplyr::mutate(
      dplyr::across(where(is.character), stringr::str_squish),
      current_date = std_dates(.data[["test_date"]]) %>%
        dplyr::coalesce(std_dates(.data[["date_added"]])) %>%
        tidyr::replace_na(replace = lubridate::today()),
      from_nbs = stringr::str_starts(.data[["pkey"]], "NBS-"),
      school_age = acns_school_age(
        birth_date = std_dates(.data[["date_of_birth"]]),
        current_date = .data[["current_date"]]
      ),
      long_term_care = acns_long_term_care(
        pnumber = .data[["pnumber"]],
        addr1 = .data[["addr1"]],
        zip = .data[["zip"]]
      ),
      duplicate = acns_duplicate(
        first_name = .data[["first_name"]],
        last_name = .data[["last_name"]],
        dob = .data[["date_of_birth"]]
      ),
      deceased = NA
    ) %>%
    dplyr::select(-"current_date") %>%
    janitor::clean_names(case = "screaming_snake") %>%
    as_date_tbl(date = date)
}

acns_school_age <- function(birth_date, current_date, cutoff = "09/30") {

  # Convert to dates
  birth_date   <- lubridate::as_date(birth_date)
  current_date <- lubridate::as_date(current_date)

  # Set minimum bound on legal birth dates
  min_date <- lubridate::as_date("1900-01-01")

  # Convert invalid birth dates to missing
  birth_date <- dplyr::if_else(
    min_date <= birth_date & birth_date <= current_date,
    birth_date,
    lubridate::as_date(NA)
  )

  # Get date for determining school age
  school_date <- acns_school_date(current_date, cutoff = cutoff)

  # Determine whether is between 5-18 on defined cutoff date
  lubridate::interval(birth_date, school_date) %>%
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

  # Reference addresses - {house number} {street} {zip}
  address <- fst::fst(ltcf_addr_path) %>%
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
  phone <- fst::fst(ltcf_phone_path)[[1L]]

  # Check whether phone matches long-term care facility
  phone_is_ltcf <- pnumber %in% phone

  # Check whether address matches long-term care facility
  addr1_is_ltcf <- std_addr(paste(addr1, zip)) %in% address

  # Return whether match was found
  as.logical(phone_is_ltcf + addr1_is_ltcf) %>% tidyr::replace_na(FALSE)
}

acns_duplicate <- function(first_name, last_name, dob) {
  # Load and get necessary columns
  load_positive() %>%
    dplyr::transmute(
      first_name = std_names(.data[["patient_first_name"]]),
      last_name = std_names(.data[["patient_last_name"]]),
      dob = std_dates(.data[["patient_dob"]]) %>% lubridate::as_date()
    ) %>%
    # Bind input to NBS data
    dplyr::bind_rows(
      dplyr::tibble(
        first_name = std_names(first_name),
        last_name = std_names(last_name),
        dob = std_dates(dob) %>% lubridate::as_date()
      ),
      .id = "id"
    ) %>%
    # Get duplicate numbers
    dplyr::add_count(
      .data[["first_name"]],
      .data[["last_name"]],
      .data[["dob"]],
      name = "n_dupes"
    ) %>%
    # Filter back to input only
    dplyr::filter(.data[["id"]] == 2L) %>%
    # Detect duplicates
    dplyr::transmute(duplicated = .data[["n_dupes"]] >= 1L) %>%
    # Pull out logical vector
    dplyr::pull(1L)
}
