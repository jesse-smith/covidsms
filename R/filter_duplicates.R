filter_positive_duplicates <- function(.data, days = 180L) {

  dates <- c("specimen_coll_dt", "inv_start_dt", "report_date")

  phone_cols <- c("patient_tel_cell", "patient_tel_home", "patient_phone_work")

  delay <- suppressMessages(
    coviData::load_report_date() %>%
      dplyr::as_tibble() %>%
      dplyr::mutate(
        dplyr::across(dplyr::ends_with("_date"), ~ lubridate::as_date(.x))
      ) %>%
      covidModel::estimate_delay() %>%
      dplyr::pull("delay") %>%
      as.double() %>%
      divide_by(0.9) %>%
      ceiling() %>%
      as.integer()
  )


  start <- lubridate::as_date("2020-03-05")
  today <- lubridate::today()
  NA_Date_ <- lubridate::NA_Date_

  is_valid_date <- rlang::new_function(
    args = rlang::pairlist2(.x = ),
    body = rlang::expr(dplyr::between(.x, !!start, !!today))
  )

  rm_date <- rlang::new_function(
    args = rlang::pairlist2(.x = ),
    body = rlang::expr(dplyr::if_else(is_valid_date(.x), .x, !!NA_Date_))
  )

  clean_date <- rlang::new_function(
    args = rlang::pairlist2(.x = ),
    body = rlang::expr(std_dates(.x) %>% lubridate::as_date() %>% rm_date())
  )

  data_all <- .data %>%
    dplyr::mutate(
      .row_id_tmp_ = dplyr::row_number(),
      .first_name_tmp_ = std_names(.data[["patient_first_name"]]),
      .last_name_tmp_ = std_names(.data[["patient_last_name"]]),
      .dob_tmp_ = std_dates(.data[["patient_dob"]]) %>% lubridate::as_date(),
      .phone_tmp_ = dplyr::across({{ phone_cols }}, std_phone) %>%
        dplyr::transmute(ph = coviData::coalesce_across({{ phone_cols }})) %>%
        dplyr::pull("ph"),
      .dt_tmp_ = dplyr::across({{ dates }}, ~ clean_date(.x)) %>%
        dplyr::transmute(dt = coviData::coalesce_across({{ dates }})) %>%
        dplyr::pull("dt"),
      .within_days_tmp_ = .data[[".dt_tmp_"]] > today - (days + delay)
    ) %>%
    dplyr::arrange(dplyr::desc(.data[[".dt_tmp_"]]))
  remove(.data)

  data_dedup <- data_all %>%
    dplyr::filter(dplyr::cumall(.data[[".within_days_tmp_"]])) %>%
    dplyr::distinct(
      .data[[".first_name_tmp_"]],
      .data[[".last_name_tmp_"]],
      .data[["dob_tmp_"]],
      .data[["phone_tmp_"]],
      .keep_all = TRUE
    )

  data_all %>%
    dplyr::filter(dplyr::cumany(!.data[[".within_days_tmp_"]])) %>%
    dplyr::bind_rows(data_dedup) %>%
    dplyr::arrange(.data[[".row_id_tmp_"]]) %>%
    dplyr::select(
      -c(".row_id_tmp_"),
      -c(".first_name_tmp_", ".last_name_tmp_", ".dob_tmp_", ".phone_tmp_"),
      -c(".dt_tmp_", ".within_days_tmp_")
    )
}

detect_duplicates <- function(
  .data,
  ...,
  col_name = ".duplicate",
  count = FALSE
) {

  cols <- coviData::select_colnames(.data, !!!...) %>% rlang::syms()

  .data %>%
    dplyr::add_count(!!!cols, name = ".n_duplicates") %>%
    dplyr::mutate(
      {{ col_name }} := .data[[".n_duplicates"]] > 1L,
      .before = ".n_duplicates"
    ) %>%
    purrr::when(count ~ ., ~ dplyr::select(., -".n_duplicates"))
}
