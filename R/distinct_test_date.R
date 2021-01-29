distinct_test_date <- function(.data) {

  labs <- c("AEL", "BAPTIST", "CCHS", "POPLAR", "UT")

  .data %>%
    dplyr::mutate(
      .row_id_tmp_ = dplyr::row_number(),
      .test_dt_tmp_ = std_dates(.data[["TEST_DATE"]]),
      .dob_tmp_ = std_dates(.data[["DATE_OF_BIRTH"]]),
      .lab_order_tmp_ = !.data[[".lab_tmp_"]] %in% labs
    ) %>%
    dplyr::arrange(.data[[".test_dt_tmp_"]], .data[[".lab_order_tmp_"]]) %>%
    coviData::coalesce_dupes(
      .data[["FIRST_NAME"]],
      .data[["LAST_NAME"]],
      .data[[".dob_tmp_"]],
      .data[[".test_dt_tmp_"]]
    ) %>%
    dplyr::arrange(.data[[".row_id_tmp_"]]) %>%
    dplyr::select(
      -c(".row_id_tmp_", ".test_dt_tmp_", ".dob_tmp_", ".lab_order_tmp_")
    )
}
