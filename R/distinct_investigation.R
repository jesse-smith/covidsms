distinct_investigation <- function(.data) {

  labs <- c("AEL", "BAPTIST", "CCHS", "POPLAR", "UT")

  .data %>%
    dplyr::mutate(
      .row_id_tmp_ = dplyr::row_number(),
      .rpt_dt_tmp_ = std_dates(.data[["DATE_ADDED"]]),
      .test_dt_tmp_ = std_dates(.data[["TEST_DATE"]]),
      .lab_order_tmp_ = !.data[[".lab_tmp_"]] %in% labs
    ) %>%
    dplyr::arrange(
      .data[[".inv_id_tmp_"]],
      .data[[".rpt_dt_tmp_"]],
      .data[[".test_dt_tmp_"]],
      .data[[".lab_order_tmp_"]]
    ) %>%
    coviData::coalesce_dupes(.data[[".inv_id_tmp_"]]) %>%
    dplyr::arrange(.data[[".row_id_tmp_"]]) %>%
    dplyr::select(
      -c(
        ".row_id_tmp_",
        ".rpt_dt_tmp_",
        ".test_dt_tmp_",
        ".lab_order_tmp_",
        ".inv_id_tmp_"
      )
    )
}
