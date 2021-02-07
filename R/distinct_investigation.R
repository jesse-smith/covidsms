#' Deduplicate Investigations
#'
#' `distinct_investigations()` coalesces duplicate investigations down to a
#'  single record.
#'
#' This function is memoised; it will cache return values and load them rather
#' than re-processing the same input data.
#'
#' @param .data Input data from `translate_positive()`
#'
#' @return Deduplicated data
distinct_investigation <- function(.data) {

  labs <- c("AEL", "BAPTIST", "CCHS", "POPLAR", "UT")

  .data %>%
    dplyr::mutate(
      .row_id_tmp_ = dplyr::row_number(),
      .rpt_dt_tmp_ = std_dates(.data[["date_added"]]) %>% lubridate::as_date(),
      .test_dt_tmp_ = std_dates(.data[["test_date"]]) %>% lubridate::as_date(),
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
