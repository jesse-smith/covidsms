
filter_by_new <- function(.data, filter = TRUE, date = attr(.data, "date")) {
  if (filter) dplyr::filter(.data, .data[["date_added"]] == date) else .data
}
