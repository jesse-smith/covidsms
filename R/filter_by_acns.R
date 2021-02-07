filter_by_acns <- function(
  .data,
  filter = TRUE,
  excl_last = FALSE,
  date = attr(.data, "date")
) {

  if (!filter) return(.data)

  dplyr::anti_join(
    .data,
    load_sms_all(excl_last = excl_last) %>% std_sms(),
    by = c("first_name", "last_name", "date_of_birth", "test_date")
  )
}
