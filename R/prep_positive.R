#' Transform NBS Data to Format Necessary for Exporting to ACNS File
#'
#' `prep_positive()` transforms data from NBS snapshot files to the
#' format needed for exports to the ACNS process.
#'
#' @param .data Data from NBS snapshot file
#'
#' @param filter_lab Should records from labs reported through the ACNS process
#'   be filtered out?
#'
#' @param labs Character vector of labs to filter; the default is all labs
#'   reporting directly to SCHD
#'
#' @param filter_new Should only new positives be returned? These are still
#'   de-duplicated across the full NBS dataset.
#'
#' @param filter_acns Should duplicates from previous ACNS files be removed?
#'
#' @param date The `date` attribute for the resulting `date_tbl`
#'
#' @return A `date_tbl`, which is a `tibble` with a `date` attribute
#'
#' @export
prep_positive <- function(
  .data = load_positive(),
  filter_lab = TRUE,
  labs = c("AEL", "BAPTIST", "CCHS", "POPLAR", "UT"),
  filter_new = FALSE,
  filter_acns = filter_new,
  date = attr(.data, "date")
) {

  .data %>%
    filter_by_residence() %>%
    filter_by_death() %>%
    translate_positive() %>%
    distinct_investigation() %>%
    distinct_test_date() %>%
    filter_by_lab(filter = filter_lab, labs = labs) %>%
    filter_by_new(filter = filter_new, date = date) %>%
    filter_by_acns(filter = filter_acns, excl_last = TRUE, date = date) %>%
    remove_temp() %>%
    as_date_tbl(date = date)
}

#' Create Primary Key from an NBS ID
#'
#' `pkey_nbs()` creates a primary key of the format "{NBS}-{ID_NUMBER}" from
#' an 8-digit unique identifier in NBS. For labs, this is the `lab_local_id`;
#' for investigations, this is `inv_local_id`; for people, this *should* be
#' `patient_local_id`, though many people have multiple `patient_local_id`s.
#'
#' @param string A character vector of NBS IDs
#'
#' @return A character vector of primary keys
pkey_nbs <- function(string) {
  string %>%
    stringr::str_extract("[0-9]{8}") %>%
    {paste0("NBS-0", .)}
}
