#' Translate Positive Case Variables to ACNS
#'
#' `translate_positive()` replaces columns from positive NBS data with their
#' equivalents in the ACNS file and drops any un-mapped columns.
#' This is not always a 1-to-1 mapping.
#'
#' This function is memoised; it will cache return values and load them rather
#' than re-processing the same input data.
#'
#' @param .data A data frame containing positive NBS data (as `character`)
#'
#' @return The input data with columns mapped to their ACNS counterparts
#'
#' @export
translate_positive <- function(.data) {

  # List of ACNS column - NBS column mappings. Additions/deletions here must
  # be accompanied by a change to the 'translation' code below.
  p <- list(
    date_added = "report_date",
    pkey = "lab_local_id",
    test_date = "spec_date_only",
    first_name = "patient_first_name",
    last_name = "patient_last_name",
    dob = "patient_dob",
    sex = "patient_current_sex",
    pnumber = c("patient_tel_cell", "patient_tel_home", "patient_phone_work"),
    addr1 = "patient_street_addr_1",
    addr2 = "patient_street_addr_2",
    city = "patient_city",
    state = "patient_state",
    zip = "patient_zip",
    id = "inv_local_id",
    lab = "perform_facility_name"
  )

  # Perform 'translation'; any addition above must be accompanied by an
  # addition to this code to actually 'translate' the data.
  .data %>%
    dplyr::transmute(
      date_added = std_dates(.data[[p$date_added]]) %>% lubridate::as_date(),
      pkey = pkey_nbs(.data[[p$pkey]]),
      result = "POSITIVE",
      test_date = std_dates(.data[[p$test_date]]) %>% lubridate::as_date(),
      first_name = std_names(.data[[p$first_name]]),
      last_name = std_names(.data[[p$last_name]]),
      date_of_birth = std_dates(.data[[p$dob]]) %>% lubridate::as_date(),
      sex = std_names(.data[[p$sex]]),
      pnumber = dplyr::across(p$pnumber, std_phone) %>%
        dplyr::transmute(ph = coviData::coalesce_across(p$pnumber)) %>%
        dplyr::pull("ph"),
      addr1 = std_addr(.data[[p$addr1]]),
      addr2 = std_addr(.data[[p$addr2]]),
      city = std_city(.data[[p$city]]),
      state = std_state(.data[[p$state]]),
      zip = std_zip(.data[[p$zip]]),
      .inv_id_tmp_ = .data[[p$id]],
      .lab_tmp_ = .data[[p$lab]] %>%
        std_lab_names() %>%
        std_labs_ael() %>%
        std_labs_baptist() %>%
        std_labs_poplar() %>%
        std_labs_ut() %>%
        std_labs_labcorp() %>%
        std_labs_quest() %>%
        std_labs_methodist() %>%
        std_labs_mm() %>%
        std_labs_mmg()
    )
}
