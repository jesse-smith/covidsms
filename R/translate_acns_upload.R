#' Translate ACNS Data into Format Upload-able to the ACNS System
#'
#' @param .acns Prepared ACNS data
#'
#' @return ACNS data translated into the format needed by the ACNS system
#'
#' @export
translate_acns_upload <- function(.acns) {
  .acns %>%
    dplyr::filter(!.data[["duplicate"]], !.data[["long_term_care"]]) %>%
    dplyr::mutate(
      dplyr::across(where(lubridate::is.Date), ~ format(.x, "%m/%d/%Y"))
    ) %>%
    dplyr::select(-where(is.logical), -"nbs") %>%
    janitor::clean_names(case = "screaming_snake")
}
