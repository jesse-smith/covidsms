detect_ltcf <- function(.data) {

  ltcf_phones <- fst::fst(ltcf_phone_path) %>%
    dplyr::as_tibble() %>%
    dplyr::pull(1L)

  dplyr::mutate(.data, LONG_TERM_CARE = .data[["PNUMBER"]] %in% ltcf_phones)
}
