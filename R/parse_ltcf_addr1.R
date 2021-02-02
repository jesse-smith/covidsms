#' Parse `ADDR1` Field to House Number + Street Name
#'
#' `parse_ltcf_addr1()` parses out house numbers and street names (excluding
#' directionals and suffixes) from the `ADDR1` field in positive/ACNS data.
#'
#' @param .data A data frame or data frame extension
#'
#' @param .col `<tidy-select>` The column containing address information. This
#'   must match only one column, which must be character
#'
#' @return `.data` with the added column `.addr1_tmp_` containing the parsed
#'   address string
#'
#' @export
parse_ltcf_addr1 <- function(.data, .col = "ADDR1") {

  # Get and check column name for address
  col_nm <- coviData::select_colnames(.data, {{ .col }})
  coviData::assert_cols(.data, {{ col_nm }}, ptype = "character", n = 1L)

  dir_dict <- postmastr::pm_dictionary(type = "directional")
  suf_dict <- postmastr::pm_dictionary(type = "suffix")

  # Identify unique address strings to join back to `.data` later
  # Remove city, state, and ZIP with `std_ltcf_addr1()`
  data_id <- .data %>%
    dplyr::mutate(.ltcf_addr1_tmp_ = std_ltcf_addr1(.data[[col_nm]])) %>%
    postmastr::pm_identify(.ltcf_addr1_tmp_)

  # Parse short address
  data_id %>%
    postmastr::pm_prep(.ltcf_addr1_tmp_, type = "street") %>%
    postmastr::pm_house_parse() %>%
    postmastr::pm_streetDir_parse(dictionary = dir_dict) %>%
    postmastr::pm_streetSuf_parse(dictionary = suf_dict) %>%
    postmastr::pm_street_parse() %>%
    # Create new house + street variable from parsed address
    dplyr::transmute(
      .data[["pm.uid"]],
      .addr1_tmp_ = std_addr(paste(.data[["pm.house"]], .data[["pm.street"]]))
    ) %>%
    # Join back to main dataset and clean up
    dplyr::right_join(data_id, by = "pm.uid") %>%
    dplyr::select(-dplyr::starts_with("pm.")) %>%
    dplyr::mutate(.ltcf_addr1_tmp_ = .data[[".addr1_tmp_"]]) %>%
    dplyr::select(-".addr1_tmp_")
}

#' Remove ZIP, State, and City from Long-Term Care Facility Address Fields
#'
#' `std_ltcf_addr1()` removes fields provided elsewhere from the `ADDR1`
#' variable in the prepared positive/ACNS file.
#'
#' @param string Character. Input address vector.
#'
#' @return `string` with city, state, and ZIP removed
#'
#' @export
std_ltcf_addr1 <- function(string) {

  ltcf_cities <- fst::fst(covidsms::ltcf_addr_path)[["pm.city"]] %>%
    unique() %>%
    sort() %>%
    paste0(collapse = "|")

  ltcf_cities_pattern <- stringr::str_glue("({ltcf_cities})$")

  string %>%
    stringr::str_squish() %>%
    stringr::str_remove("[0-9 ]+$") %>%
    stringr::str_squish() %>%
    stringr::str_remove("(TN|TENN|TENNESSEE)$") %>%
    stringr::str_squish() %>%
    stringr::str_remove(ltcf_cities_pattern) %>%
    stringr::str_squish()
}
