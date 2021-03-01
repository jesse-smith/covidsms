#' Standardize Phone Numbers
#'
#' `std_phone()` standardizes North American phone numbers to a 10-digit
#' national format. Invalid or non-North American numbers are converted to `NA`.
#' Note that `std_phone()` only check for truly valid number ranges when
#' `dialr = TRUE`; otherwise, all 10-digit numbers are considered valid.
#'
#' @param x A vector convertible to `character` format containing phone numbers
#'
#' @param dialr Should the `dialr` package be used to check for valid number
#'   ranges? The default uses dialr if it is installed.
#'
#' @param quiet Should the progress bar from dialr be shown? Ignored if
#'   `dialr = FALSE`.
#'
#' @return A `character` vector of standardized phone numbers
#'
#' @export
std_phone <- function(x, dialr = rlang::is_installed("dialr"), quiet = TRUE) {

  extraction_pattern <- "^([+]?0?1)?[0-9]{10}"
  removal_pattern <- "^([+]?0?1)(?=[0-9]{10})"

  ph <- x %>%
    as.character() %>%
    stringr::str_remove_all(pattern = "[^0-9+]") %>%
    stringr::str_extract(pattern = extraction_pattern) %>%
    stringr::str_remove(pattern = removal_pattern)

  if (dialr) {
    coviData::assert_all(
      rlang::is_installed("dialr"),
      message = "The `dialr` package must be installed to use this feature"
    )
  }

  if (dialr && quiet) {
    # Wrap dialr in expression to conditionally prevent status updates
    ph_expr <- rlang::expr({
      ph_out <- format(
        dialr::phone(ph, region = "US"),
        format = "NATIONAL",
        clean = TRUE,
        strict = TRUE
      )
    })
    # Evaluate expression; saves output to `ph_out`
    invisible(utils::capture.output(eval(ph_expr)))
    ph_out
  } else if (dialr) {
    format(
      dialr::phone(ph, region = "US"),
      format = "NATIONAL",
      clean = TRUE,
      strict = TRUE
    )
  } else {
    ph
  }
}
