#' Test that an Object is ACSCII Character Encoded
#'
#' `is_chr_ascii()` tests whether an atomic vector or data frame is entirely an
#' ASCII-encoded character.
#'
#' @param x An object to test
#'
#' @param na_rm Should missing values be removed for testing? If `FALSE`, any
#'   missing values will return `NA`
#'
#' @return `TRUE` if the object is ASCII character, `NA` if it includes missing
#' values and `na_rm = FALSE`, otherwise `FALSE`
#'
#' @export
is_chr_ascii <- function(x, na_rm = FALSE) {
  UseMethod("is_chr_ascii")
}

#' @rdname is_chr_ascii
#'
#' @export
is_chr_ascii.data.frame <- function(x, na_rm = FALSE) {
  cols_chr_ascii <- x %>%
    dplyr::summarize(
      dplyr::across(dplyr::everything(), ~ test_chr_ascii(.x, na_rm = na_rm))
    ) %>%
    tidyr::pivot_longer(dplyr::everything()) %>%
    dplyr::pull(.data[["value"]])

  all(cols_chr_ascii)
}

#' @rdname is_chr_ascii
#'
#' @export
is_chr_ascii.default <- function(x, na_rm = FALSE) {
  test_chr_ascii(x, na_rm = na_rm)
}

#' Test that an Atomic Vector is ASCII Character Encoded
#'
#' `test_chr_ascii()` tests that an atomic vector is a character vector and
#' ASCII encoded. It powers `is_chr_ascii()`.
#'
#' @param x An atomic vector
#'
#' @param na_rm Should missing values be removed for testing?
#'
#' @return `TRUE` if the object is ASCII character, `NA` if it includes missing
#' values and `na_rm = FALSE`, otherwise `FALSE`
#'
#' @noRd
test_chr_ascii <- function(x, na_rm = FALSE) {
  all(is.character(x), stringi::stri_enc_isascii(x), na.rm = na_rm)
}
