#' Convert Data to ASCII Character Representation
#'
#' `as_chr_ascii()` converts atomic vectors and data frames to purely ASCII
#' character representation.
#'
#' @param x An object to convert
#'
#' @return `x` as an ASCII-encoded character vector
#'
#' @export
as_chr_ascii <- function(x) {
  UseMethod("as_chr_ascii")
}

#' @rdname as_chr_ascii
#'
#' @export
as_chr_ascii.data.frame <- function(x) {
  dplyr::mutate(x, dplyr::across(dplyr::everything(), ~ to_chr_ascii(.x)))
}

#' @rdname as_chr_ascii
#'
#' @export
as_chr_ascii.logical <- function(x) {
  to_chr_ascii(x)
}

#' @rdname as_chr_ascii
#'
#' @export
as_chr_ascii.integer <- function(x) {
  to_chr_ascii(x)
}

#' @rdname as_chr_ascii
#'
#' @export
as_chr_ascii.double <- function(x) {
  to_chr_ascii(x)
}

#' @rdname as_chr_ascii
#'
#' @export
as_chr_ascii.factor <- function(x) {
  to_chr_ascii(x)
}

#' @rdname as_chr_ascii
#'
#' @export
as_chr_ascii.character <- function(x) {
  to_chr_ascii(x)
}

#' @rdname as_chr_ascii
#'
#' @export
as_chr_ascii.complex <- function(x) {
  to_chr_ascii(x)
}

#' Convert Atomic Vectors to ASCII
#'
#' `to_chr_ascii()` converts atomic vectors to ASCII representation. It powers
#' `as_chr_ascii()`.
#'
#' @param x An atomic vector
#'
#' @return `x` as an ASCII encoded character vector
#'
#' @noRd
to_chr_ascii <- function(x) {
  x %>%
    as.character() %>%
    stringi::stri_trans_general("Any-Latin;Latin-ASCII") %>%
    stringi::stri_enc_toascii()
}
