.onLoad <- function(libname, pkgname) {

  # Read functions
  read_inv_positive <<- mem_if_not(read_inv_positive)
  read_pcr_positive <<- mem_if_not(read_pcr_positive)

  # `prep_positive()` sub-functions
  translate_positive <<- mem_if_not(translate_positive)
  distinct_investigation <<- mem_if_not(distinct_investigation)
}

#' Memoise a Function if Not Already Done
#'
#' @param f The function to memoise
#'
#' @param ... Additional arguments to pass to `memoise()`
#'
#' @return The memoised function
mem_if_not <- function(f, ...) {
  # Do nothing if the memoise package isn't installed
  if (!requireNamespace("memoise", quietly = TRUE)) return(f)

  # Memoise the function if it isn't already
  if (memoise::is.memoised(f)) f else memoise::memoise(f, ...)
}


#' Memoised `read_file_delim()` Wrappers
#'
#' These memoised wrappers are used to speed up repeated loading of the same
#' files. They only re-load the file if an argument has changed. This allows
#' functions like `load_inv_positive()` to run *much* more quickly when
#' subsequent calls with the same arguments are made.
#'
#' @param path The path to the file to read
#'
#' @param ... Additional arguments to pass to `read_file_delim()`
#'
#' @return A `tibble` containing the loaded data
#'
#' @name read-memoised
#'
#' @aliases read_inv_positive read_pcr_positive
NULL

#' @rdname read-memoised
read_inv_positive <- function(path, ...) {
  coviData::read_file_delim(path, ...)
}

#' @rdname read-memoised
read_pcr_positive <- function(path, ...) {
  coviData::read_file_delim(path, ...)
}

#' Memoised `prep_positive()` Functions
#'
#' These functions are called repeatedly throughout the pipeline, with the same
#' inputs, and are relatively expensive to run. Memoisation significantly speeds
#' up repeat calls.
#'
#' @param .data Input data frame
#'
#' @return Processed `.data`
#'
#' @name prep_positive-memoised
NULL
