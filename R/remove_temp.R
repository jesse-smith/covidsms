#' Remove Temporary Variables from a Dataset
#'
#' This packages uses the convention of naming temporary variables with
#' the pattern `.{semantic_name}_tmp_`. This function removes all variables with
#' such a name.
#'
#' @param .data A data frame or data frame extension
#'
#' @return The data frame with temporary variables removed
remove_temp <- function(.data) {
  dplyr::select(.data, -dplyr::matches(c("^[.].*_tmp_$", "^[.]tmp_$")))
}
