#' `date_tbl`: A `tibble` with a `date` Attribute
#'
#' @description
#' `date_tbl()` is a subclass of `tibble` that stores a `date` attribute as
#' metadata. This is useful, for example, in tracking the report date of a
#' dataset.
#'
#' `as_date_tbl()` converts an object to a `date_tbl`
#'
#' `is_date_tbl()` tests whether an object is a `date_tbl`
#'
#' `new_date_tbl()` constructs a `date_tbl` from a `tibble`-like object and a
#'   date with minimal checking
#'
#' `validate_date_tbl()` validates the properties of a `date_tbl`
#'
#' @param x A `date_tbl`, `tibble`-like object, or an object to test for
#'   inheritance from `date_tbl`. In `as_date_tbl()`, this can be anything
#'   coercable to a `tibble`; `new_date_tbl()` does less work and is
#'   somewhat stricter about the input. `is_date_tbl()` and
#'   `validate_date_tbl()` will take any object, but will return `FALSE` or an
#'   error (respectively) if the object is not a properly created `date_tbl`.
#'
#' @param date A `Date` or datetime (`POSIXt`) object of length 1
#'
#' @inheritParams tibble::as_tibble
#'
#' @inheritParams tibble::is_tibble
#'
#' @inheritParams tibble::new_tibble
#'
#' @return A `date_tbl`
#'
#' @name class-date_tbl
#'
#' @aliases as_date_tbl is_date_tbl new_date_tbl validate_date_tbl
NULL

#' @rdname class-date_tbl
#'
#' @export
as_date_tbl <- function(
  x,
  date,
  .rows = NULL,
  .name_repair = c("check_unique", "unique", "universal", "minimal"),
  rownames = pkgconfig::get_config("tibble::rownames", NULL)
) {
  x %>%
    tibble::as_tibble(
      .rows = .rows,
      .name_repair = .name_repair,
      rownames = rownames
    ) %>%
    new_date_tbl(date = date) %>%
    validate_date_tbl()
}

#' @rdname class-date_tbl
#'
#' @export
is_date_tbl <- function(x) {
  x_class <- class(x)
  class_size <- vctrs::vec_size(x_class)
  date_tbl_superclass <- x_class[(class_size - 3):class_size]
  identical(
    date_tbl_superclass,
    c("date_tbl", "tbl_df", "tbl", "data.frame")
  )
}

#' @rdname class-date_tbl
#'
#' @keywords internal
#'
#' @export
new_date_tbl <- function(x, date, nrow = vctrs::vec_size(x)) {
  tibble::new_tibble(x, date = date, nrow = nrow, class = "date_tbl")
}

#' @rdname class-date_tbl
#'
#' @keywords internal
#'
#' @export
validate_date_tbl <- function(x) {

  # Check class
  if (!is_date_tbl(x)) {
    rlang::abort("`x` must be of class `date_tbl`")
  }

  # Check date attribute
  date <- attr(x, "date", exact = TRUE)
  has_date <- !is.null(date)
  if (!has_date) {
    rlang::abort("`x` must have a `date` attribute")
  }

  # Check date type
  date_is_dt_dttm <- lubridate::is.Date(date) | lubridate::is.POSIXt(date)
  if (!date_is_dt_dttm) {
    rlang::abort("`date` attribute must be a `Date` or datetime")
  }

  # Check date range
  date_range_is_valid <- all(
    lubridate::as_date("2020-01-01") <= date,
    date <= lubridate::today()
  )
  if (!date_range_is_valid) {
    rlang::abort(
      paste0(
        "`date` must be between 2020-01-01 and today (",
        lubridate::today(),
        ")"
      )
    )
  }

  # Validate tibble
  tibble::validate_tibble(x)
}
