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
#'   ranges? The default uses dialr if it is installed and working.
#'
#' @param quiet Should the progress bar from dialr be shown? Ignored if
#'   `dialr = FALSE`.
#'
#' @return A `character` vector of standardized phone numbers
#'
#' @export
std_phone <- function(x, dialr = test_dialr(), quiet = TRUE) {

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

#' Test that the dialr Package Works
#'
#' dialr depends on the libphonenumber Java library and requires a working
#' Java Development Kit installation with environment variables set
#' appropriately. This function checks that dialr works and attempts to provide
#' informative messages about why it may not.
#'
#' @param quiet Should warnings be shown if dialr does not work? These will only
#'   be shown once per session.
#'
#' @return A boolean indicating whether dialr is functioning properly
#'
#' @export
test_dialr <- function(quiet = FALSE) {
  # Test that dialr works; if it does, don't perform specific checks
  sink <- try(
    utils::capture.output({test <- dialr::phone("9012222000", region = "US")}),
    silent = TRUE
  )
  passed <- !rlang::inherits_any(test, "try-error")
  if (passed || quiet) return(passed)

  # Test that dialr is installed
  if (!rlang::is_installed("dialr")) {
    rlang::warn(
      "dialr will not be used to parse phone numbers b/c it is not installed",
      .frequency = "once",
      .frequency_id = "dialr_not_installed"
    )
    return(FALSE)
  }

  # Check JAVA_HOME and PATH environment variables
  if (rlang::is_empty(Sys.getenv("JAVA_HOME"))) {
    rlang::warn(
      paste(
        "dialr will not be used to parse phone numbers b/c",
        "the `JAVA_HOME` environment variable is not set.",
        "The `JAVA_HOME` environment variable must contain the path",
        "to a Java Development Kit installation"
      ),
      .frequency = "once",
      .frequency_id = "JAVA_HOME_not_set"
    )
    return(FALSE)
  } else if (!stringr::str_detect(Sys.getenv("PATH"), "jdk[^;]*bin")) {
    rlang::warn(
      paste(
        "dialr will not be used to parse phone numbers b/c",
        "the `PATH` environment variable does not contain a JDK bin.",
        "The `PATH` environment variable must contain the path",
        "to a Java Development Kit installation's bin folder"
      ),
      .frequency = "once",
      .frequency_id = "path_missing_jdk_bin"
    )
    return(FALSE)
  }

  # If failed for another reason, print and return `FALSE`
  if (!passed) {
    msg <- stringr::str_replace(
      as.vector(test),
      "^Error",
      replacement = "Warning (converted from error)"
    )
    rlang::warn(msg, .frequency = "once", .frequency_id = "dialr_failed_other")
  }
  passed
}
