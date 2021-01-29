assert_string <- function(string, pattern, arg = NULL, na_rm = FALSE) {

  if (is.null(arg)) {
    arg <- rlang::enexpr(string) %>% rlang::as_label()
  }

  assert_all(
    is.character(string),
    message = paste(arg, "must be a character vector"),
    class = "error_assert_string"
  )

  if (na_rm) {
    string_na <- na.omit(string)
  } else {
    string_na <- string
  }

  valid <- stringr::str_detect(string_na, pattern = pattern)

  if (!rlang::is_true(all(valid))) {
    rlang::abort(
      paste0(
        "All values in `", arg, "` must match the regular expression",
        "\n\n",
        pattern,
        "\n\n",
        "The following values are invalid:\n",
        rlang::format_error_bullets(set_names(string[!valid], "x"))
      ),
      class = "error_assert_string"
    )
  } else {
    string
  }
}
