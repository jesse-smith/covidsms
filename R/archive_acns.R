archive_sms <- function(
  path,
  dir = "V:/EPI DATA ANALYTICS TEAM/Mass Texting/ACNS_DAILY_OUT/",
  force = FALSE,
  fail = FALSE
) {
  # Standardize path to file
  path <- path_create(path)

  # Check whether file is empty - this causes an error in vroom() when loading
  # via load_sms_all()
  is_empty <- vctrs::vec_is_empty(coviData::read_file_delim(path))

  # Create new path for archive
  new_file <- fs::path_file(path) %>%
    fs::path_ext_remove() %>%
    paste0("_", acns_date_updated(), if (is_empty) "_EMPTY")
  new_ext <- fs::path_ext(path)
  new_path <- path_create(dir, new_file, ext = new_ext)

  # Check whether file exists
  if (!force && fs::file_exists(new_path)) {
    rlang::warn(
      paste(
        "Archive file already exists; no data was written.",
        "To overwrite the existing file, set `force = TRUE`."
      )
    )
  } else {
    fs::file_copy(
      path = path,
      new_path = new_path,
      overwrite = force
    )
  }

  invisible(new_path)
}
