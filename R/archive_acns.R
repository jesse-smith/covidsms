archive_sms <- function(
  path,
  dir = "V:/EPI DATA ANALYTICS TEAM/Mass Texting/ACNS_DAILY_OUT/",
  force = FALSE,
  fail = FALSE
) {
  # Standardize path to file
  path <- path_create(path)

  # Get latest date added to use as date in filename
  date <- path %>%
    coviData::read_file_delim(
      col_select = "DATE_ADDED",
      col_types = vroom::cols(DATE_ADDED = vroom::col_date(format = "%m/%d/%Y"))
    ) %>%
    dplyr::pull(1L) %>%
    max(na.rm = TRUE)

  # Create new path for archive
  new_file <- fs::path_file(path) %>%
    fs::path_ext_remove() %>%
    paste0("_", date)
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
