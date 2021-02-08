#' Path to SMS File
#'
#' @inherit coviData::path_by_date params return
#'
#' @param combined Should the combined ACNS/NBS file be returned? If `TRUE`,
#'   appends `"Combined"` to the end of the directory path.
#'
#' @export
path_sms <- function(
  date = NULL,
  dir = "V:/EPI DATA ANALYTICS TEAM/Mass Texting/ACNS_DAILY_OUT/",
  combined = FALSE
) {

  if (combined) {
    dir <- path_create(dir, "Combined")
    file_stem <- "ACNS_NBS_"
  } else {
    dir <- path_create(dir)
    file_stem <- "ACNS_DAILY_OUT_"
  }
  file_regex <- paste0(".*", file_stem, "{date}[.][^/]+")
  coviData::path_by_date(
    dir = dir,
    date_format = "%Y-%m-%d",
    date_regex = "[0-9]{4}-[0-9]{2}-[0-9]{2}",
    date = date,
    file_regex = file_regex,
    type = "file"
  )
}
