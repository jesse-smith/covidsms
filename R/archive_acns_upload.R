#' Archive Upload Data for ACNS System
#'
#' @param .data Data from `translate_acns_upload()`
#'
#' @param dir The archive directory
#'
#' @param date The date on the filename
#'
#' @return `.data` (invisibly)
#'
#' @export
archive_acns_upload <- function(
  .data,
  dir = "V:/EPI DATA ANALYTICS TEAM/Mass Texting/ACNS_DAILY_OUT/Combined/",
  date = attr(.data, "date")
) {

  path <- path_create(dir, paste0("ACNS_NBS_", date), ext = "csv")

  vroom::vroom_write(
    .data,
    path = path,
    delim = ",",
    na = ""
  )

  invisible(.data)
}
