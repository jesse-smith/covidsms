#' Upload Positives in NBS to ACNS Data
#'
#' `upload_positive()` uploads data from `prep_positive()` to the ACNS SFTP
#' site for inclusion in the following day's ACNS_DAILY_OUT file.
#'
#' @param .data A `tibble` from `prep_positive()`
#'
#' @param path The location to save the data on the SFTP server
#'
#' @param usr The username for the SFTP server
#'
#' @param pwd The password for the SFTP server
#'
#' @return `.data` (invisibly)
#'
#' @export
upload_positive <- function(
  .data = prep_positive(),
  path  = "nbs_cumulative.xlsx",
  usr = Sys.getenv("acns_usr"),
  pwd = Sys.getenv("acns_pwd")
) {

  # Check that `.data` is a data frame
  coviData::assert_dataframe(.data)

  # Check that `.data` is formatted correctly
  validate_sms_data(.data)

  # Standardize remote directory path
  path <- path %>% fs::path_norm() %>% fs::path_tidy()
  # Get file name
  file_name <- fs::path_file(path)
  # Validate that file is xlsx
  ext <- path %>% fs::path_ext() %>% stringr::str_to_lower()
  if (ext != "xlsx") {
    rlang::abort("The file specified by `path` must be an 'xlsx' file")
  }

  # Create SFTP connection details
  sftp_con <- coviData::sftp_connect(
    server = "xfer.shelbycountytn.gov",
    folder = fs::path_dir(path),
    username = usr,
    password = pwd
  )

  # Save `.data` as a temporary csv file
  tmp_dir <- fs::file_temp() %>% fs::dir_create()
  tmp <- path_create(tmp_dir, file_name)
  openxlsx::write.xlsx(.data, file = tmp)
  on.exit(fs::file_delete(tmp), add = TRUE)

  coviData::sftp_upload(
    file = file_name,
    fromfolder = tmp_dir,
    sftp_connection = sftp_con
  )

  .data
}
