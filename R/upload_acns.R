#' Upload ACNS File to SFTP Server
#'
#' `upload_acns()` validates and uploads ACNS data to the Shelby County SFTP
#' server.
#'
#' @param .data ACNS data in a data frame or data frame extension
#'
#' @param path The location to save the file on the SFTP server
#'
#' @param usr The username for the SFTP server
#'
#' @param pwd The password for the SFTP server
#'
#' @return The input data (invisibly)
#'
#' @export
upload_acns <- function(
  .data,
  path = "ACNS/ACNS_DAILY_PARSED.csv",
  usr = Sys.getenv("sftp_usr"),
  pwd = Sys.getenv("sftp_pwd")
) {
  # Check that `.data` is a data frame
  if (!is.data.frame(.data)) {
    rlang::abort("`.data` must be a data frame or data frame extension")
  }

  sms_data <- .data %>%
    # Remove duplicates and long-term care
    dplyr::filter(
      !dplyr::across(dplyr::matches("^duplicate$")),
      !dplyr::across(dplyr::matches("^long_term_care$"))
    ) %>%
    dplyr::select(
      c(
        c("DATE_ADDED", "RESULT", "TEST_DATE"),
        c("FIRST_NAME", "LAST_NAME", "DATE_OF_BIRTH", "SEX", "PNUMBER")
      )
    )

  # Check that `.data` is formatted correctly
  validate_sms_data(sms_data, type = "acns")

  # Standardize remote directory path
  path <- path %>% fs::path_norm() %>% fs::path_tidy()
  # Get file name
  file_name <- fs::path_file(path)
  # Validate that file is csv
  ext <- path %>% fs::path_ext() %>% stringr::str_to_lower()
  if (ext != "csv") {
    rlang::abort("The file specified by `path` must be a 'csv' file")
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
  vroom::vroom_write(sms_data, delim = ",", path = tmp)
  on.exit(fs::file_delete(tmp), add = TRUE)

  coviData::sftp_upload(
    file = file_name,
    fromfolder = tmp_dir,
    sftp_connection = sftp_con
  )

  invisible(.data)
}
