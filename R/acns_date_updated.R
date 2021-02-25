#' Get Modification Time of ACNS File from SFTP Server
#'
#' @param usr Username for SFTP server
#'
#' @param pwd Password for SFTP server
#'
#' @param path Server path to the ACNS file
#'
#' @return A `Date`
#'
#' @export
acns_date_updated <- function(
  usr = Sys.getenv("sftp_usr"),
  pwd = Sys.getenv("sftp_pwd"),
  path = "ACNS/ACNS_DAILY_OUT.txt"
) {

  handle <- curl::new_handle(
    port = 22L,
    username = usr,
    password = pwd,
    disallow_username_in_url = TRUE,
    default_protocol = "sftp",
    filetime = TRUE,
    nobody = TRUE
  )

  uri <- coviData::path_create("sftp://xfer.shelbycountytn.gov", path)

  response <- curl::curl_fetch_memory(uri, handle)

  status <- response[["status_code"]]

  coviData::assert(
    status == 0L,
    message = paste0("SFTP request failed with cURL status ", status)
  )

  lubridate::as_date(response[["modified"]])
}
