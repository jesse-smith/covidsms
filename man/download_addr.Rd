% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/download_acns.R
\name{download_addr}
\alias{download_addr}
\title{Download Address File from SFTP Server}
\usage{
download_addr(
  path = "ACNS_ADDRESS_SAMPLE.csv",
  usr = Sys.getenv("acns_usr"),
  pwd = Sys.getenv("acns_pwd"),
  server = "xfer.shelbycountytn.gov",
  ...
)
}
\arguments{
\item{path}{Path to file to download on SFTP server}

\item{usr}{Username for SFTP server}

\item{pwd}{Password for SFTP server}

\item{server}{Server address}

\item{...}{Additional arguments to pass to \code{read_file()}}
}
\value{
A \code{tibble} containing data from the ACNS address file
}
\description{
\code{download_addr()} downloads and loads the address file from the SFTP server.
Date columns are returned as dates; all others are returned as character. All
column names are passed through \code{janitor::clean_names()}.
}
