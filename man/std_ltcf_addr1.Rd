% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/parse_ltcf_addr1.R
\name{std_ltcf_addr1}
\alias{std_ltcf_addr1}
\title{Remove ZIP, State, and City from Long-Term Care Facility Address Fields}
\usage{
std_ltcf_addr1(string)
}
\arguments{
\item{string}{Character. Input address vector.}
}
\value{
\code{string} with city, state, and ZIP removed
}
\description{
\code{std_ltcf_addr1()} removes fields provided elsewhere from the \code{ADDR1}
variable in the prepared positive/ACNS file.
}
