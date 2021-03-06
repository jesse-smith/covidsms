% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/filter_by_death.R
\name{filter_by_death}
\alias{filter_by_death}
\title{Filter Deaths from Positive NBS Tests}
\usage{
filter_by_death(.data)
}
\arguments{
\item{.data}{Data from which to remove deaths}
}
\value{
\code{.data} with known deaths removed
}
\description{
\code{filter_by_death()} removes deaths from positive test results to avoid
attempts to text a deceased individual.
}
