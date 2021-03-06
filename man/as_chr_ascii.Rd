% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/as_chr_ascii.R
\name{as_chr_ascii}
\alias{as_chr_ascii}
\alias{as_chr_ascii.data.frame}
\alias{as_chr_ascii.logical}
\alias{as_chr_ascii.integer}
\alias{as_chr_ascii.double}
\alias{as_chr_ascii.factor}
\alias{as_chr_ascii.character}
\alias{as_chr_ascii.complex}
\title{Convert Data to ASCII Character Representation}
\usage{
as_chr_ascii(x)

\method{as_chr_ascii}{data.frame}(x)

\method{as_chr_ascii}{logical}(x)

\method{as_chr_ascii}{integer}(x)

\method{as_chr_ascii}{double}(x)

\method{as_chr_ascii}{factor}(x)

\method{as_chr_ascii}{character}(x)

\method{as_chr_ascii}{complex}(x)
}
\arguments{
\item{x}{An object to convert}
}
\value{
\code{x} as an ASCII-encoded character vector
}
\description{
\code{as_chr_ascii()} converts atomic vectors and data frames to purely ASCII
character representation.
}
