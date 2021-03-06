% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dialr.R
\name{install_dialr}
\alias{install_dialr}
\title{Install dialr and System Dependencies}
\usage{
install_dialr(inst_java = !detect_java(dialr = TRUE), java_dir = "~")
}
\arguments{
\item{inst_java}{Should Java be installed if not already?}

\item{java_dir}{Directory to install OpenJDK}
}
\value{
\code{TRUE} if successful; will error otherwise
}
\description{
\code{install_dialr()} installs dialr and the latest JDK release (if not already
installed)
}
