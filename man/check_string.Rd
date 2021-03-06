% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils_validation.R
\name{check_string}
\alias{check_string}
\title{Check that a specific list element is a string}
\usage{
check_string(
  obj,
  attr,
  err_h,
  allowed_values = NULL,
  allow_na = FALSE,
  allow_null = FALSE
)
}
\arguments{
\item{obj}{The list object, that should be checked}

\item{attr}{A string holding the name of the list element, that should be
checked.}

\item{err_h}{An error handler.}

\item{allowed_values}{An optional set of allowed strings.}

\item{allow_na}{Are \code{NA} values allowed?}

\item{allow_null}{A logical, defining whether the argument is allowed to be \code{NULL}.}
}
\description{
Check that a specific list element is a string
}
