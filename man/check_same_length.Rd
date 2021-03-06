% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils_validation.R
\name{check_same_length}
\alias{check_same_length}
\title{Check that two specific list elements have the same vector length}
\usage{
check_same_length(obj, attr1, attr2, err_h, allow_null = FALSE)
}
\arguments{
\item{obj}{The list object, that should be checked}

\item{attr1}{First attribute to compare length with.}

\item{attr2}{Second attribute to compare length with.}

\item{err_h}{An error handler.}

\item{allow_null}{A logical, defining whether the argument is allowed to be \code{NULL}.}
}
\description{
Check that two specific list elements have the same vector length
}
