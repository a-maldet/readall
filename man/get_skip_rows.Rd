% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/file_structure_methods.R
\name{get_skip_rows}
\alias{get_skip_rows}
\alias{get_skip_rows.file_structure_fwf}
\alias{get_skip_rows.file_structure_dsv}
\alias{get_skip_rows.file_structure_excel}
\alias{get_skip_rows.file_structure_sas}
\title{Helper function for \code{read_data()}: Calculate the skipped rows}
\usage{
get_skip_rows(obj, ...)

\method{get_skip_rows}{file_structure_fwf}(obj, ...)

\method{get_skip_rows}{file_structure_dsv}(obj, ...)

\method{get_skip_rows}{file_structure_excel}(obj, ...)

\method{get_skip_rows}{file_structure_sas}(obj, ...)
}
\arguments{
\item{obj}{A \link[=new_file_structure_fwf]{file_structure} class or a
\link[=new_file_definition]{file_definition} class object, that holds all informations needed for
reading a specific data set.}

\item{...}{Optional error handler}
}
\description{
Currently there is nothing to calculate since \code{skip} is returned as it is,
no matter what values are in \code{header} and \code{col_names}.
}
