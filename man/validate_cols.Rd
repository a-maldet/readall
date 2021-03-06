% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cols.R
\name{validate_cols}
\alias{validate_cols}
\title{validate argument \code{cols} in \verb{new_file_structure_*()}}
\usage{
validate_cols(cols, file_type, sep_width = NULL, err_h)
}
\arguments{
\item{cols}{A list object, where each entry represents a column definition.
Each element of the list, is a list itself with possible entries:
\itemize{
\item \code{name}: Column name (optional)
\item \code{type}: Colunn type (required)
\item \code{start}: Column start postition (optional and only for FWF)
\item \code{end}: Column end postition (optional and only for FWF)
\item \code{width}: Column width (optional and only for FWF)
}}

\item{file_type}{A character string holding the current file_type.}

\item{sep_width}{An optional string argument, defining the given \code{sep_width}
argument in the FWF-Specification. If the file type is not FWF than it
can be omitted.}

\item{err_h}{An error handler.
find first column that is not skipped}
}
\description{
validate argument \code{cols} in \verb{new_file_structure_*()}
}
