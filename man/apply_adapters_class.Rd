% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/adapters.R
\name{apply_adapters_class}
\alias{apply_adapters_class}
\title{Apply the adapter-functions stored in an \link[=new_adapters]{adapters} class object to a data.frame}
\usage{
apply_adapters_class(data, adapters)
}
\arguments{
\item{data}{The data.frame, which should be modified}

\item{adapters}{An \link[=new_adapters]{adapters} class object holding the adapter
functions, which should be applied to the data.frame.}
}
\value{
The modified data.frame
}
\description{
Apply the adapter-functions stored in an \link[=new_adapters]{adapters} class object to a data.frame
}
\section{adapters}{

An adapter function is a function that takes a data.frame as input argument
and returns a modified version of this data.frame.
The adapter functions are stored in an \link[=new_adapters]{adapters}
class object, which is a special list that contains all adapter functions
and a description text of each function. This class objects can be
created by using the function \code{\link[=new_adapters]{new_adapters()}}.
The \link[=new_adapters]{adapters} class objects can be added to a
\link[=new_file_structure_fwf]{file_structure} or a
\link[=new_file_definition]{file_definition} or a \link[=new_file_collection]{file_collection} class object.
After reading a data file (by calling \link[=read_data]{read_data(file_definition)})
all adapter functions listed in the \code{adapters} argument of the
file_definition]\code{\link[=new_file_definition]{new_file_definition()}} class object
will be applied consecutively to the loaded data set.
Adapter functions can be added to an existing
\link[=new_file_structure_fwf]{file_structure} or a \link[=new_file_definition]{file_definition} or
a \link[=new_file_collection]{file_collection} class
object by using the function \code{\link[=add_adapters]{add_adapters()}}.
Adapter functions can be used for several tasks:
\itemize{
\item adapt the data sets in such a way that they can be concatenated for
mutliple years
\item compute new variables from existing variables
\item fix errors in variables
\item transform the values of a variable of an older data set, such that it
complies with a newer variable definition
}
}

