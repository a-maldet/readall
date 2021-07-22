#' @import magrittr
#' @include composerr.R
NULL

if(getRversion() >= "2.15.1")  utils::globalVariables(c("."))

#' convert vector to string
#' 
#' e.g. `c(1, 2, 3) -> "'1', '2', '3'"
#' @param x A numeric or character vector
#' @return A string
stringify <- function(x) {
  if (is.null(x))
    return("NULL")
  paste(paste0("'", x, "'"), collapse = ", ")
}

#' Get type of object, but also distinguishing `factor`
#' 
#' @param x An object
#' @return Possible strings:
#'   - `"logical"`
#'   - `"numeric"`
#'   - `"character"`
#'   - `"factor"`
#'   - `"list"`
#'   - various other non-atomic types
type_of_var <- function(x) {
  if (is.factor(x))
    return("factor")
  mode(x)
}

#' Remove NULL entries from list
#' 
#' @param x A list object
#' @return The list object stripped of all `NULL` entries
remove_null_entries <- function(x)
  `[`(x, !sapply(x, is.null))

#' Put some elements of a character vector infront
#' 
#' @param x The character vector that should be reordered
#' @param vals The values that should be put infront, if they are contained
#'   in `x`
#' @return The ordered character vector
order_infront <- function(x, vals) {
  for (val in rev(vals)) {
    if (val %in% x)
      x <- c(val, setdiff(x, val))
  }
  x
}
  