#' @include file_structure.R
NULL

#' Throw an error: Object is not of class  `file_structure`
#' 
#' @param err_h An error handler
err_file_definition <- function(err_h) {
  paste(
    "The supplied object is not of class 'file_definition'.",
    "Please use one of the following functions in order to create a valid object:\n",
    "'new_file_definition(), 'new_file_definition_fwf()', 'new_file_definition_dsv()',",
    "'new_file_definition_excel()' or 'new_file_definition_sas()'."
  ) %>% err_h
}

#' Validate [file_definition][new_file_definition()] object
#' 
#' @param obj A [file_definition][new_file_definition()] object
#' @param err_h An error handler
validate_file_definition <- function(obj, err_h) {
  classes <- class(obj)
  sub_classes <- c(
    "file_definition_fwf",
    "file_definition_dsv",
    "file_definition_excel",
    "file_definition_sas"
  )
  if (!"file_definition" %in% classes)
    paste(
      "The supplied object is not of class 'file_definition'.",
      "Please use one of the following functions in order to create a valid object:\n",
      "'new_file_definition(), 'new_file_definition_fwf()', 'new_file_definition_dsv()',",
      "'new_file_definition_excel()' or 'new_file_definition_sas()'."
    ) %>% err_h
  if (sum(sub_classes %in% classes) == 0)
    paste0(
      "The supplied object is of none of the following classes:\n\t",
      stringify(sub_classes),
      "\nPlease use one of the following functions in order to create a valid object:\n",
      "'new_file_definition(), 'new_file_definition_fwf()', 'new_file_definition_dsv()',",
      "'new_file_definition_excel()' or 'new_file_definition_sas()'."
    ) %>% err_h
  if (sum(sub_classes %in% classes) > 1)
    paste0(
      "The supplied object is assigend to multiple classes:\n",
      stringify(sub_classes[sub_classes %in% classes]),
      "\nIt must be assigned to only one of this classes.",
      "\nPlease use one of the following functions in order to create a valid object:\n",
      "'new_file_definition(), 'new_file_definition_fwf()', 'new_file_definition_dsv()',",
      "'new_file_definition_excel()' or 'new_file_definition_sas()'."
    ) %>% err_h
  check_string(obj, "file_path", err_h = err_h)
  validate_file_structure(obj, err_h = err_h)
  if (!isTRUE(obj$cols_keep))
    check_character_vector(obj, "cols_keep", err_h = err_h)
  if (!isFALSE(obj$extra_col_file_path))
    check_string(obj, "extra_col_file_path", err_h = err_h)
  # check optional arguments
  check_string(obj, "extra_col_name", err_h = err_h, allow_null = TRUE)
  if (!is.atomic(obj$extra_col_val))
    err_h("The argument 'extra_col_val' must have an atomic data type.")
  if (!is.null(obj$extra_col_val) && length(obj$extra_col_val) != 1)
    err_h("Argument 'extra_col_val' must be of length 1.")
  if (xor(is.null(obj$extra_col_name), is.null(obj$extra_col_val)))
    paste(
      "The arguments 'extra_col_name' and 'extra_col_val' must both be supplied",
      "or both be omitted."
    ) %>% err_h
  # check file_structure
  validate_file_structure(obj, err_h = err_h)
}
