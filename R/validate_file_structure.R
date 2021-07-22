#' @include utils_validation.R enum.R
NULL

#' Throw an error: Object is not of class  `file_structure`
#' 
#' @param err_h An error handler
err_file_structure <- function(err_h) {
  paste(
    "The supplied object is not a 'file_structure' class object.",
    "Please use one of the following functions, in order to create a valid object:\n",
    "'new_file_definition(), 'new_file_definition_fwf()', 'new_file_definition_dsv()',",
    " 'new_file_definition_excel()', 'new_file_definition_sas()',",
    "'new_file_structure_fwf()', 'new_file_structure_dsv()',",
    "'new_file_structure_excel()' or 'new_file_structure_sas()."
  ) %>% err_h
}

#' Validate `file_structure` object
#' 
#' @param obj A `file_definition` object
#' @param ... Additional arguments
#' @rdname validate_file_structure
validate_file_structure <- function(obj, ...) UseMethod("validate_file_structure")

#' Missing class for [validate_file_structure()]
#' 
#' @inheritParams validate_file_structure
#' @param err_h An error handler
#' @export
validate_file_structure.default <- function(obj, err_h) {
  err_file_structure(err_h)
}

#' @param err_h An error handler
#' @export
#' @rdname validate_file_structure
validate_file_structure.file_structure <- function(
  obj,
  err_h = composerr("Error while calling 'validate_file_structure()'")
) {
  err_h <- composerr("Invalid 'file_structure' class object", err_h)
  classes <- class(obj)
  sub_classes <- c(
    "file_structure_fwf",
    "file_structure_dsv",
    "file_structure_excel",
    "file_structure_sas"
  )
  if (sum(sub_classes %in% classes) == 0)
    paste0(
      "The supplied object is not assigned to any of the following classes:\n",
      stringify(sub_classes),
      "\nPlease use one of the following function, in order to create a valid class object:\n\t",
      "'new_file_structure_fwf()', 'new_file_structure_dsv()', ",
      "'new_file_structure_excel()' oder 'new_file_structure_sas()'."
    ) %>% err_h
  if (sum(sub_classes %in% classes) > 1)
    paste0(
      "The supplied object is assigned to multiple classes:\n\t",
      stringify(sub_classes[sub_classes %in% classes]),
      "\nIt must be assigned to only one of these classes.",
      "\nPlease use one of the following function, in order to create a valid class object:\n\t",
      "'new_file_structure_fwf()', 'new_file_structure_dsv()', ",
      "'new_file_structure_excel()' oder 'new_file_structure_sas()'."
    ) %>% err_h
  check_list(obj, err_h = err_h)
  if (!is.null(obj$file_meta))
    validate_file_meta(
      obj$file_meta,
      err_h = composerr(
        "Invalid meta data in Attribut 'file_meta'",
        err_h
      )
    )
  check_character_vector(
    obj,
    "col_types",
    err_h = err_h,
    allowed_values = unlist(enum_col_types),
    allow_null = "file_structure_sas" %in% classes
  )
  check_character_vector(
    obj,
    "col_names",
    err_h = err_h,
    allow_null = TRUE
  )
  # check that 'col_types' and 'col_names' have the same length
  check_same_length(obj, "col_types", "col_names", allow_null = TRUE, err_h = err_h)
  check_same_length(obj, "col_types", "file_meta", allow_null = TRUE, err_h = err_h)
  if (!"file_structure_sas" %in% classes) {
    check_string(obj, "na", err_h = err_h)
    # Check that 'cols' does not exists
    check_missing(obj, "cols", err_h = err_h)
    check_logical(obj, "trim_ws", err_h = err_h, allow_null = TRUE)
  }
  check_integer(obj, "skip_rows", err_h = err_h, min_val = 0)
  check_integer(obj, "n_max", err_h = err_h, min_val = 1, allow_inf = TRUE)
  check_logical(obj, "to_lower", err_h = err_h)
  if (!"file_structure_fwf" %in% classes) 
    check_logical(obj, "rename_cols", err_h = err_h)
  check_character_vector(
    obj,
    "specification_files",
    err_h = err_h,
    allow_null = TRUE
  ) 
  validate_adapters(
    obj$adapters,
    err_h = composerr("The 'adapters' attribute is invalid", err_h)
  )
  invisible(obj)
}

#' @export
#' @rdname validate_file_structure
validate_file_structure.file_structure_fwf <- function(
  obj,
  err_h = composerr("Error while calling 'validate_file_structure()'")
) {
  NextMethod()
  err_h <- composerr("Invalid FWF 'file_structure' object", err_h)
  check_string(obj, "decimal_mark", err_h = err_h, allowed_values = c(".", ","))
  check_string(obj, "big_mark", err_h = err_h, allowed_values = c(".", ","))
  if (obj$decimal_mark == obj$big_mark)
    err_h("The arguments 'decimal_mark' and 'big_mark' must have different values.")
  check_string(
    obj,
    "encoding",
    err_h = err_h,
    allowed_values = unlist(enum_encodings)
  )
  # check optional arguments
  check_integer_vector(
    obj,
    "col_start",
    err_h = err_h,
    min_val = 1,
    ascending = TRUE,
    allow_null = TRUE
  )
  check_integer_vector(
    obj,
    "col_end",
    err_h = err_h,
    min_val = 1,
    ascending = TRUE,
    allow_null = TRUE,
    allow_na_last = TRUE
  )
  check_integer_vector(
    obj,
    "col_widths",
    err_h = err_h,
    min_val = 1,
    allow_null = TRUE,
    allow_na_last = TRUE
  )
  check_integer(obj, "sep_width", err_h = err_h, min_val = 0, allow_null = TRUE)
  # check that not too many column arguments are defined (avoid over-definition)
  col_attributes <- c("col_start", "col_end", "col_widths", "sep_width")
  lapply(
    utils::combn(4, 3, simplify = FALSE),
    function(perm) {
      if (
        lapply(perm, function(id) !is.null(obj[[col_attributes[id]]])) %>%
        unlist %>%
        all
      )
        paste(
          "It not allowed that all three arguments",
          stringify(col_attributes(perm)),
          "are supplied. Only two of them must be supplied."
        ) %>%
        err_h
    }
  ) %>% invisible
  # check that sufficiently many column arguments are set
  if (
    lapply(
      col_attributes,
      function(attr) !is.null(obj[[attr]])
    ) %>% unlist %>% sum != 2
  )
    paste(
      "The column positions are not sufficiently defined.",
      "Exactly two of the following attributes must be supplied:\n",
      stringify(col_attributes)
    ) %>% err_h
  # check col_* arguments have the same length
  check_same_length(obj, "col_types", "col_start", allow_null = TRUE)
  check_same_length(obj, "col_types", "col_end", allow_null = TRUE)
  check_same_length(obj, "col_types", "col_widths", allow_null = TRUE)
  # check that 'col_start' is not greater than 'col_end'
  if (!is.null(obj$col_tart) && !is.null(obj$col_end) &&
      any(obj$col_start > obj$col_end)
  )
    paste(
      "Argument 'start' has entries great than the corresponding entries in",
      "argument 'end'."
    ) %>% err_h
  invisible(obj)
}

#' @export
#' @rdname validate_file_structure
validate_file_structure.file_structure_dsv <- function(
  obj,
  err_h = composerr("Error while 'validate_file_structure()'")
) {
  NextMethod()
  err_h <- composerr("Invalid DSV 'file_structure' class object", err_h)
  # check required arguments
  check_required_attributes(obj, attrs_required = c("header", "sep"), err_h = err_h)
  check_logical(obj, "header", err_h = err_h)
  check_string(obj, "sep", err_h = err_h)
  check_string(obj, "decimal_mark", err_h = err_h, allowed_values = c(".", ","))
  check_string(obj, "big_mark", err_h = err_h, allowed_values = c(".", ","))
  if (obj$decimal_mark == obj$big_mark)
    err_h("The arguments 'decimal_mark' and 'big_mark' must have different values.")
  check_string(
    obj,
    "encoding",
    err_h = err_h,
    allowed_values = unlist(enum_encodings)
  )
  invisible(obj)
}

#' @export
#' @rdname validate_file_structure
validate_file_structure.file_structure_excel <- function(
  obj,
  err_h = composerr("Error while calling 'validate_file_structure()'")
) {
  NextMethod()
  err_h <- composerr("Invalid EXCEL 'file_structure' class object", err_h)
  # check required arguments
  check_required_attributes(obj, attrs_required = c("header"), err_h = err_h)
  check_logical(obj, "header", err_h = err_h)
  # check optional arguments
  if (!is.null(obj$sheet) && !is.character(obj$sheet) && !is.numeric(obj$sheet))
    err_h("Argument 'sheet' must either be a positive integer or a string.")
  if (is.character(obj$sheet))
    check_string(obj, "sheet", err_h = err_h, allow_null = TRUE)
  if (is.numeric(obj$sheet))
    check_integer(obj, "sheet", err_h = err_h, min_val = 1, allow_null = TRUE)
  check_excel_range(obj, "range", err_h = err_h, allow_null = TRUE)
  invisible(obj)
}

#' @export
#' @rdname validate_file_structure
validate_file_structure.file_structure_sas <- function(
  obj,
  err_h = composerr("Error while calling 'validate_file_structure()'")
) {
  NextMethod()
  err_h <- composerr("Invalid SAS 'file_structure' class object", err_h)
  # check required arguments
  check_string(
    obj,
    "encoding",
    err_h = err_h,
    allowed_values = unlist(enum_encodings),
    allow_null = TRUE
  )
  check_logical(
    obj,
    "retype_cols",
    err_h = err_h
  )
  invisible(obj)
}
