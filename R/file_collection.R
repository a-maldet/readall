#' @include file_definition.R
NULL

#' Create a list of file_definitionuration objects
#' 
#' With the function [read_data()] you can read FWF-, DSV-, EXCEL- or SAS data files
#' and store the data in a data.frame.
#' But with [read_data()] you can also read multiple data files
#' at once and automatically concatenate
#' the resulting data.frames into a single data.frame. In order to do so,
#' you need to create a
#' list of `[file_definition class][new_file_definition()]` objects.
#' This `file_definitionuration list` is a `file_collection` class object and each
#' list entry a [file_definition class][new_file_definition()] class object holding all
#' information needed for reading a specific data file.
#' Each file_definitionuration list entry can be created by using one of the following
#' functions:
#' - [new_file_definition()] for FWF, DSV or EXCEL data files
#' - [new_file_definition_fwf()] for FWF data files
#' - [new_file_definition_dsv()] for DSV data files
#' - [new_file_definition_excel()] for EXCEL data files
#' - [new_file_definition_sas()] for SAS data files
#' @inheritSection get_file_type File types
#' @inheritSection new_file_structure_fwf difference file_structure/file_definition/file_collection
#' @inheritSection new_file_structure_fwf adapters
#' @param ... Multiple file file_definitionurations. This file file_definitionurations can be
#'   created with the functions [new_file_definition()], [new_file_definition_fwf()],
#'   [new_file_definition_dsv()], [new_file_definition_excel()] or [new_file_definition_sas()]
#'   depending on the given file type.
#' @param to_lower An optional logical argument.
#'   If omitted, then the `to_lower` arguments in the file_definitionurations
#'   given in `...` remain unchanged. If `to_lower` is not omitted, 
#'   then for each file_definitionuration the argument `to_lower` will be
#'   updated with the new value.
#'   The `to_lower` argument defines if the names of the columns should
#'   be transformed to lower case after reading the data set (by calling
#'   [read_data()]). This transformation will be applied before comparing the
#'   column names (in the case of SAS-Files or DSV- and EXCE-Files with
#'   `header = TRUE`).
#' @param cols_keep An optional argument, which can either be `TRUE` or
#'   a character vector. If the argument is omitted (`NULL`), then the 
#'   `cols_keep` arguments of all `file_collection` entries will stay unchanged.
#'   If the argument `cols_keep` is not `NULL`, then its value will overwrite
#'   the `cols_keep` argument of each `file_collection` entry.
#'   If the `cols_keep` argument is set to `TRUE`, then all columns of each
#'   data set will be kept when calling [read_data()].
#'   If `cols_keep` is a character vector, then the values in `cols_keep`
#'   represent the names of the columns, which will be kept, when calling
#'   [read_data()].
#' @param extra_col_file_path An optional argument, which can either be
#'   `FALSE` or a string. If the argument is omitted (`NULL`), then the 
#'   `extra_col_file_path` argument of each `file_collection` entry will stay 
#'   unchanged.
#'   If the argument `extra_col_file_path` is not `NULL`, then its value will
#'   overwrite the `extra_col_file_path` argument of each `file_collection` entry.
#'   If set to `FALSE` no file-path-column will be added to the data sets, when
#'   calling [read_data()].
#'   If the argument `extra_col_file_path` is a string, then a column holding
#'   the file path of each data file will be added to the read data sets, when
#'   calling [read_data()]. The string of `extra_col_file_path` will be used
#'   as column name for this additional column.
#' @param extra_adapters An optional [adapters][new_adapters()] class object,
#'   which holds a list of adapter functions. These adapter functions will
#'   be added to the adapter functions already stored in each file 
#'   [file_definitionuration][new_file_definition()] passed in via `...`. For further details
#'   on adapter functions see section *adapters*.
#' @return A list, where each list entry is file [file_definitionuration][new_file_definition()],
#'   which holds all information needed to read a specific data file with the
#'   function [read_data()].
#' @inheritSection new_file_structure_fwf adapters
#' @export
new_file_collection <- function(
  ...,
  to_lower = NULL,
  cols_keep = NULL,
  extra_col_file_path = NULL,
  extra_adapters = new_adapters()
) {
  err_h <- composerr("Error while calling 'new_file_collection()'")
  file_collection <- list(...)
  if (length(file_collection) == 0)
    err_h("No 'file_definition' objects were supplied.")
  if (!is.null(names(file_collection)))
    err_h("The 'file_definition' objects must be supplied without argument name.")
  new_file_collection_(
    file_collection = file_collection,
    to_lower = to_lower,
    cols_keep = cols_keep,
    extra_col_file_path = extra_col_file_path,
    extra_adapters = extra_adapters,
    err_h = err_h
  )
}

#' A helper function for [new_file_collection()]
#' 
#' @inherit new_file_collection
#' @param file_collection An unnamed list of file file_definitionurations. This file file_definitionurations can be
#'   created with the functions [new_file_definition()], [new_file_definition_fwf()],
#'   [new_file_definition_dsv()], [new_file_definition_excel()] or [new_file_definition_sas()],
#'   depending on the given file type.
#' @param err_h An error handler
new_file_collection_ <- function(
  file_collection,
  to_lower = NULL,
  cols_keep = NULL,
  extra_col_file_path = NULL,
  extra_adapters = new_adapters(),
  err_h = composerr("Error while calling 'new_file_collection_()'")
) {
  if (!is.null(cols_keep) && !isTRUE(cols_keep)) {
    if (!is.character(cols_keep) || length(cols_keep) == 0)
      paste(
        "If 'cols_keep' is supplied, then it must either be `TRUE`",
        "or a non empty character vector."
      ) %>% err_h
    if (any(is.na(cols_keep)))
      paste(
        "If 'cols_keep' is supplied, then it must not contain any `NA` entries."
      ) %>% err_h
    if (length(cols_keep) != length(unique(cols_keep)))
      paste(
        "If 'cols_keep' is supplied, then it must not contain duplicates."
      ) %>% err_h
  }
  if (!is.null(extra_col_file_path) && !isFALSE(extra_col_file_path)) {
    if (!is.character(extra_col_file_path) || length(extra_col_file_path) != 1)
      paste(
        "If 'extra_col_file_path' is supplied, then it must either be `FALSE`",
        "or a string."
      ) %>% err_h
    if (is.na(extra_col_file_path))
      paste(
        "If 'extra_col_file_path' is supplied, then it must not be `NA`."
      ) %>% err_h
  }
  if (!is.null(to_lower) &&
    (!is.logical(to_lower) || length(to_lower) != 1 || is.na(to_lower)))
    paste(
      "If 'to_lower' is supplied, then it must be a logical value."
    ) %>% err_h
  validate_adapters(
    adapters = extra_adapters,
    err_h = composerr("Invalid argument 'extra_adapters'")
  )
  validate_file_collection(
    file_collection,
    validate_class = FALSE,
    err_h = err_h
  ) %>%
    lapply(function(x) {
      if (!is.null(cols_keep))
        x$cols_keep <- cols_keep
      x
    }) %>%
    lapply(function(x) {
      if (!is.null(extra_col_file_path))
        x$extra_col_file_path <- extra_col_file_path
      x
    }) %>%
    lapply(function(x) {
      if (!is.null(to_lower))
        x$to_lower <- to_lower
      x
    }) %>%
    structure(class = "file_collection") %>%
    add_adapters_(adapters = extra_adapters, err_h = err_h)
}

#' Validate [file_collection][new_file_collection()] object
#' 
#' @param file_collection A [file_collection][new_file_collection()] object
#' @param validate_class A logical argument, defining if the object class `file_collection`
#'   required.
#' @param err_h An error handler
validate_file_collection <- function(file_collection, validate_class = TRUE, err_h) {
  err_h <- composerr("Invalid 'file_collection' class object", err_h) 
  if (isTRUE(validate_class) && !"file_collection" %in% class(file_collection))
    paste(
      "The supplied 'file_collection' object is not of class 'file_collection'.",
      "Please use the function 'new_file_collection()' in order to create a",
      "valid 'file_collection' class object."
    ) %>% err_h
  if (!is.list(file_collection))
    paste(
      "The object is not a list.",
      "Please use the function 'new_file_collection()' in order to create a",
      "valid 'file_collection' class object."
    ) %>% err_h
  if (!is.null(names(file_collection)))
    paste(
      "The entries of the list must not have names.",
      "Please use the function 'new_file_collection()' in order to create a",
      "valid 'file_collection' class object."
    ) %>% err_h
  lapply(
    seq_len(length(file_collection)),
    function(i) {
      validate_file_definition(
        file_collection[[i]],
        err_h = composerr(
          paste0("Invalid 'file_definition' class object in the ", i, "-th list entry"),
          err_h
        )
      )
    }
  ) %>% invisible
}

#' Set the `cols_keep` argument for a [file_definition][new_file_definition()] or a
#' [file_collection][new_file_collection()] class object
#' 
#' @param obj A [file_definition][new_file_definition()] or a [file_collection][new_file_collection()]
#'   class object, for which the `cols_keep` argument should be set.
#' @param value A character vector holding the column names, which should
#'   be kept.
#' @return The modified object
#' @seealso [set_cols_keep_intersection()]
#' @export
set_cols_keep <- function(obj, value) {
  set_cols_keep_(
    obj,
    value,
    err_h = composerr("Error while calling 'set_cols_keep()'")
  )
}

#' Helper function for [set_cols_keep()]
#' 
#' @inheritParams set_cols_keep
#' @param err_h An error handler
#' @rdname set_cols_keep_
set_cols_keep_ <- function(obj, value, err_h) UseMethod("set_cols_keep_")

#' Wrong class case for `set_cols_keep_()`
#' 
#' @inheritParams set_cols_keep_
#' @export
set_cols_keep_.default <- function(obj, value, err_h)
  paste(
    "The supplied object in argument 'obj' is not a 'file_collection', nor",
    "a 'file_definition' class object.",
    "Please use one of the following functions in order to create a",
    "valid object:\n",
    "'new_file_collection()', 'new_file_definition()', 'new_file_definition_fwf()',",
    "'new_file_definition_dsv()' oder 'new_file_definition_sas()'."
  ) %>% err_h

#' @rdname set_cols_keep_
#' @export
set_cols_keep_.file_definition <- function(obj, value, err_h) {
  if (!isTRUE(value)) {
    if (!is.character(value) || length(value) == 0)
      paste(
        "The argument 'value' must either be `TRUE`",
        "or a non empty character vector."
      ) %>% err_h
    if (any(is.na(value)))
      paste(
        "The argument 'value' must not contain any `NA` values."
      ) %>% err_h
    if (length(value) != length(unique(value)))
      paste(
        "The argument 'value' must not contain duplicates."
      ) %>% err_h
  }
  obj$cols_keep <- value
  validate_file_definition(obj, err_h = err_h)
  obj
}

#' @rdname set_cols_keep_
#' @export
set_cols_keep_.file_collection <- function(obj, value, err_h) {
  if (!isTRUE(value)) {
    if (!is.character(value) || length(value) == 0)
      paste(
        "The argument 'value' must either be `TRUE`",
        "or a non empty character vector."
      ) %>% err_h
    if (any(is.na(value)))
      paste(
        "The argument 'value' must not contain any `NA` values."
      ) %>% err_h
    if (length(value) != length(unique(value)))
      paste(
        "The argument 'value' must not contain duplicates."
      ) %>% err_h
  }
  for (i in seq_len(length(obj))) {
    obj[[i]]$cols_keep <- value
  }
  validate_file_collection(obj, err_h = err_h)
  obj
}

#' Select the maximal set of columns in a [file_collection][new_file_collection()] class object
#' 
#' This function sets the `cols_keep` attribute in all
#' [file_definition][new_file_definition()] class objects stored in
#' a [file_collection][new_file_collection()] class object to the maximal
#' set of columns. This set is the intersection of column names stored
#' in the `col_names` attribute of each [file_definition][new_file_definition()]
#' class object.
#' @param obj A [file_collection][new_file_collection()]
#'   class object, for which the `cols_keep` argument should be set.
#' @return The modified [file_collection][new_file_collection()] class object.
#' @seealso [set_cols_keep()]
#' @export
set_cols_keep_intersection <- function(obj) {
  set_cols_keep_intersection_(
    obj,
    err_h = composerr("Error while calling 'set_cols_keep_intersection()'")
  )
}

#' Helper function for [set_cols_keep_intersection()]
#' 
#' @inheritParams set_cols_keep_intersection
#' @param err_h An error handler
#' @rdname set_cols_keep_
set_cols_keep_intersection_ <- function(obj, err_h) {
  validate_file_collection(obj, err_h = err_h)
  cols_intersection <- Reduce(
    intersect,
    lapply(
      obj[sapply(obj, function(x) !is.null(x$col_names)) %>% unlist],
      function(x) x$col_names
    )
  )
  if (length(cols_intersection) == 0)
    err_h("No mutual columns found: Empty intersection of column names.")
  set_cols_keep_(
    obj = obj,
    value = cols_intersection,
    err_h = err_h
  )
}

#' Set the `extra_col_file_path` argument for a [file_definition][new_file_definition()] or a
#' [file_collection][new_file_collection()] class object
#' 
#' @param obj A [file_definition][new_file_definition()] or a [file_collection][new_file_collection()]
#'   class object, for which the `extra_col_file_path` argument should be set.
#' @param value A string holding the column name, of the column holding the
#'   file path strings.
#' @return The modified object
#' @export
set_extra_col_file_path <- function(obj, value) {
  set_extra_col_file_path_(
    obj,
    value,
    err_h = composerr("Error while calling 'set_extra_col_file_path()'")
  )
}

#' Helper function for [set_extra_col_file_path()]
#' 
#' @inheritParams set_extra_col_file_path
#' @param err_h An error handler
#' @rdname set_extra_col_file_path_
set_extra_col_file_path_ <- function(obj, value, err_h)
  UseMethod("set_extra_col_file_path_")

#' Wrong class case for `set_extra_col_file_path_()`
#' 
#' @inheritParams set_extra_col_file_path_
#' @export
set_extra_col_file_path_.default <- function(obj, value, err_h)
  paste(
    "The object supplied in argument 'obj' is neither a 'file_collection'",
    "class object nor a 'file_definition' class object.",
    "Please use one of the following functions in order to create a valid",
    "object:\n",
    "'new_file_collection()', 'new_file_definition()', 'new_file_definition_fwf()',",
    "'new_file_definition_dsv()' oder 'new_file_definition_sas()'."
  ) %>% err_h

#' @rdname set_extra_col_file_path_
#' @export
set_extra_col_file_path_.file_definition <- function(obj, value, err_h) {
  if (!isFALSE(value)) {
    if (!is.character(value) || length(value) != 1)
      paste(
        "The argument 'value' must either be `FALSE` or a string."
      ) %>% err_h
    if (is.na(value))
      paste(
        "The argument 'value' must not be `NA`."
      ) %>% err_h
  }
  obj$extra_col_file_path <- value
  validate_file_definition(obj, err_h = err_h)
  obj
}

#' @rdname set_extra_col_file_path_
#' @export
set_extra_col_file_path_.file_collection <- function(obj, value, err_h) {
  if (!isFALSE(value)) {
    if (!is.character(value) || length(value) != 1)
      paste(
        "The argument 'value' must either be `FALSE` or a string."
      ) %>% err_h
    if (is.na(value))
      paste(
        "The argument 'value' must not be `NA`."
      ) %>% err_h
  }
  for (i in seq_len(length(obj))) {
    obj[[i]]$extra_col_file_path <- value
  }
  validate_file_collection(obj, err_h = err_h)
  obj
}

#' Set the `n_max` argument for a [file_definition][new_file_definition()] or a
#' [file_collection][new_file_collection()] class object
#' 
#' @param obj A [file_definition][new_file_definition()] or a [file_collection][new_file_collection()]
#'   class object, for which the `n_max` argument should be set.
#' @param value Either an integer defining the maximum number of rows, which
#'   should be read when calling [read_data()].
#' @return The modified object
#' @export
set_n_max <- function(obj, value) {
  set_n_max_(
    obj,
    value,
    err_h = composerr("Error while calling 'set_n_max()'")
  )
}

#' Helper function for [set_n_max()]
#' 
#' @inheritParams set_n_max
#' @param err_h An error handler
#' @rdname set_n_max_
set_n_max_ <- function(obj, value, err_h) UseMethod("set_n_max_")

#' Wrong class case for `set_n_max_()`
#' 
#' @inheritParams set_n_max_
#' @export
set_n_max_.default <- function(obj, value, err_h)
  paste(
    "The object supplied in argument 'obj' is neither a 'file_collection'",
    "class object nor a 'file_definition' class object.",
    "Please use one of the following functions in order to create a valid",
    "object:\n",
    "'new_file_collection()', 'new_file_definition()', 'new_file_definition_fwf()',",
    "'new_file_definition_dsv()' oder 'new_file_definition_sas()'."
  ) %>% err_h

#' @rdname set_n_max_
#' @export
set_n_max_.file_structure <- function(obj, value, err_h) {
  if (!is.numeric(value) || length(value) != 1 || is.na(value) || 
    is.nan(value) || (value != Inf && (as.integer(value) != value || value <= 0))
  )
    paste(
      "The argument 'value' must either be `Inf` or a positive integer."
    ) %>% err_h
  obj$n_max <- value
  validate_file_definition(obj, err_h = err_h)
  obj
}

#' @rdname set_n_max_
#' @export
set_n_max_.file_collection <- function(obj, value, err_h) {
  if (!is.numeric(value) || length(value) != 1 || is.na(value) || 
      is.nan(value) || (value != Inf && (as.integer(value) != value || value <= 0))
  )
    paste(
      "The argument 'value' must either be `Inf` or a positive integer."
    ) %>% err_h
  for (i in seq_len(length(obj))) {
    obj[[i]]$n_max <- value
  }
  validate_file_collection(obj, err_h = err_h)
  obj
}

#' Set the `adapters` argument for a [file_definition][new_file_definition()] or a
#' [file_collection][new_file_collection()] class object
#' 
#' @param obj A [file_definition][new_file_definition()] or a [file_collection][new_file_collection()]
#'   class object, for which the `adapters` argument should be set.
#' @param value An [adapters][new_adapters()] class object
#' @return The modified object
#' @export
set_adapters <- function(obj, value) {
  set_adapters_(
    obj,
    value,
    err_h = composerr("Error while calling 'set_adapters()'")
  )
}

#' Helper function for [set_adapters()]
#' 
#' @inheritParams set_adapters
#' @param err_h An error handler
#' @rdname set_adapters_
set_adapters_ <- function(obj, value, err_h) UseMethod("set_adapters_")

#' Wrong class case for `set_adapters_()`
#' 
#' @inheritParams set_adapters_
#' @export
set_adapters_.default <- function(obj, value, err_h)
  paste(
    "The object supplied in argument 'obj' is neither a 'file_collection'",
    "class object nor a 'file_definition' class object.",
    "Please use one of the following functions in order to create a valid",
    "object:\n",
    "'new_file_collection()', 'new_file_definition()', 'new_file_definition_fwf()',",
    "'new_file_definition_dsv()' oder 'new_file_definition_sas()'."
  ) %>% err_h

#' @rdname set_adapters_
#' @export
set_adapters_.file_structure <- function(obj, value, err_h) {
  validate_adapters(
    value,
    err_h = composerr(
      "The argument 'value' is not a valid 'adapters' class object",
      err_h
    )
  )
  obj$adapters <- value
  validate_file_definition(obj, err_h = err_h)
  obj
}

#' @rdname set_adapters_
#' @export
set_adapters_.file_collection <- function(obj, value, err_h) {
  validate_adapters(
    value,
    err_h = composerr(
      "The argument 'value' is not a valid 'adapters' class object",
      err_h
    )
  )
  for (i in seq_len(length(obj))) {
    obj[[i]]$adapters <- value
  }
  validate_file_collection(obj, err_h = err_h)
  obj
}

#' @export
print.file_collection <- function(x, ...) {
  cat("\n### Object is a file_definition list with", length(x), "entries ###\n")
  lapply(
    seq_len(length(x)),
    function(i) {
      cat(paste0("\u23F5File-Definition-", i, ": "))
      print(x[[i]], indent = 1)
      cat("\n")
    }
  ) %>% invisible
}

#' @export
`[.file_collection` <- function(x, i, ...) {
  new_file_collection_(NextMethod())
}
