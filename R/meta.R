#' @include utils.R utils_validation.R
NULL

#' Create a new `col_meta` class object
#' 
#' This class holds a set of meta information for a specific column and
#' is usually stored in a [file_meta][new_file_meta()] class object, which is
#' usually stored ina [file_structure][new_file_structure_fwf()] class object.
#' @section meta information:
#' The [col_meta][new_col_meta()] class objects are used in order to store some 
#' meta information about single data columns, like additional column desciptions,
#' and column value/level descriptions. In order to store meta information
#' about a set of columns a [file_meta][new_file_meta()] class object can be
#' used. This objects store a list of [col_meta][new_col_meta()] class objects, where
#' each [col_meta][new_col_meta()] class object corresponds to a specific column in
#' a data set. This [file_meta][new_file_meta()] class objects are usually 
#' stored in [file_structure][new_file_structure_fwf()] class objects or 
#' [file_definition][new_file_definition()] class objects. But when calling [read_data()], the
#' meta information gets also appended to the resulting `data.frame`.
#' The meta information stored in a [file_structure][new_file_structure_fwf()],
#' a [file_definition][new_file_definition()] class object or a read `data.frame` can be extracted
#' by using the function [get_meta()].
#' A [col_meta][new_col_meta()] class object holds the following informations:
#' - `desc`: A string holding the column description.
#' - `values`: A vector (character/logical/numeric) usually holding
#'   the possible column values (e.g. `c(1, 2)`) or a more abstract text
#'   version of the column values (e.g. `c("JJJJMMDD", "99999999", "")`).
#' - `values_desc`: A character vector that corresponds to the `values` vector.
#'   Each entry of `values_desc` is a more detailed description of the 
#'   corresponding entry in `values`. If some descriptions are not present,
#'   the entries are `NA` values.
#' @param desc An optional string holding a column description.
#' @param values An optional vector (character/logical/numeric) usually holding
#'   the possible column values (e.g. `c(1, 2)`) or a more abstract text
#'   version of the column values (e.g. `c("JJJJMMDD", "99999999", "")`).
#'   The argument can be omitted.
#' @param values_desc An optional character vector that corresponds to the
#'  `values` vector. Each entry of `values_desc` is a more detailed
#'   description of the corresponding entry in `values`.
#'   If some descriptions are not present, the entries are filled with `NA`.
#'   The argument can be omitted. But if `values` and `values_desc` are both
#'   present, then they have to be of the same length.
#' @param valid_start An optional atomic argument (string or number) holding 
#'   information about the first data set, in which the current
#'   variable is valid
#'   (present in the data sets). For example `valid_start = 2017`.
#' @param valid_end An optional atomic argument (string or number) holding 
#'   information about the last data set, in which the current
#'   variable is valid
#'   (present in the data sets). For example `valid_end = Inf`.
#' @return A `col_meta` class object, holding the meta information for a specific
#'   data column.
#' @seealso [new_file_meta()], [get_meta()]
#' @export
new_col_meta <- function(
  desc = NULL,
  values = NULL,
  values_desc = NULL,
  valid_start = NULL,
  valid_end = NULL
) {
  err_h <- composerr("Error while calling 'new_col_meta()'")
  new_col_meta_(
    desc = desc,
    values = values,
    values_desc = values_desc,
    valid_start = valid_start,
    valid_end = valid_end,
    err_h = err_h
  )
}

#' @inherit new_col_meta
#' @param err_h An error handler.
new_col_meta_ <- function(
  desc = NULL,
  values = NULL,
  values_desc = NULL,
  valid_start = NULL,
  valid_end = NULL,
  err_h = composerr("Error while calling 'new_col_meta_()'")
) {
  structure(
    list(
      desc = desc,
      values = values,
      values_desc = values_desc,
      valid_start = valid_start,
      valid_end = valid_end
    ),
    class = "col_meta"
  ) %>%
    validate_col_meta(err_h = err_h)
}

#' Validate [col_meta][new_col_meta()] class objects
#' 
#' @inheritSection new_col_meta meta information
#' @param obj A list object: Pre col_meta object
#' @param validate_class A logical. If set to `TRUE` then the `obj` must also
#'   have the `col_meta` class
#' @param err_h An error handler
#' @return The unmodified `obj`
validate_col_meta <- function(obj, validate_class = TRUE, err_h) {
  if (isTRUE(validate_class) && !"col_meta" %in% class(obj))
    paste(
      "The supplied object is not a 'col_meta' class object.",
      "Please use the function 'new_col_meta()' in order to create such an object."
    ) %>% err_h
  err_h <- composerr("Error in 'col_meta' class object", err_h)
  check_string(obj, "desc", err_h = err_h, allow_null = TRUE)
  if (
    (!is.null(obj$values) && !is.character(obj$values) &&
      !is.numeric(obj$values) && !is.logical(obj$values)) ||
    (!is.null(obj$values) && length(obj$values) == 0)
  )
    paste(
      "Attribute 'values' must either be a 'logical', a 'numeric' or a",
      "'character' vector."
    ) %>% err_h
  check_character_vector(obj, "values_desc", err_h = err_h, allow_null = TRUE, allow_na = TRUE)
  check_same_length(obj, "values", "values_desc", err_h = err_h, allow_null = TRUE)
  if (!is.null(obj$valid_start) && (
    !is.atomic(obj$valid_start) || length(obj$valid_start) != 1 || is.na(obj$valid_start)
  ))
    paste(
      "Attribute 'valid_start' must be an atomic value and must not be `NA`."
    ) %>% err_h
  if (!is.null(obj$valid_end) && (
    !is.atomic(obj$valid_end) || length(obj$valid_end) != 1 || is.na(obj$valid_end)
  ))
    paste(
      "Attribute 'valid_end' must be an atomic value and must not be `NA`."
    ) %>% err_h
  invisible(obj)
}

#' Create a new `file_meta` class object
#' 
#' This class holds a list of [col_meta][new_col_meta()] class objects. For
#' more details about [col_meta][new_col_meta()] class objects see section 
#' **meta information**.
#' `file_meta` class objects are usually stored in
#' [file_structure][new_file_structure_fwf()] class objects or
#' [file_definition][new_file_definition()] class objects or as attributes of `data.frames`.
#' @inheritSection new_col_meta meta information
#' @param ... None, one or multiple [col_meta][new_col_meta()] class objects
#' @return A new [file_meta][new_file_meta()] class object, holding all
#'   passed in [col_meta][new_col_meta()] class objects.
#' @seealso [new_col_meta()], [get_meta()]
#' @export
new_file_meta <- function(...) {
  new_file_meta_(
    obj = list(...),
    err_h = composerr("Error while calling 'new_file_meta()'")
  )
}

#' Helper function for [new_file_meta()]
#' 
#' Create a new [file_meta][new_file_meta()] class object.
#' @inheritSection new_col_meta meta information
#' @param obj An unnamed list, where each entry is a [col_meta][new_col_meta()] class
#'   object.
#' @param err_h An error handler
new_file_meta_ <- function(
  obj,
  err_h = composerr("Error while calling 'new_file_meta_()'")
) {
  structure(obj, class = "file_meta") %>%
    validate_file_meta(err_h = err_h)
}

#' Validate [file_meta][new_file_meta()] class objects
#' 
#' @inheritSection new_col_meta meta information
#' @param obj An unnamed list object, where each entry is a [col_meta][new_col_meta()]
#'   class object.
#' @param validate_class A logical. If set to `TRUE` then the `obj` must also
#'   have the `file_meta` class.
#' @param err_h An error handler.
#' @return The unmodified `obj`
validate_file_meta <- function(obj, validate_class = TRUE, err_h) {
  if (isTRUE(validate_class) && !"file_meta" %in% class(obj))
    paste(
      "The supplied object is not a 'file_meta' class object.",
      "Please use the function 'new_file_meta()' in order to create such an object."
    ) %>% err_h
  err_h <- composerr("Error in 'file_meta' class object", err_h)
  check_list(obj = obj, err_h = err_h, allow_names = FALSE)
  lapply(
    seq_len(length(obj)),
    function(i) {
      validate_col_meta(
        obj = obj[[i]],
        err_h = composerr(paste0("Error in the ", i, "-th entry"), err_h)
      )
    }
  )
  invisible(obj)
}

#' @export
`[.file_meta` <- function(x, i, ...) {
  new_file_meta_(NextMethod())
}

#' Get meta information for one or multiple columns
#' 
#' Retrieve all meta information for a single or multiple data columns
#' and store it in a `data.frame`.
#' @param obj A [file_structure][new_file_structure_fwf()] class object or a
#'   [file_definition][new_file_definition()] class object or a [file_collection][new_file_collection()]
#'   class object or a `data.frame` generated by
#'   the [read_data()] function (holds the meta information as 
#'   attribute). The meta information will be extracted from this
#'   object.
#' @param cols An optional object, defining for which columns the meta
#'   information should be retrieved. It can either be a character vector,
#'   holding the names of the columns of interest, or it can be 
#'   numeric vector holding the indices of the columns of interest.
#'   If `cols` is omitted, then all columns are used.
#' @return Returns a `data.frame` holding all meta information.
#' @export
get_meta <- function(obj, cols = NULL) {
  get_meta_(
    obj = obj,
    cols = cols,
    err_h = composerr("Error while calling 'get_meta()'")
  )
}

#' Helper function for [get_meta()]
#' 
#' @inherit get_meta
#' @param ... Additional arguments
#' @export
#' @rdname get_meta_
get_meta_ <- function(obj, ...) UseMethod("get_meta_", obj)

#' Wrong class for [get_meta_()]
#' 
#' @inheritParams get_meta_
#' @param err_h An error handler
#' @export
get_meta_.default <- function(
  obj,
  cols = NULL,
  err_h = composerr("Error while calling 'get_meta_()'"),
  ...
) {
  paste(
    "The supplied object is nor a 'file_structure' class object nor a data.frame."
  ) %>% err_h
}

#' @param err_h An error handler
#' @rdname get_meta_
#' @export
get_meta_.file_structure <- function(
  obj,
  cols = NULL,
  err_h = composerr("Error while calling 'get_meta_()'"),
  ...
) {
  validate_file_structure(obj)
  file_meta <- obj$file_meta
  if (is.null(file_meta)) {
    err_h("No meta data was found.", handler = warning)
    if (!is.null(obj$col_names) || !is.null(obj$col_types)) {
      file_meta <- do.call(
        new_file_meta_,
        args = list(
          obj = lapply(
            seq_len(max(length(obj$col_names), length(obj$col_types))),
            function(i) new_col_meta()
          ),
          err_h = composerr(
            "Error while creating the dummy meta data list.",
            err_h
          )
        )
      ) 
    } else {
      return(invisible(NULL))
    }
  }
  if (is.null(cols))
    cols <- seq_len(length(file_meta))
  if (is.character(cols)) {
    check_character_vector(list(cols = cols), "cols", err_h = err_h)
    if (get_file_type(obj) == enum_file_types$sas)
      paste(
        "Fuer SAS-Spezifikations-Objeke sind Spalten-Namen im Argument 'cols'",
        "nicht zulaessig"
      ) %>% err_h
    if (is.null(obj$col_names))
      paste(
        "The object contains no column names, which could be matched with",
        "the column names given in 'cols'. Please use column ids instead of column names."
      ) %>% err_h
    wrong_cols <- cols[!cols %in% obj$col_names]
    if (length(wrong_cols) > 0)
      paste(
        "The following column names could not be find in 'obj':\n",
        stringify(wrong_cols)
      ) %>% err_h
    cols <- match(cols, obj$col_names)
  } else if (is.numeric(cols)) {
    check_integer_vector(
      list(cols = cols), 
      "cols",
      err_h = err_h,
      min_val = 1,
      max_val = length(file_meta)
    )
  } else {
    paste(
      "If argument 'cols' is supplied, then it must either be a character vector",
     "or a vector of positive integers."
    ) %>% err_h
  }
  create_meta_df(
    cols = cols,
    col_names = obj$col_names,
    col_types = obj$col_types,
    file_meta = file_meta
  )
}


#' @rdname get_meta_
#' @export
get_meta_.data.frame <- function(
  obj,
  cols = NULL,
  err_h = composerr("Error while calling 'get_meta_()'"),
  ...
) {
  file_meta_pre <- lapply(
    names(obj),
    function(col) attr(obj[[col]], "col_meta")
  )
  if (all(lapply(file_meta_pre, is.null) %>% unlist)) {
    paste(
      "Keine Meta-Informationen vorhanden. Eventuell wurde das data.frame",
      "'obj' nicht mittels der Funktion 'read_data()' erzeugt."
    ) %>% message
    return(invisible(NULL))
  }
  file_meta <- new_file_meta_(
    obj = lapply(
      file_meta_pre,
      function(col_meta) {
        if (is.null(col_meta)) {
          new_col_meta()
        } else {
          col_meta
        }
      }
    ),
    err_h = composerr(
      "Error while reading the meta data stored in the data.frame columns",
      err_h
    )
  )
  col_names <- names(obj)
  if (is.null(cols))
    cols <- seq_len(length(file_meta))
  if (is.character(cols)) {
    check_character_vector(list(cols = cols), "cols", err_h = err_h)
    wrong_cols <- cols[!cols %in% col_names]
    if (length(wrong_cols) > 0)
      paste(
        "The following column names could not be found:\n",
        stringify(wrong_cols)
      ) %>% err_h
    cols <- match(cols, col_names)
  } else if (is.numeric(cols)) {
    check_integer_vector(
      list(cols = cols), 
      "cols",
      err_h = err_h,
      min_val = 0,
      max_val = length(file_meta)
    )
  } else {
    paste(
      "If argument 'cols' is supplied, then it must either be a character",
     "vector or a vector of positive integers."
    ) %>% err_h
  }
  create_meta_df(
    cols = cols,
    col_names = col_names,
    col_types = lapply(obj, type_of_var) %>% unlist,
    file_meta = file_meta
  )
}

#' @rdname get_meta_
#' @export
get_meta_.file_collection <- function(
  obj,
  cols = NULL,
  err_h = composerr("Error while calling 'get_meta_()'"),
  ...
) {
  do.call(
    rbind,
    args = lapply(
      seq_len(length(obj)),
      function(i) {
        x <- get_meta_(
          obj[[i]],
          cols = cols,
          err_h = composerr(
            paste0(
              "Error while reading the meta data of the ", i, "-th ",
              "'file_collection' entry."
            ),
            err_h,
            ...
          )
        )
        x$file_path <- obj[[i]]$file_path
        x[, 
          c(
            "file_path", "col_name", "col_id", "col_type", "col_desc", "col_values",
            "col_values_desc", "col_valid_start", "col_valid_end"
          )
        ]
      }
    )
  )
}

#' Helper function for creating the meta data data.frame in [get_meta()]
#' 
#' @param cols A numeric vector holding the column indices
#' @param col_names A character vector holding the column names
#' @param col_types A character vector holding the column types
#' @param file_meta A [file_meta][new_file_meta()] class object holding the
#'   meta information for all columns.
#' @return A data.frame holding the meta information for columns specified in
#'   `cols`.
create_meta_df <- function(cols, col_names, col_types, file_meta) {
  if (is.null(file_meta)) {
    num_entries <- rep(1, min(1, length()))
  } else {
    num_entries <- lapply(
      seq_len(max(cols)),
      function(i) max(
        1,
        length(file_meta[[i]]$values),
        length(file_meta[[i]]$values_desc)
      )
    ) %>% unlist
  }
  data.frame(
    col_name = lapply(
      cols,
      function(i) {
        if (is.null(col_names)) {
          col_name <- NA_character_
        } else {
          col_name <- col_names[i]
        }
        rep(col_name, num_entries[i])
      }
    ) %>% unlist,
    col_id = lapply(
      cols,
      function(i) rep(i, num_entries[i])
    ) %>% unlist,
    col_type = lapply(
      cols,
      function(i) {
        if (is.null(col_types)) {
          col_type <- NA_character_
        } else {
          col_type <- col_types[i]
        }
        rep(col_type, num_entries[i])
      }
    ) %>% unlist,
    col_desc = lapply(
      cols,
      function(i) {
        if (is.null(file_meta[[i]]$desc)) {
          col_desc <- NA_character_
        } else {
          col_desc <- file_meta[[i]]$desc
        }
        rep(col_desc, num_entries[i])
      }
    ) %>% unlist,
    col_values = lapply(
      cols,
      function(i) {
        if (is.null(file_meta[[i]]$values)) {
          col_values <- rep(NA_character_, num_entries[i])
        } else {
          col_values <- file_meta[[i]]$values
        }
        col_values
      }
    ) %>% unlist,
    col_values_desc = lapply(
      cols,
      function(i) {
        if (is.null(file_meta[[i]]$values_desc)) {
          col_values_desc <- rep(NA_character_, num_entries[i])
        } else {
          col_values_desc <- file_meta[[i]]$values_desc
        }
        col_values_desc
      }
    ) %>% unlist,
    col_valid_start = lapply(
      cols,
      function(i) {
        val <- file_meta[[i]]$valid_start
        if (is.null(val))
          val <- NA_character_
        rep(val, num_entries[i])
      }
    ) %>% unlist,
    col_valid_end = lapply(
      cols,
      function(i) {
        val <- file_meta[[i]]$valid_end
        if (is.null(val))
          val <- NA_character_
        rep(val, num_entries[i])
      }
    ) %>% unlist
  )
}


