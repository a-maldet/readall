#' @include file_collection.R adapters.R
NULL

#' Read FWF, DSV or EXCEL data files
#' 
#' The functions `read_data()`, `read_data_fwf()`, `read_data_dsv()` and
#' `read_data_excel()` are all used in order to read FWF, DSV or EXCEL data files.
#' The function `read_data()` is the heart of the `readall` package and it
#' only requires the user to pass a single function argument
#' (a [file_definition][new_file_definition()] class object), holding all
#' needed file information in order to read the data file. By instead passing a
#' [file_collection][new_file_collection()] class object into `read_data()`, it is
#' also possible to read multiple data files at once and store the concatenated
#' data sets into a single data.frame.
#' The functions `read_data_fwf()`, `read_data_dsv()` and `read_data_excel()`
#' are less flexible, but have a more common structure, since this functions
#' do not use [file_definition][new_file_definition()] class objects, but require the user
#' to pass in all file information directly as function arguments.
#' 
#' The function `read_data()` can either read a single data file and 
#' return a data.frame or it can read multiple data files at once and return
#' the concatenated data sets as a single data.frame. 
#' `read_data()` can read the following data file types:
#' - `FWF`: Fixed width files. This files are text files, where the data is
#'   stored in columns, that have a fixed character width.
#' - `DSV`: Delimiter-separated value file. This files are text files, where
#'   the data is stored in columns that are separated by a delimiter character.
#' - `EXCEL`: An excel file holding the data.
#'
#' In order to read a single file with `read_data()`
#' a [file_definition][new_file_definition()] class object must be
#' passed into the function argument `file_definition`.
#' This [file_definition][new_file_definition()] class objects
#' contain all information needed for reading a specific data file.
#' When calling `read_data(file_definition)` where `file_definition` is a [file_definition][new_file_definition()] class
#' object, the following tasks will be executed:
#' - reading the data file specified in `file_definition` and storing the data to a data.frame
#' - if the argument `to_lower` was set to `TRUE`, then replace all column
#'   names of the read data set by its lower case version.
#' - if the column names where read from the data file and the column names
#'   are given by the `col_names` argument, then compare the read column
#'   names with the column names given in `col_names` and print a warning in
#'   case of discrepancies.
#' - in the case of SAS-files: If the argument `col_types` was given as well,
#'   then compare the read data types of the data columns with the 
#'   data types given in `col_types` and print a warning in
#'   case of discrepancies.
#' - modifying the resulting data.frame by consecutively applying all adapter functions
#'   stored in the adapter function list argument `file_definition$adapters`.
#'   For details see section *adapters*
#' - Optionally adding a column with value `file_definition$extra_col_val` and column name
#'   `file_definition$extra_col_name`. For details see [new_file_definition()]
#' - Optionally adding a character column holding the path of the read data file
#'   with column name defined in `file_definition$extra_col_file_path`.
#' - If `file_definition$cols_keep` is not `NULL`, then only the columns defined in
#'   `file_definition$cols_keep` will be kept. If the attribute is `NULL` then
#'   all columns will be kept.
#' - Finally the resulting data.frame will be returned.
#' 
#' In order to read multiple data files at once and automatically concatenate
#' the resulting data.frames into a single data.frame, you need to create a
#' list of `[file_definition][new_file_definition()]` class objects first by using the function
#' [new_file_collection()].
#' Each list entry holds the meta data of a different data file.
#' When `read_data()` is applied on a [file_collection][new_file_collection()] class
#' object, then the following tasks will be executed:
#' - loop through the list apply `read_data()` on every list
#'   entry. Since these entries are [file_definition][new_file_definition()] class objects, the
#'   tasks of reading single data files (as described above) will be executed
#'   for each list entry.
#' - concatenate all resulting data.frames into a single data.frame.
#' @inheritSection get_file_type File types
#' @inheritSection new_file_structure_fwf difference file_structure/file_definition/file_collection
#' @inheritSection new_file_structure_fwf adapters
#' @param file_definition A file_definitionuration object, holds all informations needed for
#'   reading the data. This object can be created with one of the following 
#'   functions:
#'   - `new_file_definition()`: For reading FWF, DSV or EXCEL files
#'   - `new_file_definition_fwf()`: For reading FWF files
#'   - `new_file_definition_dsv()`: For reading DSV files
#'   - `new_file_definition_excel()`: For reading EXCEL files
#' @return A data.frame holding the read data.
#' @export
#' @rdname read_data
read_data <- function(file_definition) {
  read_data_(
    file_definition,
    err_h = composerr("Error while calling 'read_data()'")
  )
}

#' @inheritParams new_file_definition_fwf
#' @export
#' @rdname read_data
read_data_fwf <- function(
  file_path,
  specification_files = NULL,
  cols = NULL,
  col_names = NULL,
  col_types = NULL,
  col_start = NULL,
  col_end = NULL,
  col_widths = NULL,
  sep_width = 0,
  skip_rows = 0,
  na = "",
  decimal_mark = ".",
  big_mark = ",",
  trim_ws = TRUE,
  n_max = Inf,
  encoding = "latin1",
  to_lower = TRUE,
  adapters = new_adapters(),
  cols_keep = TRUE,
  extra_col_name = NULL,
  extra_col_val = NULL,
  extra_col_file_path = FALSE,
  ...
) {
  err_h <- composerr("Error while calling 'read_data_fwf()'")
  new_file_definition_fwf_(
    file_path = file_path,
    specification_files = specification_files,
    cols = cols,
    col_names = col_names,
    col_types = col_types,
    col_start = col_start,
    col_end = col_end,
    col_widths = col_widths,
    sep_width = sep_width,
    skip_rows = skip_rows,
    na = na,
    decimal_mark = decimal_mark,
    big_mark = big_mark,
    trim_ws = trim_ws,
    n_max = n_max,
    encoding = encoding,
    to_lower = to_lower,
    adapters = adapters,
    cols_keep = cols_keep,
    extra_col_name = extra_col_name,
    extra_col_val = extra_col_val,
    extra_col_file_path = extra_col_file_path,
    err_h = err_h,
    ...
  ) %>%
    read_data_(err_h = err_h)
}

#' @inheritParams new_file_definition_dsv
#' @export
#' @rdname read_data
read_data_dsv <- function(
  file_path,
  specification_files = NULL,
  cols = NULL,
  col_names = NULL,
  col_types = NULL,
  sep = ";",
  header = TRUE,
  skip_rows = 0,
  na = "",
  decimal_mark = ".",
  big_mark = ",",
  trim_ws = TRUE,
  n_max = Inf,
  encoding = "latin1",
  to_lower = TRUE,
  rename_cols = FALSE,
  adapters = new_adapters(),
  cols_keep = TRUE,
  extra_col_name = NULL,
  extra_col_val = NULL,
  extra_col_file_path = FALSE,
  ...
) {
  err_h <- composerr("Error while calling 'read_data_dsv()'")
  new_file_definition_dsv_(
    file_path = file_path,
    specification_files = specification_files,
    cols = cols,
    col_names = col_names,
    col_types = col_types,
    sep = sep,
    header = header,
    skip_rows = skip_rows,
    na = na,
    decimal_mark = decimal_mark,
    big_mark = big_mark,
    trim_ws = trim_ws,
    n_max = n_max,
    encoding = encoding,
    to_lower = to_lower,
    rename_cols = rename_cols,
    adapters = adapters,
    cols_keep = cols_keep,
    extra_col_name = extra_col_name,
    extra_col_val = extra_col_val,
    extra_col_file_path = extra_col_file_path,
    err_h = err_h,
    ...
  ) %>%
    read_data_(err_h = err_h)
}

#' @inheritParams new_file_definition_excel
#' @export
#' @rdname read_data
read_data_excel <- function(
  file_path,
  specification_files = NULL,
  range = NULL,
  sheet = NULL,
  cols = NULL,
  col_names = NULL,
  col_types = NULL,
  header = TRUE,
  skip_rows = 0,
  na = "",
  trim_ws = TRUE,
  n_max = Inf,
  to_lower = TRUE,
  rename_cols = FALSE,
  adapters = new_adapters(),
  cols_keep = TRUE,
  extra_col_name = NULL,
  extra_col_val = NULL,
  extra_col_file_path = FALSE,
  ...
) {
  err_h <- composerr("Error while calling 'read_data_excel()'")
  new_file_definition_excel_(
    file_path = file_path,
    specification_files = specification_files,
    range = range,
    sheet = sheet,
    cols = cols,
    col_names = col_names,
    col_types = col_types,
    header = header,
    skip_rows = skip_rows,
    na = na,
    trim_ws = trim_ws,
    n_max = n_max,
    to_lower = to_lower,
    rename_cols = rename_cols,
    adapters = adapters,
    cols_keep = cols_keep,
    extra_col_name = extra_col_name,
    extra_col_val = extra_col_val,
    extra_col_file_path = extra_col_file_path,
    err_h = err_h,
    ...
  ) %>%
    read_data_(err_h = err_h)
}

#' @inheritParams new_file_definition_sas
#' @export
#' @rdname read_data
read_data_sas <- function(
  file_path,
  specification_files = NULL,
  skip_rows = 0,
  n_max = Inf,
  encoding = NULL,
  to_lower = TRUE,
  rename_cols = FALSE,
  retype_cols = FALSE,
  adapters = new_adapters(),
  cols_keep = TRUE,
  extra_col_name = NULL,
  extra_col_val = NULL,
  extra_col_file_path = FALSE,
  ...
) {
  err_h <- composerr("Error while calling 'read_data_sas()'")
  new_file_definition_sas_(
    file_path = file_path,
    specification_files = specification_files,
    skip_rows = skip_rows,
    n_max = n_max,
    encoding = encoding,
    to_lower = to_lower,
    rename_cols = rename_cols,
    retype_cols = retype_cols,
    adapters = adapters,
    cols_keep = cols_keep,
    extra_col_name = extra_col_name,
    extra_col_val = extra_col_val,
    extra_col_file_path = extra_col_file_path,
    err_h = err_h,
    ...
  ) %>%
    read_data_(err_h = err_h)
}

#' Helper method for `read_data()`
#' 
#' @inheritParams read_data
#' @param ... Additional arguments
#' @rdname read_data_
read_data_ <- function(file_definition, ...) UseMethod("read_data_")

#' Missing class for `[read_data_()]`
#' 
#' @inheritParams read_data_
#' @param err_h An error handler
#' @export
read_data_.default <- function(
  file_definition,
  err_h = composerr("Error while calling 'read_data_()'"),
  ...
) {
  paste(
    "The object supplied in argument 'file_definition' is not a",
    "'file_definition' class object nor a 'file_collection' class object.",
    "Please use one of the following functions, in order to create a",
    "valid object:\n",
    "'new_file_collection()',",
    "'new_file_definition()', 'new_file_definition_fwf()',",
    "'new_file_definition_dsv()' oder 'new_file_definition_excel()',",
    "'new_file_definition_sas()'"
  ) %>% err_h
}

#' @rdname read_data_
#' @param err_h An error handler
#' @param try_first A logical value. If set to `TRUE`, then the function [read_data_()]
#'   tries reading in a small amount of rows first, in order to check
#'   that the specified data files can be read and concatenated at all.
#'   This should save the user from waiting a long time before getting an
#'   error message, telling him that the file_definitionuration must be updated.
#' @export
read_data_.file_collection <- function(
  file_definition,
  err_h = composerr("Error while calling 'read_data_()'"),
  try_first = TRUE
) {
  validate_file_collection(
    file_definition,
    err_h = composerr("Invalid object in argument 'file_definition'", err_h)
  )
  if (isTRUE(try_first) && length(file_definition) > 1)
    read_data_(
      file_definition = new_file_collection_(
        lapply(
          file_definition,
          function(conf) {
            conf$n_max <- 20
            conf
          }
        ),
        err_h = composerr(
          paste(
            "Error while creating the 'file_collection' for the trial run",
            "(reading 20 rows of each file)."
          ),
          err_h
        )
      ),
      err_h = composerr(
        paste(
          "Error while creating the 'file_collection' for the trial run",
          "(reading 20 rows of each file)."
        ),
        err_h
      ),
      try_first = FALSE
    ) %>% invisible
  data <- lapply(
    seq_len(length(file_definition)),
    function(i) {
      err_h <- composerr(
        paste0(
          "Error while reading the ",
          i,
          "-th data file (file_path = ",
          stringify(file_definition[[i]]$file_path),
          ")"
        ),
        err_h
      )
      data <- read_data_(
        file_definition[[i]],
        err_h = err_h
      )
      if (!is.data.frame(data))
        err_h("The created object is not a data.frame.")
      names_d <- colnames(data)
      names_freq <- table(names_d)
      names_dup <- names(names_freq)[which(names_freq > 1)]
      if (length(names_dup) > 0)
        paste(
          "The created data.frame has duplicated column names:\n",
          stringify(names_dup)
        ) %>% err_h
      data
    }
  )
  lapply(
    seq_len(length(file_definition)),
    function(i) {
      err_h <- composerr(
        paste0(
          "Error while concatenating the ", i, "-th data.frame (file_path =",
          stringify(file_definition[[i]]$file_path),
          ") with the other data.frames."
        ),
        err_h
      )
      names_di <- colnames(data[[i]])
      names_di <- names_di[order(names_di)]
      names_d1 <- colnames(data[[1]])
      names_d1 <- names_d1[order(names_d1)]
      names_missing <- setdiff(names_d1, names_di)
      if (length(names_missing) > 0)
        paste0(
          "The following column names exist in the 1-st data.frame, ",
          "but are missing in the ", i, "-th data.frame:\n\t",
          stringify(names_missing)
        ) %>% err_h
      names_wrong <- setdiff(names_di, names_d1)
      if (length(names_wrong) > 0)
        paste0(
          "The following column names exist in the ", i, "-th data.frame, ",
          "but are missing in the 1-st data.frame:\n",
          stringify(names_wrong)
        ) %>% err_h
      types_d1 <- lapply(data[[1]], type_of_var)
      types_di <- lapply(data[[i]], type_of_var)
      lapply(
        names_di,
        function(var) {
          err_h <- composerr(
            paste("Error in column ", stringify(var)),
            err_h
          )
          if (types_d1[[var]] != types_di[[var]])
            paste0(
              "In the ", i, "-th data.frame the column is of data type ",
              stringify(types_di[[var]]),
              " but in the 1-st data.frame the column is of data type ",
              stringify(types_d1[[var]]),
              "."
            ) %>% err_h
          if (is.factor(data[[i]][[var]])) {
            levels_d1 <- levels(data[[1]][[var]])
            levels_di <- levels(data[[i]][[var]])
            if (!identical(levels_d1, levels_di))
              paste0(
                "In the ", i, "-th data.frame the column has the factor levels ",
                stringify(levels_di),
                " but in the 1-st data.frame it has the factor levels ",
                stringify(levels_d1),
                "."
              ) %>% err_h
          }
        }
      ) %>% invisible
    }
  ) %>% invisible
  tryCatch(
    do.call(
      rbind,
      data
    ),
    error = function(e) composerr(
      "The read data sets could not be concatenated",
      err_h
    )(e)
  )
}

#' @rdname read_data_
#' @export
read_data_.file_definition_fwf <- function(
  file_definition,
  err_h = composerr("Error while calling 'read_data_()'")
) {
  validate_file_definition(
    file_definition,
    err_h = composerr("Invalid object in argument 'file_definition'", err_h)
  )
  tryCatch(
    do.call(
      readr::read_fwf,
      args = c(
        list(
          file = file_definition$file_path,
          col_positions = get_fwf_col_positions(file_definition),
          col_types = get_col_types(file_definition),
          locale = get_fwf_locale(file_definition),
          na = file_definition$na,
          trim_ws = file_definition$trim_ws,
          skip = get_skip_rows(file_definition),
          n_max = file_definition$n_max
        ),
        file_definition$additional_arguments
      )
    ),
    error = function(e)
      paste(
        "The file ", stringify(file_definition$file_path),
        "could not be read:\n", e
      ) %>% err_h
  ) %>% read_data_post_process(file_definition = file_definition, err_h = err_h)
}

#' @rdname read_data_
#' @export
read_data_.file_definition_dsv <- function(
  file_definition,
  err_h = composerr("Error while calling 'read_data_()'")
) {
  validate_file_definition(
    file_definition,
    err_h = composerr("Invalid object in argument 'file_definition'", err_h)
  )
  tryCatch(
    x <- do.call(
      readr::read_delim,
      args = c(
        list(
          file = file_definition$file_path,
          delim = file_definition$sep,
          col_names = get_col_names(file_definition),
          col_types = get_col_types(file_definition),
          locale = get_fwf_locale(file_definition),
          na = file_definition$na,
          trim_ws = file_definition$trim_ws,
          skip = get_skip_rows(file_definition),
          n_max = file_definition$n_max
        ),
        file_definition$additional_arguments
      )
    ),
    error = function(e)
      paste(
        "The file", stringify(file_definition$file_path),
        "could not be read:\n", e
      ) %>% err_h
  ) %>% read_data_post_process(file_definition = file_definition, err_h = err_h)
}

#' @rdname read_data_
#' @export
read_data_.file_definition_excel <- function(
  file_definition,
  err_h = composerr("Error while calling 'read_data_()'")
) {
  validate_file_definition(
    file_definition,
    err_h = composerr("Invalid object in argument 'file_definition'", err_h)
  )
  tryCatch(
    do.call(
      readxl::read_excel,
      args = c(
        list(
          path = file_definition$file_path,
          sheet = file_definition$sheet,
          range = file_definition$range,
          col_names = get_col_names(file_definition),
          col_types = get_col_types(file_definition),
          na = file_definition$na,
          trim_ws = file_definition$trim_ws,
          skip = get_skip_rows(file_definition),
          n_max = file_definition$n_max
        ),
        file_definition$additional_arguments
      )
    ),
    error = function(e)
      paste(
        "The file", stringify(file_definition$file_path),
        "could not be read:\n", e
      ) %>% err_h
  ) %>% read_data_post_process(file_definition = file_definition, err_h = err_h)
}

#' @rdname read_data_
#' @export
read_data_.file_definition_sas <- function(
  file_definition,
  err_h = composerr("Error while calling 'read_data_()'")
) {
  validate_file_definition(
    file_definition,
    err_h = composerr("Invalid object in argument 'file_definition'", err_h)
  )
  tryCatch(
    do.call(
      haven::read_sas,
      args = c(
        list(
          data_file = file_definition$file_path,
          encoding = get_encoding(file_definition),
          skip = get_skip_rows(file_definition),
          n_max = file_definition$n_max
        ),
        file_definition$additional_arguments
      )
    ),
    error = function(e)
      paste(
        "The file", stringify(file_definition$file_path),
        "could not be read:\n", e
      ) %>% err_h
  ) %>% read_data_post_process(file_definition = file_definition, err_h = err_h)
}

#' Helper function for [read_data()]: Do all post processing
#' 
#' After reading a data file the following functions will be executed:
#' - [retype_cols_()]: If the `retype_cols` argument in the [file_definition][new_file_definition()]
#'   class object is set to `TRUE` and the `col_types` argument is not empty,
#'   then the data types of the columns in the read data set
#'   will be transformed to the data types given in the `col_types` argument.
#' - [rename_cols_()]: If the `rename_cols` argument in the [file_definition][new_file_definition()]
#'   class object is set to `TRUE` and the `col_names` argument is not empty,
#'   then the column names of the read data set
#'   will be overwritten by the values in the `col_names` argument.
#' - [apply_to_lower_()]: If the `to_lower` argument in the [file_definition][new_file_definition()]
#'   class object is set to `TRUE`, then the column names of the read data set
#'   will be transformed to lower case.
#' - [compare_col_names_()]: If the `col_names` are given in the
#'   [file_definition][new_file_definition())] class object and the header row was used as well
#'   (for SAS-files always, or for DSV- and EXCEL-files when `header = TRUE`), 
#'   then the column names read by [read_data()] will be compared with the given
#'   `col_names`. In case of differences, a warning will be printed.
#' - [compare_col_types_()]: If the `col_types` are given in the
#'   [file_definition][new_file_definition())] class object, then the data types in the data
#'   columns read by [read_data()] will be compared with the given
#'   `col_types`. In case of differences, a warning will be printed.
#' - [add_meta_()]: The meta data of each column (defined in
#'   `file_definition$file_meta`) will be added as an attribute to each data.frame column.
#' - [apply_adapters_()]: Apply all adapters stored in the `adapters` attribute of
#'   the `file_definition` class object on the data set.
#' - [apply_cols_keep_()]: Only keep the columns defined in the `cols_keep`
#'   attribute of the `file_definition` class object.
#'   If `file_definition$cols_keep` is not `NULL` only the columns with the column names
#'   given in `file_definition$keep_cols` are kept.
#' - [add_extra_cols_()]: As defined in the `extra_cols_*` attributes
#'   of the `file_definition` class object, add additional columns.
#'   - If `file_definition$extra_col_name` is not `NULL`, then this value will be used as
#'     a column name and the value stored in `file_definition$extra_col_val` will be used
#'     as column value.
#'   - If `file_definition$extra_col_file_path` is not `NULL`, then this value will be used
#'     as column name and the value `file_definition$file_path` will be used as column
#'     value.
#' @param data The data.frame, which should be modified
#' @param file_definition A [file_definition][new_file_definition()] class object, holding the file
#'   information of the data file.
#' @param err_h An error handler.
#' @return The modified data.frame
#' @rdname read_data_post_process
read_data_post_process <- function(
  data,
  file_definition,
  err_h
) {
  data %>%
    retype_cols_(file_definition = file_definition, err_h = err_h) %>%
    rename_cols_(file_definition = file_definition, err_h = err_h) %>%
    apply_to_lower_(file_definition = file_definition, err_h = err_h) %>%
    compare_col_names_(file_definition = file_definition, err_h = err_h) %>%
    compare_col_types_(file_definition = file_definition, err_h = err_h) %>%
    add_meta_(file_definition = file_definition, err_h = err_h) %>%
    apply_adapters_(file_definition = file_definition, err_h = err_h) %>%
    apply_cols_keep_(file_definition = file_definition, err_h = err_h) %>%
    add_extra_cols_(file_definition = file_definition, err_h = err_h)
}

#' @rdname read_data_post_process
rename_cols_ <- function(
  data,
  file_definition,
  err_h = composerr("Error while calling 'rename_cols_()'")
) {
  if (!is.null(data) && !is.data.frame(data))
    err_h("The supplied object is not a data.frame.")
  validate_file_definition(file_definition, err_h = err_h)
  if (isTRUE(file_definition$rename_cols) && !is.null(file_definition$col_names)) {
    col_names <- file_definition$col_names
    if (!is.null(file_definition$col_types))
      col_names <- col_names[file_definition$col_types != enum_col_types$skip]
    if (ncol(data) != length(col_names))
      paste(
        "The supplied data.frame has", ncol(data), "columns, but in the",
        "'file_definition' class object", length(col_names), "columns are defined.",
        "Please check the 'file_definition' class object."
      ) %>% err_h
    colnames(data) <- col_names
  } 
  data
}

#' @rdname read_data_post_process
retype_cols_ <- function(
  data,
  file_definition,
  err_h = composerr("Error while calling 'retype_cols_()'")
) {
  if (!is.null(data) && !is.data.frame(data))
    err_h("The supplied object is not a data.frame.")
  validate_file_definition(file_definition, err_h = err_h)
  if (isTRUE(file_definition$retype_cols) && !is.null(file_definition$col_types)) {
    col_types <- file_definition$col_types[file_definition$col_types != enum_col_types$skip]
    if (ncol(data) != length(col_types))
      paste(
        "The supplied data.frame has", ncol(data), "columns, but the",
        "'file_definition' class object has", length(col_types), "columns.",
        "Please check the 'file_definition' class object."
      ) %>% err_h
    for (i in seq_len(length(col_types))) {
      funs <- list(
        as.character,
        as.logical,
        as.numeric,
        as.integer
      )
      names(funs) <- c(
        enum_col_types$character,
        enum_col_types$logical,
        enum_col_types$numeric,
        enum_col_types$integer
      )
      fun <- funs[[col_types[i]]]
      if (!is.function(fun))
        paste0(
          "Error while transforming the ", i, "-th data column. ",
          "The data type specified in attribute `col_types` is invalid."
        ) %>% err_h
      data[[i]] <- fun(data[[i]])
    }
  } 
  data
}

#' @rdname read_data_post_process
add_extra_cols_ <- function(
  data,
  file_definition,
  err_h = composerr("Error while calling 'add_extra_cols_()'")
) {
  if (!is.null(data) && !is.data.frame(data))
    err_h("The supplied object is not a data.frame.")
  validate_file_definition(file_definition, err_h = err_h)
  if (!is.null(file_definition$extra_col_name))
    data[[file_definition$extra_col_name]] <- file_definition$extra_col_val
  if (!isFALSE(file_definition$extra_col_file_path))
    data[[file_definition$extra_col_file_path]] <- file_definition$file_path
  data
}

#' @rdname read_data_post_process
apply_to_lower_ <- function(
  data,
  file_definition,
  err_h = composerr("Error while calling 'apply_to_lower_()'")
) {
  if (isTRUE(file_definition$to_lower)) 
    colnames(data) <- tolower(colnames(data))
  data
}

#' @rdname read_data_post_process
compare_col_names_ <- function(
  data,
  file_definition,
  err_h = composerr("Error while calling 'compare_col_names_()'")
) {
  if (isFALSE(file_definition$rename_cols) && !is.null(file_definition$col_names)) {
    col_names <- file_definition$col_names
    data_names <- colnames(data)
    if (!is.null(file_definition$col_types))
      col_names <- col_names[file_definition$col_types != enum_col_types$skip]
    if (isTRUE(file_definition$to_lower))
      col_names <- tolower(col_names)
    if (length(col_names) != length(data_names) || any(col_names != data_names)) {
      if (length(col_names) < length(data_names))
        col_names <- c(col_names, rep("", length(data_names) - length(col_names)))
      if (length(col_names) > length(data_names))
        data_names <- c(data_names, rep("", length(col_names) - length(data_names)))
      options(warning.length = 3000L)
      ind <- which(col_names != data_names)
      paste0(
        "The read data set has the wrong column names:\n",
        paste(
          paste0(
            "  column-", ind, ": ",
            paste0(data_names[ind], "  (expected:", col_names[ind], ")")
          ),
          collapse = "\n"
        ),
        "\nThe reading process will be continued anyway."
      ) %>% err_h(handler = warning)
    }
  }
  data
}


#' @rdname read_data_post_process
compare_col_types_ <- function(
  data,
  file_definition,
  err_h = composerr("Error while calling 'compare_col_types_()'")
) {
  if (!is.null(file_definition$col_types)) {
    col_types <- file_definition$col_types[file_definition$col_types != enum_col_types$skip]
    col_types[col_types == enum_col_types$integer] <- enum_col_types$numeric
    data_names <- colnames(data)
    data_types <- lapply(data, type_of_var) %>% unlist
    if (length(col_types) != length(data_types) || any(col_types != data_types)) {
      if (length(col_types) < length(data_types))
        col_types <- c(col_types, rep("", length(data_types) - length(col_types)))
      if (length(col_types) > length(data_types)) {
        data_types <- c(data_types, rep("", length(col_types) - length(data_types)))
        data_names <- c(data_names, rep("", length(col_types) - length(data_types)))
      }
      ind <- which(col_types != data_types)
      options(warning.length = 3000L)
      paste0(
        "The read data set has the wrong data types in the columns:\n",
        paste(
          paste0(
            "  column-", ind, " (", data_names[ind], "): ",
            paste0(data_types[ind], "  (expected:", col_types[ind], ")")
          ),
          collapse = "\n"
        ),
        "\nThe reading process will be continued anyway."
      ) %>% err_h(handler = warning)
    }
  }
  data
}

#' @rdname read_data_post_process
apply_cols_keep_ <- function(
  data,
  file_definition,
  err_h = composerr("Error while calling 'apply_cols_keep_()'")
) {
  if (!is.null(data) && !is.data.frame(data))
    err_h("The supplied object is not a data.frame.")
  validate_file_definition(file_definition, err_h = err_h)
  if (isTRUE(file_definition$cols_keep))
    return(data)
  missing_cols <- file_definition$cols_keep[!file_definition$cols_keep %in% colnames(data)]
  if (length(missing_cols) > 0)
    paste(
      "The object in argument 'cols_keep' is invalid.",
      "The following columns could not be found in the data set:\n",
      stringify(missing_cols)
    ) %>% err_h
  data[, file_definition$cols_keep]
}

#' @rdname read_data_post_process
add_meta_ <- function(
  data,
  file_definition,
  err_h = composerr("Error while calling 'add_meta_()'")
) {
  if (!is.null(data) && !is.data.frame(data))
    err_h("The supplied object is not a data.frame.")
  validate_file_definition(file_definition, err_h = err_h)
  if (!is.null(file_definition$file_meta)) {
    file_meta <- file_definition$file_meta
    if (!is.null(file_definition$col_types))
      file_meta <- file_meta[file_definition$col_types != enum_col_types$skip]
    if (ncol(data) < length(file_meta)) {
      paste(
        "The meta data could not be added to the data.frame,",
        "since the data.frame has less columns than defined in the",
        "meta data object:\n",
        "  - In the meta data object:", length(file_meta), "columns\n",
        "  - In the read data.frame: ", nrow(data), "columns\n",
        "Adding of meta data will be skipped."
      ) %>% err_h(handler = warning)
    } else {
      col_count = 0
      for (i in seq_len(length(file_definition$file_meta))) {
        if (is.null(file_definition$col_types) || file_definition$col_types[i] != enum_col_types$skip) {
          col_count <- col_count + 1
          attr(data[[colnames(data)[col_count]]], "col_meta") <- file_definition$file_meta[[i]]
        }
      }
    }
  }
  data
}
