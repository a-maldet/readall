#' @include validate_file_definition.R
NULL

#' Create new file_definitionuration object
#' 
#' In order to read a data file with [read_data()],
#' you need to create a new `file_definitionuration` object.
#' The following functions are available:
#' - `new_file_definition()`: Can create a `file_definitionuration` object for FWF, DSV, EXCEL or
#'   SAS data files, depending on the supported file type of the
#'   [file_structure][new_file_structure_fwf()] class object.
#' - `new_file_definition_fwf()`: Can create a `file_definitionuration` object for FWF files.
#' - `new_file_definition_dsv()`: Can create a `file_definitionuration` object for DSV files.
#' - `new_file_definition_excel()`: Can create a `file_definitionuration` object for EXCEL files.
#' - `new_file_definition_sas()`: Can create a `file_definitionuration` object for SAS files.
#' @inheritSection get_file_type File types
#' @inheritSection new_file_structure_fwf difference file_structure/file_definition/file_collection
#' @inheritSection new_file_structure_fwf adapters
#' @inheritSection new_col_meta meta information
#' @param file_path A string holding the path to the data file.
#' @param file_structure A [file_structure][new_file_structure_fwf()] class object.
#'   This type of objects can be created by the functions
#'   [new_file_structure_fwf()], [new_file_structure_dsv()],
#'   [new_file_structure_excel()] or [new_file_structure_sas()]
#'   and fully defines the file structure of the
#'   data files. The idea is that a single file_structure can be valid for
#'   multiple data files and therefore be reused. Whereas a `file_definition` class
#'   object also holds the path to the file and is therefore only valid for
#'   a single file.
#' @param to_lower A logical flag, defining if the names of the columns should
#'   be transformed to lower case after reading the data set (by calling
#'   [read_data()]). This transformation will be applied before comparing the
#'   column names (in the case of SAS-Files or DSV- and EXCE-Files with
#'   `header = TRUE`).
#'   In the case of `new_file_definition()` the `to_lower` argument
#'   overwrites the `to_lower` argument in the
#'   [file_structure][new_file_structure_fwf()] class object given in
#'   `file_structure`. If `to_lower` is omitted, then the `file_structure`
#'   class object remains unchanged.
#'   In the case of `new_file_definition_fwf()`, `new_file_definition_dsv()`, `new_file_definition_excel()`
#'   or `new_file_definition_sas()` the argument `to_lower` must either be `TRUE`
#'   or `FALSE`.
#' @param cols_keep Either `TRUE` or a character vector.
#'   If set to `TRUE`, then all columns of the data 
#'   are kept when calling [read_data()].
#'   If `cols_keep` character vector, then the values in `cols_keep` represent
#'   the names of the columns, which are kept  when calling [read_data()].
#' @param extra_col_name An optional string, which defines the column, which
#'   will be added to the data set (after reading it with function [read_data()]).
#'   Each entry of the column will have the single value given in `extra_col_val`.
#'   For example: This column is useful when reading similar data files for
#'   separate years (one could pass the current data set year to `extra_col_name`
#'   and set `extra_col_name = "year"`).
#'   If `extra_col_name` is omitted, no column will be added to the data set and
#'   then `extra_col_val` must be omitted as well.
#'   additional column with the column name, given in  `extra_col_name`.
#'   If omitted, then no column will be added to the data set and the 
#'   argument `extra_col_name` must be omitted as well.
#' @param extra_col_val An optional value (any atomic type), which will be added
#'   (after reading the data set with function [read_data()]) as an
#'   additional column with the column name, given in  `extra_col_name`.
#'   For example: This column is useful when reading similar data files for
#'   separate years (one could pass the current data set year to `extra_col_name`
#'   and set `extra_col_name = "year"`).
#'   If omitted, then no column will be added to the data set and the 
#'   argument `extra_col_name` must be omitted as well.
#' @param extra_col_file_path Either `FALSE` or a string.
#'   If set to `FALSE` no file-path-column will be added to the data set, when
#'   calling [read_data()].
#'   If the argument `extra_col_file_path` is a string, then a column holding
#'   the file path of the data file will be added to the read data set, when
#'   calling [read_data()]. The string of `extra_col_file_path` will be used
#'   as column name for this additional column.
#' @param extra_adapters An optional [adapters][new_adapters()] class object,
#'   which holds a list of adapter functions. These adapter functions will
#'   be added to the adapter functions already stored in the
#'   [file_structure][new_file_structure_fwf()] class object. For further details
#'   on adapter functions see section *adapters*.
#' @return An `file_definition` class object holding all information needed for
#'   reading the data file with [read_data()].
#' @seealso [read_data()], [get_col_names()], [get_col_types()], [get_file_type()]
#'   [add_adapters()], [get_encoding()], [apply_adapters()]
#' @export
#' @rdname new_file_definition
new_file_definition <- function(
  file_path,
  file_structure,
  to_lower = NULL,
  cols_keep = TRUE,
  extra_col_name = NULL,
  extra_col_val = NULL,
  extra_col_file_path = FALSE,
  extra_adapters = new_adapters()
) {
  new_file_definition_(
    file_path = file_path,
    file_structure = file_structure,
    to_lower = to_lower,
    cols_keep = cols_keep,
    extra_col_name = extra_col_name,
    extra_col_val = extra_col_val,
    extra_col_file_path = extra_col_file_path,
    extra_adapters = extra_adapters,
    err_h = composerr("Error while calling 'new_file_definition()'")
  )
}

#' Helper function for `new_file_definition()`
#' 
#' @inheritParams new_file_definition
#' @param err_h An error handler
new_file_definition_ <- function(
  file_path,
  file_structure,
  to_lower,
  cols_keep,
  extra_col_name = NULL,
  extra_col_val = NULL,
  extra_col_file_path,
  extra_adapters = new_adapters(),
  err_h = composerr("Error while calling 'new_file_definition_()'")
) {
  validate_file_structure(
    file_structure,
    err_h = composerr("Invalid argument 'file_structure'", err_h)
  )
  validate_adapters(
    extra_adapters,
    err_h = composerr("Invalid argument 'extra_adapters'", err_h)
  )
  file_structure %<>%
    add_adapters_(adapters = extra_adapters, err_h = err_h)
  if (get_file_type(file_structure) == enum_file_types$fwf) {
    constructor <- new_file_definition_fwf_
  } else if (get_file_type(file_structure) == enum_file_types$dsv) {
    constructor <- new_file_definition_dsv_
  } else if (get_file_type(file_structure) == enum_file_types$excel) {
    constructor <- new_file_definition_excel_
  }  else if (get_file_type(file_structure) == enum_file_types$sas) {
    constructor <- new_file_definition_sas_
  }
  if (!is.null(to_lower) &&
      (!is.logical(to_lower) || length(to_lower) != 1 || is.na(to_lower)))
    paste(
      "If 'to_lower' is supplied, then it must be a logical value."
    ) %>% err_h
  if (!is.null(to_lower))
    file_structure$to_lower <- to_lower
  do.call(
    constructor,
    args = c(
      list(
        file_path = file_path,
        cols_keep = cols_keep,
        extra_col_name = extra_col_name,
        extra_col_val = extra_col_val,
        extra_col_file_path = extra_col_file_path,
        err_h = err_h
      ),
      file_structure
    )
  )
}

#' @export
#' @rdname new_file_definition
#' @inheritParams new_file_structure_fwf
new_file_definition_fwf <- function(
  file_path,
  specification_files = NULL,
  cols = NULL,
  col_names = NULL,
  col_types = NULL,
  col_start = NULL,
  col_end = NULL,
  col_widths = NULL,
  file_meta = NULL,
  sep_width = NULL,
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
  err_h <- composerr("Error while calling 'new_file_definition_fwf()'")
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
    file_meta = file_meta,
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
  )
}

#' Helper function for `new_file_definition_fwf()`
#' 
#' @inheritParams new_file_definition
#' @inheritParams new_file_structure_fwf_
new_file_definition_fwf_ <- function(
  file_path,
  specification_files = NULL,
  cols = NULL,
  col_names = NULL,
  col_types = NULL,
  col_start = NULL,
  col_end = NULL,
  col_widths = NULL,
  file_meta = NULL,
  sep_width = NULL,
  skip_rows,
  na,
  decimal_mark,
  big_mark,
  trim_ws,
  n_max,
  encoding,
  to_lower,
  adapters = new_adapters(),
  cols_keep,
  extra_col_name = NULL,
  extra_col_val = NULL,
  extra_col_file_path,
  err_h,
  ...
) {
  new_file_structure_fwf_(
    file_path = file_path,
    specification_files = specification_files,
    cols = cols,
    col_names = col_names,
    col_types = col_types,
    col_start = col_start,
    col_end = col_end,
    col_widths = col_widths,
    file_meta = file_meta,
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
    class = c("file_definition_fwf", "file_definition"),
    ...
  ) %>%
    validate_file_definition(err_h = err_h)
}

#' @export
#' @rdname new_file_definition
#' @inheritParams new_file_structure_dsv
new_file_definition_dsv <- function(
  file_path,
  specification_files = NULL,
  cols = NULL,
  col_names = NULL,
  col_types = NULL,
  file_meta = NULL,
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
  err_h <- composerr("Error while calling 'new_file_definition_dsv()'")
  new_file_definition_dsv_(
    file_path = file_path,
    specification_files = specification_files,
    cols = cols,
    col_names = col_names,
    col_types = col_types,
    file_meta = file_meta,
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
  )
}

#' Helper function for `new_file_definition_dsv()`
#' 
#' @inheritParams new_file_definition_dsv
#' @inheritParams new_file_structure_dsv_
new_file_definition_dsv_ <- function(
  file_path,
  specification_files = NULL,
  cols = NULL,
  col_names = NULL,
  col_types = NULL,
  file_meta = NULL,
  sep,
  header,
  skip_rows,
  na = na,
  decimal_mark,
  big_mark,
  trim_ws,
  n_max,
  encoding,
  to_lower,
  rename_cols,
  adapters = new_adapters(),
  cols_keep,
  extra_col_name = NULL,
  extra_col_val = NULL,
  extra_col_file_path,
  err_h,
  ...
) {
  new_file_structure_dsv_(
    file_path = file_path,
    specification_files = specification_files,
    cols = cols,
    col_names = col_names,
    col_types = col_types,
    file_meta = file_meta,
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
    class = c("file_definition_dsv", "file_definition"),
    ...
  ) %>%
    validate_file_definition(err_h = err_h)
}

#' @export
#' @rdname new_file_definition
#' @inheritParams new_file_structure_dsv
new_file_definition_excel <- function(
  file_path,
  specification_files = NULL,
  sheet = 1,
  range = NULL,
  cols = NULL,
  col_names = NULL,
  col_types = NULL,
  file_meta = NULL,
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
  err_h <- composerr("Error while calling 'new_file_definition_excel()'")
  new_file_definition_excel_(
    file_path = file_path,
    specification_files = specification_files,
    sheet = sheet,
    range = range,
    cols = cols,
    col_names = col_names,
    col_types = col_types,
    file_meta = file_meta,
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
  )
}

#' Helper function for `new_file_definition_excel()`
#' 
#' @inheritParams new_file_definition_excel
#' @inheritParams new_file_structure_excel_
new_file_definition_excel_ <- function(
  file_path,
  specification_files = NULL,
  range = NULL,
  sheet,
  cols = NULL,
  col_names = NULL,
  col_types = NULL,
  file_meta = NULL,
  header,
  skip_rows,
  na,
  trim_ws,
  n_max,
  to_lower,
  rename_cols,
  adapters = new_adapters(),
  cols_keep,
  extra_col_name = NULL,
  extra_col_val = NULL,
  extra_col_file_path,
  err_h,
  ...
) {
  new_file_structure_excel_(
    file_path = file_path,
    specification_files = specification_files,
    range = range,
    sheet = sheet,
    cols = cols,
    col_names = col_names,
    col_types = col_types,
    file_meta = file_meta,
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
    class = c("file_definition_excel", "file_definition"),
    ...
  ) %>%
    validate_file_definition(err_h = err_h)
}

#' @export
#' @rdname new_file_definition
#' @inheritParams new_file_structure_dsv
new_file_definition_sas <- function(
  file_path,
  specification_files = NULL,
  file_meta = NULL,
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
  err_h <- composerr("Error while calling 'new_file_definition_excel()'")
  new_file_definition_sas_(
    file_path = file_path,
    specification_files = specification_files,
    file_meta = file_meta,
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
  )
}

#' Helper function for `new_file_definition_sas()`
#' 
#' @inheritParams new_file_definition_sas
#' @inheritParams new_file_structure_sas_
new_file_definition_sas_ <- function(
  file_path,
  specification_files = NULL,
  file_meta = NULL,
  skip_rows,
  n_max,
  encoding = NULL,
  to_lower,
  rename_cols,
  retype_cols,
  adapters = new_adapters(),
  cols_keep,
  extra_col_name = NULL,
  extra_col_val = NULL,
  extra_col_file_path,
  err_h,
  ...
) {
  new_file_structure_sas_(
    file_path = file_path,
    specification_files = specification_files,
    file_meta = file_meta,
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
    class = c("file_definition_sas", "file_definition"),
    ...
  ) %>%
    validate_file_definition(err_h = err_h)
}

#' Print [file_definition][new_file_definition()] class objects
#' 
#' @param x The list that should be printed
#' @param indent A counter, defining the indentation level
#' @param ... additional arguments
#' @export
print.file_definition <- function(x, indent = 0, ...) {
  if (indent == 0) {
    paste0("### ", get_file_type(x), "-File-Konfiguration ###\n") %>%
      cat
  } else {
    paste0(get_file_type(x), "-File-Konfiguration:\n") %>%
      cat
  }
  print_file_structure(x, indent = indent)
}
