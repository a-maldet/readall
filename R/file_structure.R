#' @include validate_file_structure.R  meta.R
NULL

#' Helper function for creating a [file_structure][new_file_structure_fwf()] class object
#' 
#' @param file_structure A list holding all file_structures for reading an
#'   FWF, DSV, EXCEL or SAS data file.
#' @param class A character vector holding the names of the subclasses of the object
#' @param err_h An error handler
#' @return A `file_structure` class object
new_file_structure_ <- function(
  file_structure,
  class,
  err_h
) {
  structure(file_structure, class = c(class, "file_structure")) %>%
    validate_file_structure(err_h = err_h) %>%
    identity
}

#' Create [file_structure][new_file_structure_fwf()] class objects
#' 
#' In order to read a data file, you need to create a
#' [file_definitionuration][new_file_definition()] class object, which holds the path
#' to the data file and all file file_structures
#' needed in order to read the data file.
#' But often multiple files share the same file structure. In this case
#' it is useful to create a [file_structure][new_file_structure_fwf()] object,
#' which only holds the file structure definitions and reuse this 
#' file_structure object for creating multiple [file_definitionuration][new_file_definition()]
#' objects for the different files.
#' For each file type there is a separate file_structure constructor:
#' - `new_file_structure_fwf()`: Create a file file_structures for
#'   FWF files. These are data files, where the data is stored in columns
#'   of fixed character width.
#' - `new_secification_dsv()`: Create a file file_structures for
#'   DSV files. These are data files, where the data is stored in columns,
#'   which are separated by a deliminator character.
#' - `new_secification_excel()`: Create a file file_structures for
#'   EXCEL files.
#' - `new_secification_sas()`: Create a file file_structures for
#'   SAS files.
#' @inheritSection get_file_type File types
#' @section difference file_structure/file_definition/file_collection:
#' The goal of the package `readall` is it to read data files. For this
#' purpose the package offers three different class objects in order to 
#' store meta data about the data files:
#' - [file_structure][new_file_structure_fwf()] class objects: Objects of this
#'   class can be used in order to define
#'   all file type specific information (e.g. column positions,
#'   column names, column types, deliminator symbols, rows to skip etc.).
#'   The idea is, that one `file_structure` object may valid for several files
#'   and therefore be used to read multiple data files.
#' - [file_definition][new_file_definition()] class objects: Objects of this class type
#'   contain all informations in order to read a single specific data file
#'   (path to the data file, file [file_structure][new_file_structure_fwf()] etc.).
#'   A [file_definition][new_file_definition()] class object contains a
#'   [file_structure][new_file_structure_fwf()], which holds all file type
#'   specific information, but also other informations that are only valid
#'   for this specific file.
#' - [file_collection][new_file_collection()] class objects: A
#'   [file_collection][new_file_collection()] class object is simply a list holding
#'   multiple [file_definition][new_file_definition()] class objects.
#'   A [file_collection][new_file_collection()] class object 
#'   can be used in order to read several data files at once and concatenate
#'   the data into a single data.frame. 
#'   
#' @section adapters:
#' An adapter function is a function that takes a data.frame as input argument
#' and returns a modified version of this data.frame.
#' The adapter functions are stored in an [adapters][new_adapters()]
#' class object, which is a special list that contains all adapter functions
#' and a description text of each function. This class objects can be
#' created by using the function [new_adapters()].
#' The [adapters][new_adapters()] class objects can be added to a
#' [file_structure][new_file_structure_fwf()] or a
#' [file_definition][new_file_definition()] or a [file_collection][new_file_collection()] class object.
#' After reading a data file (by calling [read_data(file_definition)][read_data()]) 
#' all adapter functions listed in the `adapters` argument of the
#' file_definition][new_file_definition()] class object
#' will be applied consecutively to the loaded data set.
#' Adapter functions can be added to an existing
#' [file_structure][new_file_structure_fwf()] or a [file_definition][new_file_definition()] or
#' a [file_collection][new_file_collection()] class
#' object by using the function [add_adapters()].
#' Adapter functions can be used for several tasks:
#' - adapt the data sets in such a way that they can be concatenated for
#'   mutliple years
#' - compute new variables from existing variables
#' - fix errors in variables
#' - transform the values of a variable of an older data set, such that it
#'   complies with a newer variable definition
#' @inheritSection new_col_meta meta information
#' @param specification_files An optional character vector holding the paths
#'   to the files, where the file structure is described.
#' @param cols An optional list argument, holding the column definitions.
#'   This argument can be used instead of the arguments
#'   `col_names`, `col_types`, `col_start`, `col_end`, `col_widths`, in order
#'   to define the column structure. If the argument `cols` is used, then
#'   non of the `col_*` argument are allowed. If so, the `cols` argument
#'   has the following structure: It is list, where each list entry fully describes 
#'   a single column. Each list entry must have the same subselection of the 
#'   following possible list entries:
#'   - `type`: (obligatoric) A string value defining the data type of the column.
#'     The following values are allowed: `"character"`, `"logical"`, `"integer"`,
#'     `"numeric"` and `"NULL"` (for skipping this column). In the case of SAS files
#'     the `type` information can be omitted, since the data type information
#'     is stored in the SAS data files, but the argument `type` can still
#'     be useful in order to check that the read data column have the expected
#'     data type. For SAS-Files this check is done automatically after reading
#'     the data with [read_data()].
#'   - `name`: (optional) A string holding the column name.
#'   - `start`: (optional) A number holding the position of the first character of the column.
#'   - `end`: (optional) A number holding the position of the last character of the column.
#'   - `width` (optional) A numeric holding the number characters of the column.
#'   - `col_meta`: (optional) A [col_meta][new_col_meta()] class object, holding some
#'     meta information for the specific column (column description,
#'     possible column values + descriptions of possible column values).
#'     For details see section **meta information**.
#' @param col_names An optional character vector holding the names of the columns.
#'   If omitted, then the strings `"x1"`, `"x2"`, ... will be used.
#'   In the case of DSV or EXCEL files: If the argument `header` is
#'   set to `TRUE`, then the column names given
#'   in the data header will be used instead. If `col_names` is also supplied,
#'   then the column names given in the DSV, EXCEL or SAS file will be compared
#'   with the names given in `col_names`. Sometimes it is useful, to have the
#'   column names to be automatically transformed to lower case (directly after
#'   reading the date, but before comparing the column names). This can be 
#'   achieved by setting `to_lower = TRUE`.
#'   Generally, the argument `cols` can be used instead, in order to define
#'   the column names. If the argument `cols` is not `NULL`,
#'   then the argument `col_names` must be omitted.
#' @param col_types A character vector defining the data types for each column.
#'   The following strings are allowed: `"character"`, `"logical"`,
#'   `"integer"`, `"numeric"` and `"NULL"` (for skipping this column). 
#'   Generally, the argument `cols` can be used instead, in order to define
#'   the column types. If the argument `cols` is not `NULL`,
#'   then the argument `col_types` must be omitted. In the case of SAS files
#'   the `col_types` information can be omitted, since the data type information
#'   is stored in the SAS data files, but the argument `col_types` can still
#'   be useful in order to check the read data files, if the data types are
#'   as expected. For SAS-Files this check is done automatically after reading
#'   the data with [read_data()]
#' @param col_start An optional numeric vector holding the positions of the first character
#'   of each column.
#'   Generally, the argument `cols` can be used instead, in order to define
#'   the column start positions. If the argument `cols` is not `NULL`,
#'   then the argument `col_start` must be omitted.
#' @param col_end An optional numeric vector holding the positions of the last character
#'   of each column. The last vector entry (for the most right column) 
#'   is the only entry that can be `NA`. In this case, the most right
#'   cells are always read till the new line character.
#'   Generally, the argument `cols` can be used instead, in order to define
#'   the column end positions. If the argument `cols` is not `NULL`,
#'   then the argument `col_end` must be omitted.
#' @param col_widths An optional numeric vector holding the numbers of characters
#'   of each column.
#'   Generally, the argument `cols` can be used instead, in order to define
#'   the column widths. If the argument `cols` is not `NULL`,
#'   then the argument `col_widths` must be omitted.
#' @param file_meta An optional [file_meta][new_file_meta()] class object,
#'   holding some meta information for each data column
#'   (column description, possible column values + descriptions of possible
#'   column values).
#'   For details see section **meta information**.
#'   If the argument `cols` is not `NULL`, then the argument `file_meta`
#'   must be omitted.
#' @param sep_width An optional number, defining the number of characters
#'   between each column (often `0`).
#' @param skip_rows The number of rows to be skipped. In the case of DSV or 
#' EXCEL files: If the argument `header` is set to `TRUE`, then the
#' first row is always assumed to be the header row.
#' @param na A string representing missing values in the data file.
#' @param decimal_mark A character, defining the decimal separator in numeric
#'   columns. Only the strings `"."` and `","` are allowed.
#' @param big_mark A character, defining the thousands separator in numeric
#'   columns. Only the strings `"."` and `","` are allowed.
#' @param trim_ws A logical value, defining if the character values should
#'   be stipped of all leading and trailing white spaces.
#' @param n_max A number, defining the maximum number of rows to be
#'   read. If `n_max = Inf`, then  all available rows will be read.
#' @param to_lower A logical flag, defining if the names of the columns should
#'   be transformed to lower case after reading the data set (by calling
#'   [read_data()]). This transformation will be applied before comparing the
#'   column names (in the case of SAS-Files or DSV- and EXCE-Files with
#'   `header = TRUE`).
#' @param encoding A string, defining which encoding should be assumed when
#'   reading the data file. The following valuels are allowed:
#'   - `"UTF-8"`: For *UTF-8* encoded files.
#'   - `"latin1"`: For *ISO 8859-1* (also called *Latin-1*) encoded files.
#'     This encoding is almost the same as *Windows-1252* (also called *ANSI*).
#'     They differ only in 32 symbol codes (special symbols that are rarely
#'     used). In the case of SAS files, it is possible to set `encoding = NULL`.
#'     In this case, the encoding defined in the SAS data file header
#'     will be used.
#' @param adapters An optional list argument, holding a list of adapter functions
#'   (See section *adapters*).
#' @param ... Additional function arguments for
#'   - [readr::read_fwf()] in case of FWF files
#'   - [utils::read.delim()] in case of DSV files
#'   - [readxl::read_excel()] in case of EXCEL files
#' @export
#' @rdname new_file_structure
new_file_structure_fwf <- function(
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
  ...
) {
  err_h <- composerr("Error while calling 'new_file_structure_fwf()'")
  new_file_structure_fwf_(
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
    err_h = err_h,
    ...
  )
}

#' Helper function for [new_file_structure_fwf()]
#' 
#' @inherit new_file_structure_fwf
#' @param err_h An error handler
#' @param class A character vector holding one of the following class names:
#'   - `"file_structure_fwf"` for FWF file file_structures
#'   - `"file_structure_dsv"` for DSV file file_structures
#'   - `"file_structure_excel"` for EXCEL file file_structures
new_file_structure_fwf_ <- function(
  specification_files = NULL,
  cols = NULL,
  col_names = NULL,
  col_types,
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
  err_h,
  class = NULL,
  ...
) {
  if (!is.null(cols)) {
    if (!is.null(col_names))
      err_h("It is not allowed to supply the arguments 'cols' AND 'col_names'. Only one of them must be supplied.")
    if (!is.null(col_types))
      err_h("It is not allowed to supply the arguments 'cols' AND 'col_types'. Only one of them must be supplied.")
    if (!is.null(col_widths))
      err_h("It is not allowed to supply the arguments 'cols' AND 'col_widths'. Only one of them must be supplied.")
    if (!is.null(col_start))
      err_h("It is not allowed to supply the arguments 'cols' AND 'col_start'. Only one of them must be supplied.")
    if (!is.null(col_end))
      err_h("It is not allowed to supply the arguments 'cols' AND 'col_end'. Only one of them must be supplied.")
    if (!is.null(file_meta))
      err_h("It is not allowed to supply the arguments 'cols' AND 'file_meta'. Only one of them must be supplied.")
    validate_cols(
      cols,
      file_type = enum_file_types$fwf,
      sep_width = sep_width,
      err_h = err_h
    )
    col_names <- cols_get_attribute(cols, "name")
    col_types <- cols_get_attribute(cols, "type")
    col_widths <- cols_get_attribute(cols, "width")
    col_start <- cols_get_attribute(cols, "start")
    col_end <- cols_get_attribute(cols, "end")
    if (any(lapply(cols, function(col) !is.null(col$meta)) %>% unlist)) {
      file_meta <- new_file_meta_(
        lapply(
          seq_len(length(cols)),
          function(i) {
            col_meta <- cols[[i]]$meta
            if (is.null(col_meta))
              return(new_col_meta())
            col_meta
          }
        ),
        err_h = composerr(
          "Error while parsing the 'cols' argument: The stored meta data is invalid.",
          err_h
        )
      )  
    }
  }
  new_file_structure_(
    list(
      specification_files = specification_files,
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
      ...
    ),
    class = c(class, "file_structure_fwf"),
    err_h = err_h
  )
}

#' @param sep A string holding the column deliminator symbol.
#' @param header A logical value, which defines if the first row contains
#'   the data headers. If set to `TRUE`, then the names given in the data
#'   header will be used as column names instead.
#' @param rename_cols A logical value, which defines if the columns given in
#'   the data file should be overwritten by the columns given in argument 
#'   `col_names`. If `col_names` is not given, then `rename_cols` has no 
#'   effect.
#' @export
#' @rdname new_file_structure
new_file_structure_dsv <- function(
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
  ...
) {
  err_h <- composerr("Error while calling 'new_file_structure_dsv()'")
  new_file_structure_dsv_(
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
    err_h = err_h,
    ...
  )
}

#' Helper function for [new_file_structure_dsv()]
#' 
#' @inherit new_file_structure_fwf_
#' @inheritParams new_file_structure_dsv
new_file_structure_dsv_ <- function(
  specification_files = NULL,
  cols = NULL,
  col_names = NULL,
  col_types = NULL,
  file_meta = NULL,
  sep,
  header,
  skip_rows,
  na,
  decimal_mark,
  big_mark,
  trim_ws,
  n_max,
  encoding,
  to_lower,
  rename_cols,
  adapters,
  err_h,
  class = NULL,
  ...
) {
  if (!is.null(cols)) {
    if (!is.null(col_names))
      err_h("It is not allowed to supply the arguments 'cols' AND 'col_names'. Only one of them must be supplied.")
    if (!is.null(col_types))
      err_h("It is not allowed to supply the arguments 'cols' AND 'col_types'. Only one of them must be supplied.")
    if (!is.null(file_meta))
      err_h("It is not allowed to supply the arguments 'cols' AND 'file_meta'. Only one of them must be supplied.")
    validate_cols(cols, file_type = enum_file_types$dsv, err_h = err_h)
    col_names <- cols_get_attribute(cols, "name")
    col_types <- cols_get_attribute(cols, "type")
    if (any(lapply(cols, function(col) !is.null(col$meta)) %>% unlist)) {
      file_meta <- new_file_meta_(
        lapply(
          seq_len(length(cols)),
          function(i) {
            col_meta <- cols[[i]]$meta
            if (is.null(col_meta))
              return(new_col_meta())
            col_meta
          }
        ),
        err_h = composerr(
          "Error while parsing the 'cols' object: The stored meta data is invalid",
          err_h
        )
      )  
    }
  }
  new_file_structure_(
    list(
      specification_files = specification_files,
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
      ...
    ),
    class = c(class, "file_structure_dsv"),
    err_h = err_h
  )
}

#' @export
#' @rdname new_file_structure
#' @param sheet A string or an integer number:
#'   - string: The value defines the name of the sheet, which should be read.
#'   - integer: The value defines the position of the sheet, which should be read.
#'     (start counting with `1`).
#' @param range An optional string, holding an EXCEL range string, defining the
#'   data range in the spread sheet. If `header` is set to `TRUE`, then
#'   the range must include a header row.
new_file_structure_excel <- function(
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
  ...
) {
  err_h <- composerr("Error while calling 'new_file_structure_excel()'")
  new_file_structure_excel_(
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
    err_h = err_h,
    ...
  )
}

#' Helper function for [new_file_structure_excel()]
#' 
#' @inherit new_file_structure_fwf_
#' @inheritParams new_file_structure_excel
new_file_structure_excel_ <- function(
  specification_files = NULL,
  sheet,
  range = NULL,
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
  err_h,
  class = NULL,
  ...
) {
  if (!is.null(cols)) {
    if (!is.null(col_names))
      err_h("It is not allowed to supply the arguments 'cols' AND 'col_names'. Only one of them must be supplied.")
    if (!is.null(col_types))
      err_h("It is not allowed to supply the arguments 'cols' AND 'col_types'. Only one of them must be supplied.")
    if (!is.null(file_meta))
      err_h("It is not allowed to supply the arguments 'cols' AND 'file_meta'. Only one of them must be supplied.")
    validate_cols(cols, file_type = enum_file_types$excel, err_h = err_h)
    col_names <- cols_get_attribute(cols, "name")
    col_types <- cols_get_attribute(cols, "type")
    if (any(lapply(cols, function(col) !is.null(col$meta)) %>% unlist)) {
      file_meta <- new_file_meta_(
        lapply(
          seq_len(length(cols)),
          function(i) {
            col_meta <- cols[[i]]$meta
            if (is.null(col_meta))
              return(new_col_meta())
            col_meta
          }
        ),
        err_h = composerr(
          "Error while parsing 'cols': The stored meta data is invalid",
          err_h
        )
      )  
    }
  }
  new_file_structure_(
    list(
      specification_files = specification_files,
      sheet = sheet,
      range = range,
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
      ...
    ),
    class = c(class, "file_structure_excel"),
    err_h = err_h
  )
}

#' @param retype_cols A logical value, which defines if the types of the
#'   columns given in SAS file changed to the types given in the
#'   `col_types` argument. If `col_types` is not given, then `retype_cols` has no 
#'   effect.
#' @export
#' @rdname new_file_structure
new_file_structure_sas <- function(
  specification_files = NULL,
  file_meta = NULL,
  skip_rows = 0,
  n_max = Inf,
  encoding = NULL,
  to_lower = TRUE,
  rename_cols = FALSE,
  retype_cols = FALSE,
  adapters = new_adapters(),
  ...
) {
  err_h <- composerr("Error while calling 'new_file_structure_sas()'")
  new_file_structure_sas_(
    specification_files = specification_files,
    file_meta = file_meta,
    skip_rows = skip_rows,
    n_max = n_max,
    encoding = encoding,
    to_lower = to_lower,
    rename_cols = rename_cols,
    retype_cols = retype_cols,
    adapters = adapters,
    err_h = err_h,
    ...
  )
}

#' Helper function for [new_file_structure_sas()]
#' 
#' @inherit new_file_structure_fwf_
#' @inheritParams new_file_structure_sas
new_file_structure_sas_ <- function(
  specification_files = NULL,
  file_meta = NULL,
  skip_rows,
  n_max,
  encoding = NULL,
  to_lower,
  rename_cols,
  retype_cols,
  adapters = new_adapters(),
  err_h,
  class = NULL,
  ...
) {
  new_file_structure_(
    list(
      specification_files = specification_files,
      file_meta = file_meta,
      skip_rows = skip_rows,
      n_max = n_max,
      encoding = encoding,
      to_lower = to_lower,
      rename_cols = rename_cols,
      retype_cols = retype_cols,
      adapters = adapters,
      ...
    ),
    class = c(class, "file_structure_sas"),
    err_h = err_h
  )
}

