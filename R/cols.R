#' @include utils_validation.R
NULL

#' Extract column definitions from `cols` argument
#' 
#' @param cols A list object, where each entry represents a column definition.
#'   Each element of the list, is a list itself with possible entries:
#'   - `name`: Column name (optional)
#'   - `type`: Colunn type (required)
#'   - `start`: Column start postition (optional and only for FWF)
#'   - `end`: Column end postition (optional and only for FWF)
#'   - `width`: Column width (optional and only for FWF)
#' @param attr A character string, holding the name of the Definition-Attribute,
#'   which should be pulled out.
#' @return A character or integer vector
cols_get_attribute <- function(cols, attr) {
  lapply(cols, function(col) col[[attr]]) %>%
    unlist
}

#' validate argument `cols` in `new_file_structure_*()`
#' 
#' @inheritParams cols_get_attribute
#' @param file_type A character string holding the current file_type.
#' @param sep_width An optional string argument, defining the given `sep_width`
#'   argument in the FWF-Specification. If the file type is not FWF than it
#'   can be omitted.
#' @param err_h An error handler.
validate_cols <- function(
  cols,
  file_type,
  sep_width = NULL,
  err_h
) {
  err_h <- composerr("The supplied 'cols' object is invalid", err_h)
  if (!is.null(cols)) {
    check_list(cols, err_h = err_h)
    if (length(cols) == 0)
      err_h("The object is empty.")
    if (file_type == enum_file_types$fwf) {
      allowed_col_attrs <- c("name", "type", "start", "end", "width", "meta")
    } else {
      allowed_col_attrs <- c("name", "type", "meta")
    }
    #' find first column that is not skipped
    i0 <- NULL
    lapply(
      seq_len(length(cols)),
      function(i) {
        col <- cols[[i]]
        err_h <- composerr(
          paste0("Error in the ", i, "-th entry of 'cols'"),
          err_h
        )
        # check 'type' (required)
        check_string(
          col,
          "type",
          err_h = err_h, 
          allowed_values = unlist(enum_col_types)
        )
        # If it is a skipped column, then don't check anything
        if (col$type == enum_col_types$skip)
          return(NULL)
        # If this is the first column that is not skipped set it as compare
        # column
        if (is.null(i0))
          i0 <<- i
        # check that only allowed arguments are there
        col_attrs <- names(col)
        col_attrs <- col_attrs[order(col_attrs)]
        invalid_col_attrs <- col_attrs[!col_attrs %in% allowed_col_attrs]
        if (length(invalid_col_attrs) > 0)
          paste(
            "The following entries are invalid:\n\t",
            stringify(invalid_col_attrs)
          ) %>% err_h
        # check that arguments of current col are the same as of col-i0
        col1_attrs <- names(cols[[i0]])
        col1_attrs <- col1_attrs[order(col1_attrs)]
        if (!identical(col1_attrs, col_attrs))
          paste0(
            "Different column definitions were used (",
            stringify(col_attrs),
            ") than in the ", i0, "-th entry of 'cols' (",
            stringify(col1_attrs),
            ")."
          ) %>% err_h
        # check 'name' (optional)
        check_string(col, "name", err_h = err_h, allow_null = TRUE)
        if (file_type == enum_file_types$fwf) {
          # check column-definitions 'start', 'end', 'width'
          # check only two column-definitions are used (including 'sep_width') 
          col_attributes <- c("start", "end", "width", "sep_width")
          col$sep_width <- sep_width
          lapply(
            utils::combn(4, 3, simplify = FALSE),
            function(perm) {
              if (
                lapply(
                  perm, 
                  function(j) !is.null(col[[col_attributes[j]]])
                ) %>% unlist %>% all
              )
                paste(
                  "It is not allowed to use all three column definitions:\n",
                  stringify(col_attributes(perm)),
                  "\nOnly two of the supplied column definitions must be used."
                ) %>%
                err_h
            }
          ) %>% invisible
          # check that exactly two column-definitions are used
          if (
            lapply(
              col_attributes,
              function(attr) !is.null(col[[attr]])
            ) %>% unlist %>% sum != 2
          )
            paste(
              "The column positions are not sufficiently specified",
              "Two of the following values must be specified:\n",
              stringify(col_attributes)
            ) %>% err_h
          # check column definitions (optional)
          check_integer(
            col,
            "start",
            err_h = err_h,
            min_val = 1,
            allow_null = TRUE
          )
          check_integer(
            col,
            "end",
            err_h = err_h,
            min_val = 1,
            allow_null = TRUE,
            allow_na = i == length(cols)
          )
          if (!is.null(col$start) && !is.null(col$end) && col$start > col$end)
            err_h("The value of 'start' is greater than the value of 'end'.")
          check_integer(
            col,
            "width",
            err_h = err_h,
            min_val = 1,
            allow_null = TRUE,
            allow_na = i == length(cols)
          )
          if (!is.null(col$meta))
            validate_col_meta(
              col$meta,
              err_h = composerr("The attribute 'meta' is invalid", err_h)
            )
        }
      }
    ) %>% invisible
  }
  invisible(cols)
}
