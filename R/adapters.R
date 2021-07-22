#' @include file_collection.R
NULL

#' Create a new 'adapters' class object
#' 
#' This class holds a list of adapter functions and their function description
#' and is usually stored inside of a [file_definition][new_file_definition()] or a
#' [file_structure][new_file_structure_fwf()] class object.
#' @inheritSection new_file_structure_fwf adapters
#' @param ... Either single list argument is passed to `new_adapters()`
#'   (e.g. `new_adapters(list(fun1, fun2, ...))`), where each list argument is an
#'   adapter function, or multiple functions are directly passed to
#'   `new_adapters()` (e.g. `new_adapters(fun1, fun2, ...)`).
#' @param description An optional character vector that must have the same
#'   length as the number of passed in adapter functions. If omitted then
#'   non-standard-evaluation will be used in order to capture the function
#'   name if possible. If a passed in function is not stored with a function
#'   name, then `NA_character_` will be used instead.
#' @return An `adapters` class object, holding a list of adapter functions and
#'   their descriptions.
#' @seealso [add_adapters()], [apply_adapters()]
#' @export
new_adapters <- function(..., description = NULL) {
  err_h <- composerr("Error while calling 'new_adapters()'")
  if (is.null(description)) {
    description <- rlang::enquos(...) %>%
      lapply(rlang::quo_name) %>%
      lapply(
        function(x) {if (length(x == 1) && make.names(x) == x) paste("Funktion", stringify(x)) else NA_character_}
      ) %>% unlist %>% `names<-`(NULL)
  }
  new_adapters_(
    args = list(...),
    description = description,
    err_h = err_h
  )
}

new_adapters_ <- function(
  args,
  description = NULL,
  err_h = composerr("Error while calling 'new_adapters_()'")
) {
  if (length(args) == 0 && length(description) != 0)
    err_h("No adapter functions supplied.")
  if (length(args) > 0) {
    if (length(args) == 1 && is.list(args[[1]]))
      args <- args[[1]]
    if (!is.null(description)) {
      if (!is.character(description))
        err_h("Argument 'description' is not a character vector.")
      if (length(args) != length(description))
        paste0(
          length(args), " adapter function were supplied, but ",
          length(description), " function descriptions."
        ) %>% err_h
    } else {
      description <- rep(NA_character_, length(args))
    }
  }
  structure(
    lapply(
      seq_len(length(args)),
      function(i) {
        desc <- description[i]
        if (!is.function(args[[i]]))
          paste0(
            "The ", i, "-th supplied function argument is not a function."
          ) %>% err_h
        list(
          fun = args[[i]],
          desc = desc
        )
      }
    ),
    class = "adapters"
  )
}

validate_adapters <- function(
  adapters,
  err_h = composerr("Error while calling 'validate_adapters()'"),
  validate_class = TRUE
) {
  err_h <- composerr("Invalid 'adapters' class object", err_h) 
  if (isTRUE(validate_class) && !"adapters" %in% class(adapters))
    paste(
      "The supplied object is not an 'adapters' class object.",
      "Please use 'new_adapters()' to create a valid object."
    ) %>% err_h
  if (is.null(adapters))
    err_h("The supplied object is 'NULL'.")
  if (length(adapters) == 0)
    return(adapters)
  if (!is.list(adapters) || !is.null(names(adapters)))
    paste("The supplied object must be an unnamed list object.") %>% err_h
  lapply(
    seq_len(length(adapters)),
    function(i) {
      err_h <- composerr(
        paste0("The ", i, "-th list entry is invalid"),
        err_h
      )
      if (!is.list(adapters[[i]]))
        err_h("It must be a list with list entries 'fun' and 'desc'.")
      req_vars <- c("fun", "desc")
      missing_vars <- req_vars[!req_vars %in% names(adapters[[i]])]
      if (length(missing_vars) > 0)
        paste0("The following variables are missing: ", stringify(missing_vars)) %>%
          err_h
      desc <- adapters[[i]]$desc
      fun <- adapters[[i]]$fun
      if (!is.character(desc) || length(desc) != 1)
        err_h("The sub list entry 'desc' is not a string.")
      if (!is.function(fun))
        err_h("The sub list entry 'fun' is not a function.")
    }
  )
  invisible(adapters)
}

#' Add an adapter functions to an [adapters][new_adapters()] or a
#' [pecification][new_file_structure_fwf()]
#' or a [file_definition][new_file_definition()] or a [file_collection][new_file_collection()] class object
#' 
#' @inheritSection new_file_structure_fwf adapters
#' @param obj This argument can be various different things:
#'   - An [adapters][new_adapters()] class object. Then the `adapters` will be
#'     appended to this object.
#'   - An [file_structure][new_file_structure_fwf()] class object. Then the `adapters`
#'     will be appended to the `obj$adapters` attribute of the class.
#'   - An [file_collection][new_file_collection()] class object. Then the `adapters`
#'     will be appended to each [file_definition][new_file_definition()] object in the 
#'     list.
#' @param adapters An [adapters][new_adapters()] class object, which holds
#'   all adapter functions and their descriptions. This class object can be
#'   created by using the function [new_adapters()].
#' @return The modified object
#' @export
add_adapters <- function(obj, adapters) {
  add_adapters_(
    obj = obj,
    adapters = adapters,
    err_h = composerr("Error while calling 'add_adapters()'")
  )
}

#' Helper function for [add_adapters()]
#' 
#' @inheritParams add_adapters
#' @param ... Additional arguments
#' @rdname add_adapters_
add_adapters_ <- function(obj, ...) UseMethod("add_adapters_")

#' Missing class for [add_adapters_()]
#' 
#' @inheritParams add_adapters_
#' @param err_h An error handler
#' @export
add_adapters_.default <- function(
  obj,
  adapters = NULL,
  err_h = composerr("Error while calling 'add_adapters_()'")
) {
  paste(
    "The supplied object is of none of the allowed classes:",
    "'adapters', 'file_structure', 'file_definition' or 'file_collection'."
  ) %>% err_h
}

#' @inheritParams add_adapters
#' @param err_h An error handler
#' @rdname add_adapters_
#' @export
add_adapters_.adapters <- function(
  obj,
  adapters,
  err_h = composerr("Error while calling 'add_adapters_()'")
) {
  validate_adapters(
    obj,
    err_h = composerr(
      paste(
        "The supplied 'adapters' argument is invalid"
      ),
      err_h
    )
  )
  validate_adapters(
    adapters,
    err_h = composerr("The supplied `adapters` class object is invalid", err_h)
  )
  new_adapters_(
    c(lapply(obj, function(x) x$fun), lapply(adapters, function(x) x$fun)),
    description = c(
      lapply(obj, function(x) x$desc),
      lapply(adapters, function(x) x$desc)
    ) %>% unlist,
    err_h = err_h
  )
}

#' @inheritParams add_adapters
#' @param err_h An error handler
#' @rdname add_adapters_
#' @export
add_adapters_.file_structure <- function(
  obj,
  adapters,
  err_h = composerr("Error while calling 'add_adapters_()'")
) {
  validate_file_structure(obj, err_h)
  validate_adapters(
    adapters,
    err_h = composerr("The supplied `adapters` object is invalid", err_h)
  )
  obj$adapters <- add_adapters_(obj$adapters, adapters, err_h)
  obj
}

#' @rdname add_adapters_
#' @export
add_adapters_.file_collection <- function(
  obj,
  adapters,
  err_h = composerr("Error while calling 'add_adapters_()'")
) {
  validate_file_collection(
    obj,
    err_h = composerr("The supplied 'file_collection' class object is invalid", err_h)
  )
  validate_adapters(
    adapters,
    err_h = composerr("The supplied 'adapters' class object is invalid", err_h)
  )
  lapply(
    seq_len(length(obj)),
    function(i)
      obj[[i]] <<- add_adapters_(obj[[i]], adapters, err_h = err_h)
  ) %>% invisible
  obj
}

#' Apply the adapter-functions stored in a [file_definition][new_file_definition()] class object to a data.frame
#' 
#' @inheritSection new_file_structure_fwf adapters
#' @param data The data.frame, which should be modified
#' @param file_definition A [file_definition][new_file_definition()] class object holding the adapter
#'   functions, which should be applied to the data.frame.
#' @return The modified data.frame
#' @export
apply_adapters <- function(data, file_definition) {
  apply_adapters_(
    data = data,
    file_definition = file_definition,
    err_h = composerr("Error while calling 'apply_adapters()'")
  )
}

#' Helper function for [apply_adapters()]
#' 
#' @inherit apply_adapters
#' @param err_h An error handler
apply_adapters_ <- function(
  data,
  file_definition,
  err_h = composerr("Error while calling 'apply_adapters_()'")
) {
  if (!is.data.frame(data))
    err_h("The supplied 'data' argument is not a data.frame.")
  validate_file_definition(file_definition, err_h = err_h)
  apply_adapters_class_(
    data, adapters = file_definition$adapters, err_h = err_h
  )
}

#' Apply the adapter-functions stored in an [adapters][new_adapters()] class object to a data.frame
#'
#' @inherit apply_adapters
#' @param adapters An [adapters][new_adapters()] class object holding the adapter
#'   functions, which should be applied to the data.frame.
#' @return The modified data.frame
#' @export
apply_adapters_class <- function(data, adapters) {
  apply_adapters_class_(
    data = data,
    adapters = adapters,
    err_h = composerr("Error while calling 'apply_adapters_class()'")
  )
}

#' Helper function for [apply_adapters_class()]
#' 
#' @inherit apply_adapters_class
#' @param err_h An error handler
apply_adapters_class_ <- function(
  data,
  adapters,
  err_h = composerr("Error while calling 'apply_adapters_class()'")
) {
  validate_adapters(adapters, err_h = err_h)
  lapply(
    seq_len(length(adapters)),
    function(i) {
      err_h <- composerr(
        paste0(
          "Error while applying the ", i, "-th adapter function (",
          get_adapter_desc(adapters, i), ")"
        ),
        err_h
      )
      tryCatch(
        data <<- (adapters[[i]]$fun)(data),
        error = function(e) err_h(e)
      )
    }
  )
  data
}

#' Get Description of adapter function
#' 
#' @inherit apply_adapters_class_
#' @param i The list index of the adapter function in the
#'   [adapters][new_adapters()] class object.
#' @return A string holding the description
get_adapter_desc <- function(
  adapters,
  i
) {
  desc <- adapters[[i]]$desc
  if (is.na(desc))
    desc <- "undokumentierte Funktion"
  desc
}

#' Print [adapters][new_adapters()] class objects
#' 
#' @param x The list that should be printed
#' @param indent A counter, defining the indentation level
#' @param ... additional arguments
#' @export
print.adapters <- function(x, indent = 0, ...) {
  err_h <- composerr("Error while calling 'print.adapters()'")
  validate_adapters(
    x,
    err_h = err_h
  )
  if (length(x) == 0) {
    cat("no adapter functions present\n")
  } else {
    if (indent == 0) {
      cat("### Adapter functions ###\n")
    } else {
      cat("\n")
    }
    lapply(
      seq_len(length(x)),
      function(i) {
        desc <- get_adapter_desc(x, i)
        paste(
          c(rep("  ", indent + 1), "Adapter-", i, ": ", desc, "\n"),
          collapse = ""
        ) %>% cat
      }
    )
  }
  x %>% invisible
}
