#' @include utils.R enum.R
NULL

#' Check that object is a list object
#' 
#' @param obj The object, that should be checked.
#' @param err_h An error handler.
#' @param allow_null A logical, defining whether the object is allowed to be `NULL`.
#' @param allow_names A logical, defining whether the entries may have names.
#' @param allow_empty A logical, defining whether the list may be empty.
#' @return The passed in object.
check_list <- function(
  obj,
  err_h,
  allow_null = FALSE,
  allow_names = TRUE,
  allow_empty = TRUE
) {
  if (!allow_null && is.null(obj))
    err_h("Das Objekt ist 'NULL'.") 
  if (!is.null(obj)) {
    if (!is.list(obj))
      err_h("Das Objekt ist keine Liste.")
    if (!allow_empty && length(obj) == 0)
      paste("Das Objekt ist eine Liste, hat aber keine Eintraege.") %>% err_h
    if (!allow_names && !is.null(names(obj)))
      paste(
        "Das Objekt ist eine Liste, aber die Listeneintraege duerfen keine",
        "Namen haben."
      ) %>% err_h
  }
  invisible(obj)
}

#' Check that a list object has all required attributes
#' 
#' @inheritParams check_list
#' @param obj The list object, that should be checked
#' @param attrs_required A character vector, holding the names of the required list elements
check_required_attributes <- function(obj, attrs_required, err_h) {
  attrs <- names(obj)
  attrs_missing <- attrs_required[!attrs_required %in% attrs]
  if (length(attrs_missing) > 0)
    paste0(
      "Folgende Spezifikationen fehlen:\n\t",
      stringify(attrs_missing)
    ) %>% err_h
} 

#' Check that a specific list element is a string
#' 
#' @inheritParams check_required_attributes
#' @param attr A string holding the name of the list element, that should be
#'   checked.
#' @param allowed_values An optional set of allowed strings.
#' @param allow_na Are `NA` values allowed?
#' @param allow_null A logical, defining whether the argument is allowed to be `NULL`.
check_string <- function(
  obj,
  attr,
  err_h,
  allowed_values = NULL,
  allow_na = FALSE,
  allow_null = FALSE
) {
  x <- obj[[attr]]
  text <- paste("Das Argument", stringify(attr))
  if (!allow_null && is.null(x))
    paste(text, "fehlt.") %>% err_h 
  if (!is.null(x)) {
    if (!is.character(x) || length(x) != 1)
      paste(text, "ist kein String.") %>% err_h
    if (!allow_na && is.na(x))
      paste(text, "darf kein 'NA' sein.") %>% err_h
    if (!is.na(x) && !is.null(allowed_values) && !x %in% allowed_values)
      paste(
        text,
        "hat keinen der erlaubten Werte:\n\t", stringify(allowed_values)
      ) %>% err_h
  }
  invisible(obj)
}

#' Check that a specific list element is an excel range string
#' 
#' Assumes non-zero length.
#' @inheritParams check_string
check_excel_range <- function(
  obj,
  attr,
  err_h,
  allow_na = FALSE,
  allow_null = FALSE
) {
  x <- obj[[attr]]
  text <- paste("Das Argument", stringify(attr))
  check_string(
    obj,
    attr,
    err_h = err_h,
    allow_na = allow_na,
    allow_null = allow_null
  )
  if (!is.null(x) && !is.na(x)) {
    if (!grepl("^[A-Z]+[1-9][0-9]*:[A-Z]+[1-9][0-9]*$", x, perl = TRUE))
      paste(text, "hat keinen gueltigen Excel-Range-Text.") %>% err_h
  }
  invisible(obj)
}

#' Check that a specific list element is a character vector
#' 
#' Assumes non-zero length.
#' @inheritParams check_string
#' @param allow_na_last A logical value, defining if the last entry of the 
#'   vector is allowed to be `NA`.
check_character_vector <- function(
  obj,
  attr,
  err_h,
  allowed_values = NULL,
  allow_na = FALSE,
  allow_null = FALSE,
  allow_na_last = FALSE
) {
  x <- obj[[attr]]
  text <- paste("Das Argument", stringify(attr))
  if (!allow_null && is.null(x))
    paste(text, "fehlt.") %>% err_h 
  if (!is.null(x)) {
    if (!is.character(x))
      paste(text, "ist kein Character Vektor.") %>% err_h
    if (length(x) == 0)
      paste(text, "muss mindestens ein Element besitzen.") %>% err_h
    if (!allow_na_last && !allow_na && any(is.na(x)))
      paste(text, "beinhaltet 'NA'-Werte") %>% err_h
    if (!allow_na_last && !allow_na && any(is.na(x[seq_len(length(x)- 1)])))
      paste(
        text,
        "darf nur im letzten Eintrag 'NA'-Werte beinhalten,",
        "es sind jedoch schon vorher 'NA'-Werte vorhanden."
      ) %>% err_h
    if (!is.null(allowed_values) && any(!is.na(x) & !x %in% allowed_values))
      paste(
        text,
        "hat Elemente, die keinen der erlaubten Werte annehmen:\n\t", stringify(allowed_values)
      ) %>% err_h
  }
  invisible(obj)
}

#' Check that a specific list element is a numeric value
#' 
#' @param min_val An optional argument, defining a minimal allowed value.
#' @param max_val An numeric argument, defining the maximal allowed value.
#' @param allow_inf An optional logical, defining if the value `Inf` is allowed.
#' @inheritParams check_string
check_numeric <- function(
  obj,
  attr,
  err_h,
  min_val = NULL,
  max_val = Inf,
  allow_na = FALSE,
  allow_null = FALSE,
  allow_inf = FALSE
) {
  x <- obj[[attr]]
  text <- paste("Das Argument", stringify(attr))
  if (!allow_null && is.null(x))
    paste(text, "fehlt.") %>% err_h 
  if (!is.null(x)) {
    if (!is.numeric(x))
      paste(text, "ist keine Zahl.") %>% err_h
    if (length(x) != 1)
      paste(text, "muss Vektor-Laenge 1 haben.") %>% err_h
    if (!allow_na && is.na(x))
      paste(text, "darf nicht 'NA' sein.") %>% err_h
    if (allow_inf && !is.na(x) && x == Inf)
      return(invisible(obj))
    if (!is.na(x) && !is.finite(x))
      paste(text, "ist kein finiter Zahlenwert (x=", x, ")") %>% err_h
    if (!is.na(x) && !is.null(min_val) && x < min_val)
      paste(text, "hat einen Wert kleiner", stringify(min_val)) %>% err_h
    if (!is.na(x) && !is.nan(x) && x > max_val)
      paste(text, "hat einen Wert groesser", stringify(max_val)) %>% err_h
  }
  invisible(obj)
}

#' Check that a specific list element is an integer value
#' 
#' @inheritParams check_numeric
check_integer <- function(
  obj, 
  attr, 
  err_h, 
  min_val = NULL,
  max_val = Inf,
  allow_na = FALSE,
  allow_null = FALSE,
  allow_inf = FALSE
) {
  x <- obj[[attr]]
  text <- paste("Das Argument", stringify(attr))
  check_numeric(
    obj,
    attr,
    err_h = err_h,
    min_val = min_val,
    max_val = max_val,
    allow_na = allow_na,
    allow_null = allow_null,
    allow_inf = allow_inf
  )
  if (!is.null(x)) {
    if (allow_inf && !is.na(x) && x == Inf)
      return(invisible(obj))
    if (!is.na(x) && as.integer(x) != x)
      paste(text, "ist keine ganze Zahl") %>% err_h
  }
  invisible(obj)
}

#' Check that a specific list element is a numeric vector
#' 
#' @inheritParams check_numeric
#' @inheritParams check_character_vector
check_numeric_vector <- function(
  obj, 
  attr, 
  err_h, 
  min_val = 0,
  max_val = Inf,
  allow_na = FALSE,
  allow_null = FALSE,
  allow_na_last = FALSE
) {
  x <- obj[[attr]]
  text <- paste("Das Argument", stringify(attr))
  if (!allow_null && is.null(x))
    paste(text, "fehlt.") %>% err_h 
  if (!is.null(x)) {
    if (!is.numeric(x))
      paste(text, "ist kein Numeric-Vektor.") %>% err_h
    if (length(x) == 0)
      paste(text, "ist ein leerer Vektor.") %>% err_h
    if (!allow_na_last && !allow_na && any(is.na(x)))
      paste(text, "beinhaltet 'NA'-Werte") %>% err_h
    if (any(!is.na(x) & !is.finite(x)))
      paste(text, "beinhaltet nicht finite Werte ('NaN', '-Inf', 'Inf')") %>% err_h
    if (!allow_na_last && !allow_na && any(is.na(x[seq_len(length(x)- 1)])))
      paste(
        text,
        "darf nur im letzten Eintrag 'NA'-Werte beinhalten,",
        "es sind jedoch schon vorher 'NA'-Werte vorhanden."
      ) %>% err_h
    if (!is.null(min_val) && any(!is.na(x) & x < min_val))
      paste(text, "beinhaltet Elemente, die Wert kleiner", stringify(min_val), "haben.") %>% err_h
    if (any(!is.na(x) & x > max_val))
      paste(text, "beinhaltet Elemente, die Werte groesser", stringify(max_val), "haben.") %>% err_h
  }
  invisible(obj)
}

#' Check that a specific list element is an integer vector
#' 
#' @param ascending An optional argument, defining if the numbers have to be strictly ascending.
#' @inheritParams check_numeric_vector
check_integer_vector <- function(
  obj, 
  attr, 
  err_h, 
  min_val = 0,
  max_val = Inf,
  allow_na = FALSE, 
  ascending = FALSE, 
  allow_null = FALSE,
  allow_na_last = FALSE
) {
  x <- obj[[attr]]
  text <- paste("Das Argument", stringify(attr))
  check_numeric_vector(
    obj,
    attr,
    err_h = err_h,
    min_val = min_val,
    max_val = max_val,
    allow_na = allow_na,
    allow_null = allow_null,
    allow_na_last = allow_na_last
  )
  if (!is.null(x)) {
    if (any(!is.na(x) & as.integer(x) != x))
      paste(text, "beinhaltet Elemente, die keine ganzen Zahlen sind.") %>% err_h
    if (ascending && any(diff(x[!is.na(x)]) <= 0))
      paste(text, "ist keine aufsteigende Zahlenfolge.") %>% err_h
  }
  invisible(obj)
}

#' Check that a specific list element is a logical value
#' 
#' @inheritParams check_numeric
check_logical <- function(
  obj,
  attr,
  err_h,
  allow_na = FALSE,
  allow_null = FALSE
) {
  x <- obj[[attr]]
  text <- paste("Das Argument", stringify(attr))
  if (!allow_null && is.null(x))
    paste(text, "fehlt.") %>% err_h 
  if (!is.null(x)) {
    if (is.null(x))
      paste(text, "fehlt.") %>% err_h
    if (!is.logical(x))
      paste(text, "ist nicht vom Typ 'logical'.") %>% err_h
    if (length(x) != 1)
      paste(text, "muss Vektor-Laenge 1 haben.") %>% err_h
    if (!allow_na && is.na(x))
      paste(text, "darf nicht 'NA' sein.") %>% err_h
  }
  invisible(obj)
}


#' Check that a specific list element is missing 
#' 
#' Attribute is expected to be missing.
#' @inheritParams check_numeric
check_missing <- function(obj, attr, err_h) {
  x <- obj[[attr]]
  text <- paste("Das Argument", stringify(attr))
  if (!is.null(x))
    paste(text, "ist nicht zulaessig.") %>% err_h
  invisible(obj)
}

#' Check that two specific list elements have the same vector length
#' 
#' @inheritParams check_numeric
#' @param attr1 First attribute to compare length with.
#' @param attr2 Second attribute to compare length with.
check_same_length <- function(
  obj,
  attr1,
  attr2,
  err_h,
  allow_null = FALSE
) {
  x1 <- obj[[attr1]]
  x2 <- obj[[attr2]]
  if (!allow_null && is.null(x1))
    paste("Das Argument", stringify(attr1), "fehlt.") %>% err_h
  if (!allow_null && is.null(x2))
    paste("Das Argument", stringify(attr2), "fehlt.") %>% err_h
  if (!is.null(x1) && !is.null(x2) && length(x1) != length(x2))
    paste(
      "Das Argument", stringify(attr1), "hat Vektor-Laenge", length(x1),
      "waehrend das Argument", stringify(attr2), "Vektor-Laenge", length(x2),
      "hat. Die beiden Argumente muessen aber gleiche Vektor-Laenge haben."
    ) %>% err_h
  invisible(obj)
}
