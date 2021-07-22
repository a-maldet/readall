#' Check that two class objects of type list are equal (except of the order)
#'
#' @param obj1 A list, which is also a class object and should be compared to `obj2`
#' @param obj2 A list, which is also a class object and should be compared to `obj1`
#' @param pre_text A string that should be put before the error message
expect_class_objects_equal <-function(obj1, obj2, pre_text = "") {
  # 1. Capture object and label
  act <- quasi_label(rlang::enquo(obj1), arg = "object")
  # 2. Call expect()
  expect(
    is.list(act$val),
    sprintf("Object '%s' is not a list.", act$lab)
  )
  class1 <- class(act$val)
  class2 <- class(obj2)
  expect(
    identical(class1, class2),
    sprintf(
      "%sObject '%s' has not the right classes assigned to\nHas classes: %s\nShould have: %s\n",
      pre_text,
      act$lab,
      stringify(class1),
      stringify(class2)
    )
  )
  names1 <- names(act$val)
  names2 <- names(obj2)
  missing_names <- names2[!names2 %in% names1]
  expect(
    length(missing_names) == 0,
    sprintf(
      "%sObject '%s' has not the right list entries. Following entries are missing:\n\t%s",
      pre_text,
      act$lab,
      stringify(missing_names)
    )
  )
  wrong_names <- names1[!names1 %in% names2]
  expect(
    length(wrong_names) == 0,
    sprintf(
      "%sObject '%s' has not the right list entries. Following entries are not allowed:\n\t%s",
      pre_text,
      act$lab,
      stringify(wrong_names)
    )
  )
  lapply(
    names1,
    function(var) {
      expect(
        identical(act$val[[var]], obj2[[var]]),
        sprintf(
          "%sObject '%s' has not the right value in list entry '%s'\n\tHas entries: %s\nShould have: %s",
          pre_text,
          act$lab,
          var,
          stringify(act$val[[var]]),
          stringify(obj2[[var]])
        )
      )
    }
  ) %>% invisible
}

expect_file_collection_equal <- function(obj, obj_comp) {
  # 1. Capture object and label
  act <- quasi_label(rlang::enquo(obj), arg = "object")
  # 2. Call expect()
  expect(
    identical(class(act$val), "file_collection"),
    sprintf(
      "Object '%s' is not of class 'file_collection'.",
      act$lab
    )
  ) 
  expect(
    is.list(act$val),
    sprintf("Object '%s' is not a list.", act$lab)
  )
  expect(
    is.null(names(act$val)),
    sprintf("Entries of Object '%s' are not allowed to have names.", act$lab)
  )
  expect(
    length(act$val) == length(obj_comp),
    sprintf(
      "Object '%s' should have %d entries, but has %d.",
      act$lab,
      length(obj_comp),
      length(act$val)
    )
  )
  if (length(act$val) == length(obj_comp)) {
    lapply(
      seq_len(length(act$val)),
      function(i) {
        expect_class_objects_equal(
          act$val[[i]],
          obj_comp[[i]],
          pre_text = paste0("Fehler in Eintrag-", i, " der Konfigurations-Liste: ")
        )
      }
    ) %>% invisible
  }
}

expect_dataframe_equal <- function(obj, obj_comp) {
  # 1. Capture object and label
  act <- quasi_label(rlang::enquo(obj), arg = "object")
  # 2. Call expect()
  expect(
    is.data.frame(act$val),
    sprintf("Object '%s' is not a data.frame.", act$lab)
  )
  if (is.data.frame(act$val)) {
    names1 <- names(act$val)
    names2 <- names(obj_comp)
    missing_names <- names2[!names2 %in% names1]
    expect(
      length(missing_names) == 0,
      sprintf(
        "Object '%s' has not the right columns. Following columns are missing:\n\t%s",
        act$lab,
        stringify(missing_names)
      )
    )
    wrong_names <- names1[!names1 %in% names2]
    expect(
      length(wrong_names) == 0,
      sprintf(
        "Object '%s' has not the right columns. Following columns are not allowed:\n\t%s",
        act$lab,
        stringify(wrong_names)
      )
    )
    expect(
      nrow(act$val) == nrow(obj_comp),
      sprintf(
        "Object '%s' should have %d rows, but has %d.",
        act$lab,
        nrow(obj_comp),
        nrow(act$val)
      )
    )
    if (nrow(act$val) == nrow(obj_comp) && length(missing_names) == 0) {
      lapply(
        names2,
        function(var) {
          expect(
            isTRUE(all.equal(
              act$val[[var]],
              obj_comp[[var]],
              check.attributes = FALSE
            )),
            paste0(
              "Fehler in Spalte ", stringify(var), ": \nSollwerte: ",
              stringify(obj_comp[[var]]), "\nIst-Werte: ", stringify(act$val[[var]]), "\n"
            )
          )
        }
      ) %>% invisible
      lapply(
        names2,
        function(var) {
          expect(
            identical(attr(act$val[[var]], "col_meta"), attr(obj_comp[[var]], "col_meta")),
            paste0("Falsche Meta-Information in Spalte ", stringify(var))
          )
        }
      ) %>% invisible
    }
  }
}
