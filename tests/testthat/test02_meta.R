test_that("'new_col_meta()' works", {
  meta <- new_col_meta(desc = "bla", values = 1:2, values_desc = c("a1", "a2"))
  expect_class_objects_equal(
    meta,
    structure(
      list(
        desc = "bla",
        values = 1:2,
        values_desc = c("a1", "a2"),
        valid_start = NULL,
        valid_end = NULL
      ),
      class = "col_meta"
    )
  )
})

test_that("'new_col_meta()' works without 'desc'", {
  meta <- new_col_meta(values = 1:2, values_desc = c("a1", "a2"))
  expect_class_objects_equal(
    meta,
    structure(
      list(
        desc = NULL,
        values = 1:2,
        values_desc = c("a1", "a2"),
        valid_start = NULL,
        valid_end = NULL
      ),
      class = "col_meta"
    )
  )
})

test_that("'new_col_meta()' works without 'desc' and 'values'", {
  meta <- new_col_meta(values_desc = c("a1", "a2"))
  expect_class_objects_equal(
    meta,
    structure(
      list(
        desc = NULL,
        values = NULL,
        values_desc = c("a1", "a2"),
        valid_start = NULL,
        valid_end = NULL
      ),
      class = "col_meta"
    )
  )
})

test_that("'new_col_meta()' works with no arguments", {
  meta <- new_col_meta()
  expect_class_objects_equal(
    meta,
    structure(
      list(
        desc = NULL,
        values = NULL,
        values_desc = NULL,
        valid_start = NULL,
        valid_end = NULL
      ),
      class = "col_meta"
    )
  )
})

test_that("'new_file_meta()' works", {
  file_meta <- new_file_meta(
    new_col_meta(desc = "bla1", values = 1:3, values_desc = c("a1", "a2", "a3")),
    new_col_meta(desc = "bla2", values = 1:2, values_desc = c("a1", "a2"))
  )
  expect_class_objects_equal(
    file_meta,
    structure(
      list(
        structure(
          list(
            desc = "bla1",
            values = 1:3,
            values_desc = c("a1", "a2", "a3"),
            valid_start = NULL,
            valid_end = NULL
          ),
          class = "col_meta"
        ),
        structure(
          list(
            desc = "bla2",
            values = 1:2,
            values_desc = c("a1", "a2"),
            valid_start = NULL,
            valid_end = NULL
          ),
          class = "col_meta"
        )
      ),
      class = "file_meta"
    )
  )
})

test_that("'new_file_meta()' works with no meta entries", {
  file_meta <- new_file_meta()
  expect_class_objects_equal(
    file_meta,
    structure(
      list(),
      class = "file_meta"
    )
  )
})
