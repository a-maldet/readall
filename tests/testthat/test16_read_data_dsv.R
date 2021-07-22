test_that("'read_data_dsv()' works for default case", {
  mockup <- mockup_file_structures[[3]]
  mockup$struc$trim_ws <- FALSE
  mockup$struc$col_types[3] <- "character"
  data <- do.call(
    read_data_dsv,
    args = mockup_new_file_structure_dsv_args(
      mockup$struc,
      file_path = mockup$file_path,
      use_cols = TRUE
    )
  )
  data_comp <- mockup$data
  data_comp$a3 <- c("  TRUE  ", NA, "  FALSE ")
  for (col in names(data_comp)) {
    meta <- list(
      new_col_meta(
        desc = "b1",
        values = 1:2,
        values_desc = c("b11", "b12")
      ),
      new_col_meta(
        values = c(NA, 2),
        values_desc = c("b11", NA)
      ),
      new_col_meta(
        desc = "b3",
        values_desc = c("b11", "b12")
      ),
      new_col_meta(
        desc = "b5"
      )
    ) %>%
      `[[`(which(col == names(data_comp)))
    attr(data_comp[, col], "col_meta") <- meta
  }
  expect_dataframe_equal(data, data_comp)
})

test_that("'read_data_dsv()' works for special case", {
  mockup <- mockup_file_structures[[4]]
  mockup$struc$adapters <- new_adapters(
    function(x) {
      x$b1 <- x$b1 + 1
      x
    },
    function(x) {
      x$b3 <- !x$b3
      x
    }
  )
  data <- do.call(
    read_data_dsv,
    args = mockup_new_file_structure_dsv_args(
      mockup$struc,
      file_path = mockup$file_path,
      use_cols = FALSE,
      extra_col_name = "x",
      extra_col_val = "val",
      extra_col_file_path = "y"
    )
  )
  data_comp <- mockup$data
  for (col in names(data_comp)) {
    meta <- list(
      new_col_meta(
        desc = "b1",
        values = 1:2,
        values_desc = c("b11", "b12")
      ),
      new_col_meta(
        values = c(NA, 2),
        values_desc = c("b11", NA)
      ),
      new_col_meta(
        desc = "b3",
        values_desc = c("b11", "b12")
      ),
      new_col_meta(
        desc = "b5"
      )
    ) %>%
      `[[`(which(col == names(data_comp)))
    attr(data_comp[, col], "col_meta") <- meta
  }
  data_comp$b1 <- data_comp$b1 + 1
  data_comp$b2 <- trimws(data_comp$b2)
  data_comp$b3 <- !data_comp$b3
  data_comp$x <- "val"
  data_comp$y <- mockup$file_path
  expect_dataframe_equal(data, data_comp)
})

test_that("'read_data_dsv()' works for special case with 'cols_keep'", {
  mockup <- mockup_file_structures[[4]]
  mockup$struc$adapters <- new_adapters(
    function(x) {
      x$b1 <- x$b1 + 1
      x
    },
    function(x) {
      x$b3 <- !x$b3
      x
    }
  )
  data <- do.call(
    read_data_dsv,
    args = mockup_new_file_structure_dsv_args(
      mockup$struc,
      file_path = mockup$file_path,
      use_cols = FALSE,
      extra_col_name = "x",
      extra_col_val = "val",
      extra_col_file_path = "y",
      cols_keep = c("b2", "b3")
    )
  )
  data_comp <- mockup$data[, c("b2", "b3")]
  for (col in names(data_comp)) {
    meta <- list(
      new_col_meta(
        values = c(NA, 2),
        values_desc = c("b11", NA)
      ),
      new_col_meta(
        desc = "b3",
        values_desc = c("b11", "b12")
      )
    ) %>%
      `[[`(which(col == names(data_comp)))
    attr(data_comp[, col], "col_meta") <- meta
  }
  data_comp$b2 <- trimws(data_comp$b2)
  data_comp$b3 <- !data_comp$b3
  data_comp$x <- "val"
  data_comp$y <- mockup$file_path
  expect_dataframe_equal(data, data_comp)
})
