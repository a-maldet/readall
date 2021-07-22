test_that("'read_data_sas()' works for default case", {
  mockup <- mockup_file_structures[[7]]
  data <- do.call(
    read_data_sas,
    args = mockup_new_file_structure_sas_args(
      mockup$struc,
      file_path = mockup$file_path
    ) %>%
      `[[<-`("adapters", new_adapters(
        function(x) {
          x$a2 <- haven::zap_empty(x$a2)
          x
        },
        function(x) {
          x$a3 <- as.logical(x$a3)
          x
        }
      ))
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
      NULL,
      new_col_meta(
        desc = "b4",
        values = 1:2
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

test_that("'read_data_excel()' works for special case", {
  mockup <- mockup_file_structures[[8]]
  mockup$struc$adapters <- new_adapters(
    function(x) {
      x$a2 <- haven::zap_empty(x$a2)
      x
    },
    function(x) {
      x$a3 <- as.logical(x$a3)
      x
    }
  )
  data <- do.call(
    read_data_sas,
    args = mockup_new_file_structure_sas_args(
      mockup$struc,
      file_path = mockup$file_path,
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
      NULL,
      new_col_meta(
        desc = "b4",
        values = 1:2
      ),
      new_col_meta(
        desc = "b5"
      )
    ) %>%
      `[[`(which(col == names(data_comp)))
    attr(data_comp[, col], "col_meta") <- meta
  }
  data_comp$x <- "val"
  data_comp$y <- mockup$file_path
  expect_dataframe_equal(data, data_comp)
})
