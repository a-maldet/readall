test_that("'read_data_fwf()' works for default case", {
  mockup <- mockup_file_structures[[1]]
  mockup$struc$trim_ws <- FALSE
  mockup$struc$col_types[3] <- "character"
  data <- do.call(
    read_data_fwf,
    args = mockup_new_file_structure_fwf_args(
      mockup$struc,
      file_path = mockup$file_path,
      use_cols = TRUE
    )
  )
  data_comp <- mockup$data
  data_comp[2, 2] <- "       "
  data_comp$x3 <- c(" TRUE  ", "       ", "FALSE  ")
  
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

test_that("'read_data_dsv()' works special case", {
  mockup <- mockup_file_structures[[2]]
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
    read_data_fwf,
    args = mockup_new_file_structure_fwf_args(
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

test_that("'read_data_dsv()' works special case with 'cols_keep'", {
  mockup <- mockup_file_structures[[2]]
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
    read_data_fwf,
    args = mockup_new_file_structure_fwf_args(
      mockup$struc,
      file_path = mockup$file_path,
      use_cols = FALSE,
      extra_col_name = "x",
      extra_col_val = "val",
      extra_col_file_path = "y",
      cols_keep = c("b3", "b5")
    )
  )
  data_comp <- mockup$data[, c("b3", "b5")]
  for (col in names(data_comp)) {
    meta <- list(
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
  data_comp$b3 <- !data_comp$b3
  data_comp$x <- "val"
  data_comp$y <- mockup$file_path
  expect_dataframe_equal(data, data_comp)
  df_meta <- get_meta(data)
  expect_dataframe_equal(
    df_meta,
    data.frame(
      col_name = c(paste0("b", c(3, 3, 5)), "x", "y"),
      col_id = c(1, 1, 2, 3, 4),
      col_type = c("logical", "logical", "numeric", rep("character", 2)),
      col_desc = c("b3", "b3", "b5", NA, NA),
      col_values = c(NA, NA, NA, NA, NA) %>% as.character,
      col_values_desc = c("b11", "b12", NA, NA, NA),
      col_valid_start = rep(NA_character_, 5),
      col_valid_end = rep(NA_character_, 5)
    )
  )
  df_meta <- get_meta(data, cols = c(2, 3))
  expect_dataframe_equal(
    df_meta,
    data.frame(
      col_name = c("b5", "x"),
      col_id = c(2, 3),
      col_type = c("numeric", "character"),
      col_desc = c("b5", NA),
      col_values = c(NA, NA) %>% as.character,
      col_values_desc = c(NA, NA) %>% as.character,
      col_valid_start = rep(NA_character_, 2),
      col_valid_end = rep(NA_character_, 2)
    )
  )
  df_meta <- get_meta(data, cols = c("b5", "b3", "y"))
  expect_dataframe_equal(
    df_meta,
    data.frame(
      col_name = c("b5", "b3", "b3", "y"),
      col_id = c(2, 1, 1, 4),
      col_type = c("numeric", "logical", "logical", "character"),
      col_desc = c("b5", "b3", "b3", NA),
      col_values = c(NA, NA, NA, NA) %>% as.character,
      col_values_desc = c(NA, "b11", "b12", NA) %>% as.character,
      col_valid_start = rep(NA_character_, 4),
      col_valid_end = rep(NA_character_, 4)
    )
  )
})
