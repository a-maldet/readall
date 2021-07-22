test_that("'read_data()' works for 'file_definition_fwf' class objects for default", {
  mockup <- mockup_file_structures[[1]]
  mockup$struc$trim_ws <- FALSE
  mockup$struc$col_types[3] <- "character"
  file_definition <- do.call(
    new_file_definition_fwf,
    args = mockup_new_file_structure_fwf_args(
      mockup$struc,
      file_path = mockup$file_path,
      use_cols = TRUE
    )
  )
  data <- read_data(file_definition)
  data_comp <- mockup$data
  data_comp[, "x3"] <- c(" TRUE  ", "       ", "FALSE  ")
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
  data_comp[2, 2] <- "       "
  expect_dataframe_equal(data, data_comp)
})

test_that("'read_data()' works for 'file_definition_fwf' class objects with 'cols_keep'", {
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
  file_definition <- do.call(
    new_file_definition_fwf,
    args = mockup_new_file_structure_fwf_args(
      mockup$struc,
      file_path = mockup$file_path,
      use_cols = FALSE,
      extra_col_name = "x",
      extra_col_val = "val",
      extra_col_file_path = "y",
      cols_keep = c("b1", "b3")
    )
  )
  data <- read_data(file_definition)
  data_comp <- mockup$data[, c("b1", "b3")]
  for (col in names(data_comp)) {
    meta <- list(
      new_col_meta(
        desc = "b1",
        values = 1:2,
        values_desc = c("b11", "b12")
      ),
      new_col_meta(
        desc = "b3",
        values_desc = c("b11", "b12")
      )
    ) %>%
      `[[`(which(col == names(data_comp)))
    attr(data_comp[, col], "col_meta") <- meta
  }
  data_comp$b1 <- data_comp$b1 + 1
  data_comp$b3 <- !data_comp$b3
  data_comp$x <- "val"
  data_comp$y <- mockup$file_path
  expect_dataframe_equal(data, data_comp)
})

test_that("'read_data()' works for 'file_definition_dsv' class objects for default", {
  mockup <- mockup_file_structures[[3]]
  mockup$struc$trim_ws <- FALSE
  mockup$struc$col_types[3] <- "character"
  file_definition <- do.call(
    new_file_definition_dsv,
    args = mockup_new_file_structure_dsv_args(
      mockup$struc,
      file_path = mockup$file_path,
      use_cols = TRUE
    )
  )
  data <- read_data(file_definition)
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

test_that("'read_data()' works for 'file_definition_dsv' class objects", {
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
  file_definition <- do.call(
    new_file_definition_dsv,
    args = mockup_new_file_structure_dsv_args(
      mockup$struc,
      file_path = mockup$file_path,
      use_cols = FALSE,
      extra_col_name = "x",
      extra_col_val = "val",
      extra_col_file_path = "y"
    )
  )
  data <- read_data(file_definition)
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

test_that("'read_data()' works for 'file_definition_dsv' class objects with 'cols_keep'", {
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
  file_definition <- do.call(
    new_file_definition_dsv,
    args = mockup_new_file_structure_dsv_args(
      mockup$struc,
      file_path = mockup$file_path,
      use_cols = FALSE,
      extra_col_name = "x",
      extra_col_val = "val",
      extra_col_file_path = "y",
      cols_keep = c("b1", "b2")
    )
  )
  data <- read_data(file_definition)
  data_comp <- mockup$data[, c("b1", "b2")]
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
      )
    ) %>%
      `[[`(which(col == names(data_comp)))
    attr(data_comp[, col], "col_meta") <- meta
  }
  data_comp$b1 <- data_comp$b1 + 1
  data_comp$b2 <- trimws(data_comp$b2)
  data_comp$x <- "val"
  data_comp$y <- mockup$file_path
  expect_dataframe_equal(data, data_comp)
})

test_that("'read_data()' works for 'file_definition_excel' class objects for default", {
  mockup <- mockup_file_structures[[5]]
  mockup$struc$trim_ws <- FALSE
  mockup$struc$col_types[3] <- "character"
  file_definition <- do.call(
    new_file_definition_excel,
    args = mockup_new_file_structure_excel_args(
      mockup$struc,
      file_path = mockup$file_path,
      use_cols = TRUE
    )
  )
  data <- read_data(file_definition)
  data_comp <- mockup$data
  data_comp$a3 <- c("  TRUE  ", NA, "    FALSE ")
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

test_that("'read_data()' works for 'file_definition_excel' class objects", {
  mockup <- mockup_file_structures[[6]]
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
  file_definition <- do.call(
    new_file_definition_excel,
    args = mockup_new_file_structure_excel_args(
      mockup$struc,
      file_path = mockup$file_path,
      use_cols = FALSE,
      extra_col_name = "x",
      extra_col_val = "val",
      extra_col_file_path = "y"
    )
  )
  data <- read_data(file_definition)
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

test_that("'read_data()' works for 'file_definition_excel' class objects with 'sheet = 2' and missing range", {
  mockup <- mockup_file_structures[[6]]
  mockup$struc$sheet <- 2
  mockup$struc$range <- NULL
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
  file_definition <- do.call(
    new_file_definition_excel,
    args = mockup_new_file_structure_excel_args(
      mockup$struc,
      file_path = mockup$file_path,
      use_cols = FALSE,
      extra_col_name = "x",
      extra_col_val = "val",
      extra_col_file_path = "y"
    )
  )
  data <- read_data(file_definition)
  data_comp <- mockup$data[1:4,]
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

test_that("'read_data()' works for 'file_definition_excel' class objects with 'cols_keep' and missing range", {
  mockup <- mockup_file_structures[[6]]
  mockup$struc$range <- NULL
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
  file_definition <- do.call(
    new_file_definition_excel,
    args = mockup_new_file_structure_excel_args(
      mockup$struc,
      file_path = mockup$file_path,
      use_cols = FALSE,
      extra_col_name = "x",
      extra_col_val = "val",
      extra_col_file_path = "y",
      cols_keep = c("b5", "b1")
    )
  )
  data <- read_data(file_definition)
  data_comp <- mockup$data[1:4, c("b5", "b1")]
  for (col in names(data_comp)) {
    meta <- list(
      new_col_meta(
        desc = "b5"
      ),
      new_col_meta(
        desc = "b1",
        values = 1:2,
        values_desc = c("b11", "b12")
      )
    ) %>%
      `[[`(which(col == names(data_comp)))
    attr(data_comp[, col], "col_meta") <- meta
  }
  data_comp$b1 <- data_comp$b1 + 1
  data_comp$x <- "val"
  data_comp$y <- mockup$file_path
  expect_dataframe_equal(data, data_comp)
})

test_that("'read_data()' works for 'file_definition_sas' class objects", {
  mockup <- mockup_file_structures[[8]]
    mockup$struc$adapters <- new_adapters(
    function(x) {
      x$a3 <- as.logical(x$a3)
      x
    },
    function(x) {
      x$a1 <- x$a1 + 1
      x
    },
    function(x) {
      x$a2 <- haven::zap_empty(x$a2)
      x
    },
    function(x) {
      y <- x
      x <- y
      x$a3 <- !x$a3
      x
    }
  )
  file_definition <- do.call(
    new_file_definition_sas,
    args = mockup_new_file_structure_sas_args(
      mockup$struc %>%
        `[[<-`("encoding", NULL) %>%
        `[[<-`("n_max", 6),
      file_path = mockup$file_path,
      extra_col_name = "x",
      extra_col_val = "val",
      extra_col_file_path = "y"
    )
  )
  data <- read_data(file_definition)
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
  data_comp$a1 <- data_comp$a1 + 1
  data_comp$a2 <- trimws(data_comp$a2, which = "right")
  data_comp$a4 <- trimws(data_comp$a4, which = "right")
  data_comp$a3 <- !data_comp$a3
  data_comp$x <- "val"
  data_comp$y <- mockup$file_path
  expect_dataframe_equal(data, data_comp)
})


