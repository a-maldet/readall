test_that("'new_file_collection()' works", {
  file_collection <- new_file_collection(
    do.call(
      new_file_definition_fwf,
      args = mockup_new_file_structure_fwf_args(
        mockup_file_structures[[2]]$struc %>%
          `[[<-`("col_names", c("u1", "u2", "u3", "u4", "u5")),
        use_cols = FALSE,
        file_path = mockup_file_structures[[2]]$file_path,
        cols_keep = c("a1", "b1"),
        extra_col_name = "e",
        extra_col_val = "val1",
        extra_col_file_path = "f1",
        adapters = new_adapters(
          function(x) {
            x$u1 <- x$u1 + 1
            x
          }
        )
      )
    ),
    do.call(
      new_file_definition_dsv,
      args = mockup_new_file_structure_dsv_args(
        mockup_file_structures[[4]]$struc %>%
          `[[<-`("col_names", c("u1", "u2", "u3", "u4", "u5")),
        use_cols = FALSE,
        file_path = mockup_file_structures[[4]]$file_path,
        cols_keep = c("a2", "b2"),
        extra_col_val = "val2",
        extra_col_name = "e",
        extra_col_file_path = "f2",
        adapters = new_adapters(
          function(x) {
            x$u1 <- x$u1 + 1
            x
          }
        )
      )
    ),
    do.call(
      new_file_definition_excel,
      args = mockup_new_file_structure_excel_args(
        mockup_file_structures[[6]]$struc %>%
          `[[<-`("col_names", c("u1", "u2", "u3", "u4", "u5")),
        file_path = mockup_file_structures[[6]]$file_path,
        cols_keep = c("a3", "b3"),
        extra_col_name = "e",
        extra_col_val = "val3",
        extra_col_file_path = "f3",
        adapters = new_adapters(
          function(x) {
            x$u1 <- x$u1 + 1
            x
          },
          function(x) {
            x$u3 <- !x$u3
            x
          }
        )
      )
    ),
    do.call(
      new_file_definition_sas,
      args = mockup_new_file_structure_sas_args(
        mockup_file_structures[[8]]$struc,
        file_path = mockup_file_structures[[8]]$file_path,
        cols_keep = c("a4", "b4"),
        extra_col_name = "e",
        extra_col_val = "val4",
        extra_col_file_path = "f4",
        adapters = new_adapters(
          function(x) {
            names(x) <- c("u1", "u2", "u3", "u4", "u5")
            x
          },
          function(x) {
            x$u4 <- NULL
            x
          },
          function(x) {
            x$u2 <- haven::zap_empty(x$u2)
            x
          },
          function(x) {
            x$u3 <- as.logical(x$u3)
            x
          }
        )
      )
    ),
    cols_keep = TRUE,
    extra_col_file_path = "file",
    extra_adapters = new_adapters(
      function(x) {
        x$u1 <- x$u1 + 3
        x
      }
    )
  )
  data <- read_data(file_collection)
  data_comp <- do.call(
    rbind,
    args = list(
      {
        df <- mockup_file_structures[[2]]$data
        for (col in names(df)) {
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
            `[[`(which(col == names(df)))
          attr(df[, col], "col_meta") <- meta
        }
        names(df) <- c("u1", "u2", "u3", "u5")
        df$u1 <- df$u1 + 4
        df$u2 <- trimws(df$u2)
        df$e <- "val1"
        df$file <- mockup_file_structures[[2]]$file_path
        df
      },
      {
        df <- mockup_file_structures[[4]]$data
        for (col in names(df)) {
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
            `[[`(which(col == names(df)))
          attr(df[, col], "col_meta") <- meta
        }
        names(df) <- c("u1", "u2", "u3", "u5")
        df$u1 <- df$u1 + 4
        df$u2 <- trimws(df$u2)
        df$e <- "val2"
        df$file <- mockup_file_structures[[4]]$file_path
        df
      },
      {
        df <- mockup_file_structures[[6]]$data
        for (col in names(df)) {
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
            `[[`(which(col == names(df)))
          attr(df[, col], "col_meta") <- meta
        }
        names(df) <- c("u1", "u2", "u3", "u5")
        df$u1 <- df$u1 + 4
        df$u2 <- trimws(df$u2)
        df$u3 <- !df$u3
        df$e <- "val3"
        df$file <- mockup_file_structures[[6]]$file_path
        df
      },
      {
        df <- mockup_file_structures[[8]]$data
        for (col in names(df)) {
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
            new_col_meta(),
            new_col_meta(
              desc = "b5"
            )
          ) %>%
            `[[`(which(col == names(df)))
          attr(df[, col], "col_meta") <- meta
        }
        names(df) <- c("u1", "u2", "u3", "u4", "u5")
        df$u1 <- df$u1 + 3
        df$u4 <- NULL
        df$e <- "val4"
        df$file <- mockup_file_structures[[8]]$file_path
        df
      }
    )
  )
  expect_dataframe_equal(data, data_comp)
})

test_that("'new_file_collection()' works with 'cols_keep'", {
  file_collection <- new_file_collection(
    do.call(
      new_file_definition_fwf,
      args = mockup_new_file_structure_fwf_args(
        mockup_file_structures[[1]]$struc%>%
          `[[<-`("col_names", c("u1", "u2", "u3", "u4", "u5")),
        use_cols = FALSE,
        file_path = mockup_file_structures[[1]]$file_path,
        cols_keep = c("a1", "b1"),
        extra_col_name = "e",
        extra_col_val = "val1",
        extra_col_file_path = "f1",
        adapters = new_adapters(
          function(x) {
            x$u1 <- x$u1 + 1
            x
          }
        )
      )
    ),
    do.call(
      new_file_definition_dsv,
      args = mockup_new_file_structure_dsv_args(
        mockup_file_structures[[3]]$struc %>%
          `[[<-`("col_names", c("u1", "u2", "u3", "u4", "u5")) %>%
          `[[<-`("header", FALSE) %>%
          `[[<-`("skip", 1),
        use_cols = FALSE,
        file_path = mockup_file_structures[[3]]$file_path,
        cols_keep = c("a2", "b2"),
        extra_col_val = "val2",
        extra_col_name = "e",
        extra_col_file_path = "f2",
        adapters = new_adapters(
          function(x) {
            x$u1 <- x$u1 + 1
            x
          }
        )
      )
    ),
    do.call(
      new_file_definition_excel,
      args = mockup_new_file_structure_excel_args(
        mockup_file_structures[[5]]$struc %>%
          `[[<-`("col_names", c("u1", "u2", "u3", "u4", "u5")) %>%
          `[[<-`("header", FALSE) %>%
          `[[<-`("skip", 1),
        file_path = mockup_file_structures[[5]]$file_path,
        cols_keep = c("a3", "b3"),
        extra_col_name = "e",
        extra_col_val = "val3",
        extra_col_file_path = "f3",
        adapters = new_adapters(
          function(x) {
            x$u1 <- x$u1 + 1
            x
          },
          function(x) {
            x$u3 <- !x$u3
            x
          }
        )
      )
    ),
    do.call(
      new_file_definition_sas,
      args = mockup_new_file_structure_sas_args(
        mockup_file_structures[[7]]$struc,
        file_path = mockup_file_structures[[7]]$file_path,
        cols_keep = c("a4", "b4"),
        extra_col_name = "e",
        extra_col_val = "val4",
        extra_col_file_path = "f4",
        adapters = new_adapters(
          function(x) {
            names(x) <- c("u1", "u2", "u3", "u4", "u5")
            x
          },
          function(x) {
            x$u2 <- haven::zap_empty(x$u2)
            x
          },
          function(x) {
            x$u3 <- as.logical(x$u3)
            x
          }
        )
      )
    ),
    cols_keep = c("u1", "u3", "u5"),
    extra_col_file_path = FALSE,
    extra_adapters = new_adapters(
      function(x) {
        x$u1 <- x$u1 + 3
        x
      }
    )
  )
  data <- read_data(file_collection)
  data_comp <- do.call(
    rbind,
    args = list(
      {
        df <- mockup_file_structures[[1]]$data[, c(1, 3, 4)]
        for (col in names(df)) {
          meta <- list(
            new_col_meta(
              desc = "b1",
              values = 1:2,
              values_desc = c("b11", "b12")
            ),
            new_col_meta(
              desc = "b3",
              values_desc = c("b11", "b12")
            ),
            new_col_meta(
              desc = "b5"
            )
          ) %>%
            `[[`(which(col == names(df)))
          attr(df[, col], "col_meta") <- meta
        }
        names(df) <- c("u1", "u3", "u5")
        df$u1 <- df$u1 + 4
        df$e <- "val1"
        df
      },
      {
        df <- mockup_file_structures[[3]]$data[, c(1, 3, 4)]
        for (col in names(df)) {
          meta <- list(
            new_col_meta(
              desc = "b1",
              values = 1:2,
              values_desc = c("b11", "b12")
            ),
            new_col_meta(
              desc = "b3",
              values_desc = c("b11", "b12")
            ),
            new_col_meta(
              desc = "b5"
            )
          ) %>%
            `[[`(which(col == names(df)))
          attr(df[, col], "col_meta") <- meta
        }
        names(df) <- c("u1", "u3", "u5")
        df$u1 <- df$u1 + 4
        df$e <- "val2"
        df
      },
      {
        df <- mockup_file_structures[[5]]$data[, c(1, 3, 4)]
        names(df) <- c("u1", "u3", "u5")
        for (col in names(df)) {
          meta <- list(
            new_col_meta(
              desc = "b1",
              values = 1:2,
              values_desc = c("b11", "b12")
            ),
            new_col_meta(
              desc = "b3",
              values_desc = c("b11", "b12")
            ),
            new_col_meta(
              desc = "b5"
            )
          ) %>%
            `[[`(which(col == names(df)))
          attr(df[, col], "col_meta") <- meta
        }
        df$u1 <- df$u1 + 4
        df$u3 <- !df$u3
        df$e <- "val3"
        df
      },
      {
        df <- mockup_file_structures[[7]]$data[, c(1, 3, 5)]
        for (col in names(df)) {
          meta <- list(
            new_col_meta(
              desc = "b1",
              values = 1:2,
              values_desc = c("b11", "b12")
            ),
            new_col_meta(
              desc = "b3",
              values_desc = c("b11", "b12")
            ),
            new_col_meta(
              desc = "b5"
            )
          ) %>%
            `[[`(which(col == names(df)))
          attr(df[, col], "col_meta") <- meta
        }
        names(df) <- c("u1", "u3", "u5")
        df$u1 <- df$u1 + 3
        df$u4 <- NULL
        df$e <- "val4"
        df
      }
    )
  )
  expect_dataframe_equal(data, data_comp)
})

