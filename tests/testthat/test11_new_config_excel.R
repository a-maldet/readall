test_that("'new_file_definition()' works for excel", {
  mockup_struc <- mockup_file_structures[[5]]$struc
  file_definition <- do.call(
    new_file_definition_excel,
    args = mockup_new_file_structure_excel_args(
      mockup_struc,
      file_path = "test",
      cols_keep = c("a", "b"),
      extra_col_name = "x",
      extra_col_val = 1,
      extra_col_file_path = "c"
    )
  )
  file_definition_comp <- structure(
    list(
      specification_files = NULL,
      sheet = 1,
      range = NULL,
      col_names = mockup_struc$col_names,
      col_types = mockup_struc$col_types,
      file_meta = new_file_meta(
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
          desc = "b4",
          values = 1:2
        ),
        new_col_meta(
          desc = "b5"
        )
      ),
      header = TRUE,
      skip_rows = 0,
      na = "",
      trim_ws = TRUE,
      n_max = Inf,
      to_lower = TRUE,
      rename_cols = FALSE,
      adapters = new_adapters(),
      file_path = "test",
      cols_keep = c("a", "b"),
      extra_col_name = "x",
      extra_col_val = 1,
      extra_col_file_path = "c"
    ),
    class = c("file_definition_excel", "file_definition", "file_structure_excel", "file_structure")
  )
  expect_class_objects_equal(file_definition, file_definition_comp)
  df_meta <- get_meta(file_definition)
  expect_dataframe_equal(
    df_meta,
    data.frame(
      col_name = rep(NA_character_, 9),
      col_id = c(rep(1:4, each = 2), 5),
      col_type = c(
        rep(c("numeric", "character", "logical", "NULL"), each = 2),
        "integer"
      ),
      col_desc = c("b1", "b1", rep(NA, 2), "b3", "b3", "b4", "b4", "b5"),
      col_values = c(1, 2, NA, 2, NA, NA, 1, 2, NA) %>% as.character,
      col_values_desc = c("b11", "b12", "b11", NA, "b11", "b12", NA, NA, NA),
      col_valid_start = rep(NA_character_, 9),
      col_valid_end = rep(NA_character_, 9)
    )
  )
  df_meta <- get_meta(file_definition, cols = c(2, 3))
  expect_dataframe_equal(
    df_meta,
    data.frame(
      col_name = rep(NA_character_, 4),
      col_id = c(2, 2, 3, 3),
      col_type = c(rep("character", 2), rep("logical", 2)),
      col_desc = c(rep(NA, 2), "b3", "b3"),
      col_values = c(NA, 2, NA, NA) %>% as.character,
      col_values_desc = c("b11", NA, "b11", "b12"),
      col_valid_start = rep(NA_character_, 4),
      col_valid_end = rep(NA_character_, 4)
    )
  )
})

test_that("'new_file_definition_excel()' works for excel", {
  mockup_struc <- mockup_file_structures[[6]]$struc
  file_definition <- do.call(
    new_file_definition_excel,
    args = c(
      file_path = "test",
      mockup_new_file_structure_excel_args(
        mockup_struc,
        use_cols = FALSE
      ) %>%
        `[[<-`("adapters", new_adapters(sin, cos))
    )
  )
  file_definition_comp <- structure(
    list(
      specification_files = NULL,
      sheet = "Tabelle2",
      range = "B2:F6",
      col_names = mockup_struc$col_names,
      col_types = mockup_struc$col_types,
      file_meta = new_file_meta(
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
          desc = "b4",
          values = 1:2
        ),
        new_col_meta(
          desc = "b5"
        )
      ),
      header = FALSE,
      skip_rows = 0,
      na = "na",
      trim_ws = TRUE,
      n_max = Inf,
      to_lower = TRUE,
      rename_cols = FALSE,
      adapters = new_adapters(sin, cos),
      file_path = "test",
      cols_keep = TRUE,
      extra_col_name = NULL,
      extra_col_val = NULL,
      extra_col_file_path = FALSE
    ),
    class = c("file_definition_excel", "file_definition", "file_structure_excel", "file_structure")
  )
  expect_class_objects_equal(file_definition, file_definition_comp)
  df_meta <- get_meta(file_definition)
  expect_dataframe_equal(
    df_meta,
    data.frame(
      col_name = c(rep(paste0("b", 1:4), each = 2), "b5"),
      col_id = c(rep(1:4, each = 2), 5),
      col_type = c(rep("numeric", 2), rep("character", 2),  rep("logical", 2), rep("NULL", 2), "integer"),
      col_desc = c("b1", "b1", rep(NA, 2), "b3", "b3", "b4", "b4", "b5"),
      col_values = c(1, 2, NA, 2, NA, NA, 1, 2, NA) %>% as.character,
      col_values_desc = c("b11", "b12", "b11", NA, "b11", "b12", NA, NA, NA),
      col_valid_start = rep(NA_character_, 9),
      col_valid_end = rep(NA_character_, 9)
    )
  )
  df_meta <- get_meta(file_definition, cols = c(2, 3))
  expect_dataframe_equal(
    df_meta,
    data.frame(
      col_name = c("b2", "b2", "b3", "b3"),
      col_id = c(2, 2, 3, 3),
      col_type = c(rep("character", 2), "logical", "logical"),
      col_desc = c(rep(NA, 2), "b3", "b3"),
      col_values = c(NA, 2, NA, NA) %>% as.character,
      col_values_desc = c("b11", NA, "b11", "b12"),
      col_valid_start = rep(NA_character_, 4),
      col_valid_end = rep(NA_character_, 4)
    )
  )
})
