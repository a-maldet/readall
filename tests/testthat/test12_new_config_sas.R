test_that("'new_file_definition_sas()' works for default case", {
  mockup_struc <- mockup_file_structures[[7]]$struc
  file_definition <- do.call(
    new_file_definition_sas,
    args = mockup_new_file_structure_sas_args(
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
      skip_rows = 0,
      n_max = Inf,
      encoding = NULL,
      to_lower = TRUE,
      rename_cols = FALSE,
      retype_cols = FALSE,
      adapters = new_adapters(),
      file_path = "test",
      cols_keep = c("a", "b"),
      extra_col_name = "x",
      extra_col_val = 1,
      extra_col_file_path = "c"
    ),
    class = c("file_definition_sas", "file_definition", "file_structure_sas", "file_structure")
  )
  expect_class_objects_equal(file_definition, file_definition_comp)
})

test_that("'new_file_definition_sas()' works for special case", {
  mockup_struc <- mockup_file_structures[[8]]$struc
  file_definition <- do.call(
    new_file_definition_sas,
    args = c(
      file_path = "test",
      mockup_new_file_structure_sas_args(mockup_struc) %>%
        `[[<-`("adapters", new_adapters(sin, cos))
    )
  )
  file_definition_comp <- structure(
    list(
      specification_files = NULL,
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
      skip_rows = 2,
      n_max = 6,
      encoding = NULL,
      to_lower = TRUE,
      rename_cols = FALSE,
      retype_cols = FALSE,
      adapters = new_adapters(sin, cos),
      file_path = "test",
      cols_keep = TRUE,
      extra_col_name = NULL,
      extra_col_val = NULL,
      extra_col_file_path = FALSE
    ),
    class = c("file_definition_sas", "file_definition", "file_structure_sas", "file_structure")
  )
  expect_class_objects_equal(file_definition, file_definition_comp)
})

