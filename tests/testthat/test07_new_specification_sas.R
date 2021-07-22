test_that("'new_file_structure_sas()' works for default case", {
  mockup_struc <- mockup_file_structures[[7]]$struc
  struc <- do.call(
    new_file_structure_sas,
    args = mockup_new_file_structure_sas_args(
      mockup_struc
    )
  )
  struc_comp <- structure(
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
      adapters = new_adapters()
    ),
    class = c("file_structure_sas", "file_structure")
  )
  expect_class_objects_equal(struc, struc_comp)
})

test_that("'new_file_structure_sas()' works for special case", {
  mockup_struc <- mockup_file_structures[[8]]$struc
  struc <- do.call(
    new_file_structure_sas,
    args = mockup_new_file_structure_sas_args(
      mockup_struc
    ) %>%
      `[[<-`("adapters", new_adapters(sin, cos))
  )
  struc_comp <- structure(
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
      adapters = new_adapters(sin, cos)
    ),
    class = c("file_structure_sas", "file_structure")
  )
  expect_class_objects_equal(struc, struc_comp)
})
