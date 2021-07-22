test_that("'new_file_structure_excel()' works with cols", {
  mockup_struc <- mockup_file_structures[[5]]$struc
  struc <- do.call(
    new_file_structure_excel,
    args = mockup_new_file_structure_excel_args(
      mockup_struc,
      use_cols = T
    )
  )
  struc_comp <- structure(
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
      adapters = new_adapters()
    ),
    class = c("file_structure_excel", "file_structure")
  )
  expect_class_objects_equal(struc, struc_comp)
})

test_that("'new_file_structure_excel()' works without cols", {
  mockup_struc <- mockup_file_structures[[6]]$struc
  struc <- do.call(
    new_file_structure_excel,
    args = mockup_new_file_structure_excel_args(
      mockup_struc,
      use_cols = F
    ) %>%
      `[[<-`("adapters", new_adapters(sin, cos))
  )
  struc_comp <- structure(
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
      adapters = new_adapters(sin, cos)
    ),
    class = c("file_structure_excel", "file_structure")
  )
  expect_class_objects_equal(struc, struc_comp)
})
