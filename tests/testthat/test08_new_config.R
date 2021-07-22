test_that("'new_file_definition()' works for fwf", {
  mockup_struc <- mockup_file_structures[[1]]$struc
  file_definition <- new_file_definition(
    file_path = "test",
    file_structure = do.call(
      new_file_structure_fwf,
      args = mockup_new_file_structure_fwf_args(mockup_struc)
    )
  )
  file_definition_comp <- structure(
    list(
      specification_files = NULL,
      col_names = mockup_struc$col_names,
      col_types = mockup_struc$col_types,
      col_start = mockup_struc$col_start,
      col_end = NULL,
      col_widths = NULL,
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
      sep_width = 0,
      skip_rows = 0,
      na = "",
      decimal_mark = ".",
      big_mark = ",",
      trim_ws = TRUE,
      n_max = Inf,
      encoding = "latin1",
      to_lower = TRUE,
      adapters = new_adapters(),
      file_path = "test",
      cols_keep = TRUE,
      extra_col_name = NULL,
      extra_col_val = NULL,
      extra_col_file_path = FALSE
    ),
    class = c("file_definition_fwf", "file_definition", "file_structure_fwf", "file_structure")
  )
  expect_class_objects_equal(file_definition, file_definition_comp)
})

test_that("'new_file_definition()' works for fwf", {
  mockup_struc <- mockup_file_structures[[2]]$struc
  file_definition <- new_file_definition(
    file_path = "test",
    file_structure = do.call(
      new_file_structure_fwf,
      args = mockup_new_file_structure_fwf_args(
        mockup_struc,
        use_cols = FALSE
      )
    ),
    cols_keep = c("a", "b"),
    extra_col_name = "x",
    extra_col_val = "val",
    extra_col_file_path = "y"
  )
  file_definition_comp <- structure(
    list(
      specification_files = NULL,
      col_names = mockup_struc$col_names,
      col_types = mockup_struc$col_types,
      col_start = mockup_struc$col_start,
      col_end = NULL,
      col_widths = NULL,
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
      sep_width = 1,
      skip_rows = 1,
      na = "na",
      decimal_mark = ",",
      big_mark = ".",
      trim_ws = TRUE,
      n_max = Inf,
      encoding = "latin1",
      to_lower = TRUE,
      adapters = new_adapters(),
      file_path = "test",
      cols_keep = c("a", "b"),
      extra_col_name = "x",
      extra_col_val = "val",
      extra_col_file_path = "y"
    ),
    class = c("file_definition_fwf", "file_definition", "file_structure_fwf", "file_structure")
  )
  expect_class_objects_equal(file_definition, file_definition_comp)
})

test_that("'new_file_definition()' works for dsv", {
  mockup_struc <- mockup_file_structures[[3]]$struc
  file_definition <- new_file_definition(
    file_path = "test",
    file_structure = do.call(
      new_file_structure_dsv,
      args = mockup_new_file_structure_dsv_args(mockup_struc)
    )
  )
  file_definition_comp <- structure(
    list(
      specification_files = NULL,
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
      sep = ";",
      header = TRUE,
      skip_rows = 0,
      na = "",
      decimal_mark = ".",
      big_mark = ",",
      trim_ws = TRUE,
      n_max = Inf,
      encoding = "latin1",
      to_lower = TRUE,
      rename_cols = FALSE,
      adapters = new_adapters(),
      file_path = "test",
      cols_keep = TRUE,
      extra_col_name = NULL,
      extra_col_val = NULL,
      extra_col_file_path = FALSE
    ),
    class = c("file_definition_dsv", "file_definition", "file_structure_dsv", "file_structure")
  )
  expect_class_objects_equal(file_definition, file_definition_comp)
})

test_that("'new_file_definition()' works for dsv", {
  mockup_struc <- mockup_file_structures[[4]]$struc
  file_definition <- new_file_definition(
    file_path = "test",
    file_structure = do.call(
      new_file_structure_dsv,
      args = mockup_new_file_structure_dsv_args(mockup_struc)
    )
  )
  file_definition_comp <- structure(
    list(
      specification_files = NULL,
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
      sep = "|",
      header = FALSE,
      skip_rows = 0,
      na = "na",
      decimal_mark = ",",
      big_mark = ".",
      trim_ws = TRUE,
      n_max = Inf,
      encoding = "UTF-8",
      to_lower = TRUE,
      rename_cols = FALSE,
      adapters = new_adapters(),
      file_path = "test",
      cols_keep = TRUE,
      extra_col_name = NULL,
      extra_col_val = NULL,
      extra_col_file_path = FALSE
    ),
    class = c("file_definition_dsv", "file_definition", "file_structure_dsv", "file_structure")
  )
  expect_class_objects_equal(file_definition, file_definition_comp)
})

test_that("'new_file_definition()' works for excel", {
  mockup_struc <- mockup_file_structures[[5]]$struc
  file_definition <- new_file_definition(
    file_path = "test",
    file_structure = do.call(
      new_file_structure_excel,
      mockup_new_file_structure_excel_args(mockup_struc)
    ),
    cols_keep = c("a", "b"),
    extra_col_name = "x",
    extra_col_val = 1,
    extra_col_file_path = "c"
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
})

test_that("'new_file_definition()' works for excel", {
  mockup_struc <- mockup_file_structures[[6]]$struc
  file_definition <- new_file_definition(
    file_path = "test",
    file_structure = do.call(
      new_file_structure_excel,
      mockup_new_file_structure_excel_args(mockup_struc)
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
      adapters = new_adapters(),
      file_path = "test",
      cols_keep = TRUE,
      extra_col_name = NULL,
      extra_col_val = NULL,
      extra_col_file_path = FALSE
    ),
    class = c("file_definition_excel", "file_definition", "file_structure_excel", "file_structure")
  )
  expect_class_objects_equal(file_definition, file_definition_comp)
})

test_that("'new_file_definition()' works for sas default case", {
  mockup_struc <- mockup_file_structures[[7]]$struc
  file_definition <- new_file_definition(
    file_path = "test",
    file_structure = do.call(
      new_file_structure_sas,
      mockup_new_file_structure_sas_args(mockup_struc)
    ),
    cols_keep = c("a", "b"),
    extra_col_name = "x",
    extra_col_val = 1,
    extra_col_file_path = "c"
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

test_that("'new_file_definition()' works for sas special case", {
  mockup_struc <- mockup_file_structures[[8]]$struc
  file_definition <- new_file_definition(
    file_path = "test",
    file_structure = do.call(
      new_file_structure_sas,
      args = mockup_new_file_structure_sas_args(mockup_struc) %>%
        `[[<-`("adapters", new_adapters(sin))
    ),
    extra_adapters = new_adapters(cos)
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
