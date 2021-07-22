test_that("'new_file_structure_fwf()' works with cols and col_start+sep_width", {
  mockup_struc <- mockup_file_structures[[1]]$struc
  struc <- do.call(
    new_file_structure_fwf,
    args = mockup_new_file_structure_fwf_args(
      mockup_struc,
      use_cols = T,
      use_col_pos = c("sep_width", "col_start")
    )
  )
  struc_comp <- structure(
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
      adapters = structure(list(), class = "adapters")
    ),
    class = c("file_structure_fwf", "file_structure")
  )
  expect_class_objects_equal(struc, struc_comp)
})
test_that("'new_file_structure_fwf()' works with cols and col_end+sep_width", {
  mockup_struc <- mockup_file_structures[[1]]$struc
  struc <- do.call(
    new_file_structure_fwf,
    args = mockup_new_file_structure_fwf_args(
      mockup_struc,
      use_cols = T,
      use_col_pos = c("sep_width", "col_end")
    )
  )
  struc_comp <- structure(
    list(
      specification_files = NULL,
      col_names = mockup_struc$col_names,
      col_types = mockup_struc$col_types,
      col_start = NULL,
      col_end = mockup_struc$col_end,
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
      adapters = structure(list(), class = "adapters")
    ),
    class = c("file_structure_fwf", "file_structure")
  )
  expect_class_objects_equal(struc, struc_comp)
})

test_that("'new_file_structure_fwf()' works with cols and col_widths+sep_width", {
  mockup_struc <- mockup_file_structures[[1]]$struc
  struc <- do.call(
    new_file_structure_fwf,
    args = mockup_new_file_structure_fwf_args(
      mockup_struc,
      use_cols = T,
      use_col_pos = c("sep_width", "col_widths")
    ) %>%
      `[[<-`("adapters", new_adapters(sin))
  )
  struc_comp <- structure(
    list(
      specification_files = NULL,
      col_names = mockup_struc$col_names,
      col_types = mockup_struc$col_types,
      col_start = NULL,
      col_end = NULL,
      col_widths = mockup_struc$col_widths,
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
      adapters = structure(list(list(fun = sin, desc = "Funktion 'sin'")), class = "adapters")
    ),
    class = c("file_structure_fwf", "file_structure")
  )
  expect_class_objects_equal(struc, struc_comp)
})
test_that("'new_file_structure_fwf()' works with cols and col_end+col_widths", {
  mockup_struc <- mockup_file_structures[[1]]$struc
  struc <- do.call(
    new_file_structure_fwf,
    args = mockup_new_file_structure_fwf_args(
      mockup_struc,
      use_cols = T,
      use_col_pos = c("col_end", "col_widths")
    )
  )
  struc_comp <- structure(
    list(
      specification_files = NULL,
      col_names = mockup_struc$col_names,
      col_types = mockup_struc$col_types,
      col_start = NULL,
      col_end = mockup_struc$col_end,
      col_widths = mockup_struc$col_widths,
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
      sep_width = NULL,
      skip_rows = 0,
      na = "",
      decimal_mark = ".",
      big_mark = ",",
      trim_ws = TRUE,
      n_max = Inf,
      encoding = "latin1",
      to_lower = TRUE,
      adapters = structure(list(), class = "adapters")
    ),
    class = c("file_structure_fwf", "file_structure")
  )
  expect_class_objects_equal(struc, struc_comp)
})

test_that("'new_file_structure_fwf()' works with cols and col_start+col_widths", {
  mockup_struc <- mockup_file_structures[[2]]$struc
  struc <- do.call(
    new_file_structure_fwf,
    args = mockup_new_file_structure_fwf_args(
      mockup_struc,
      use_cols = T,
      use_col_pos = c("col_widths", "col_start")
    )
  )
  struc_comp <- structure(
    list(
      specification_files = NULL,
      col_names = mockup_struc$col_names,
      col_types = mockup_struc$col_types,
      col_start = mockup_struc$col_start,
      col_end = NULL,
      col_widths = mockup_struc$col_widths,
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
      sep_width = NULL,
      skip_rows = 1,
      na = "na",
      decimal_mark = ",",
      big_mark = ".",
      trim_ws = TRUE,
      n_max = Inf,
      encoding = "latin1",
      to_lower = TRUE,
      adapters = structure(list(), class = "adapters")
    ),
    class = c("file_structure_fwf", "file_structure")
  )
  expect_class_objects_equal(struc, struc_comp)
})

test_that("'new_file_structure_fwf()' works with col_start+col_widths", {
  mockup_struc <- mockup_file_structures[[2]]$struc
  struc <- do.call(
    new_file_structure_fwf,
    args = mockup_new_file_structure_fwf_args(
      mockup_struc,
      use_cols = FALSE,
      use_col_pos = c("col_widths", "col_start"),
      adapters = new_adapters(sin)
    )
  )
  struc_comp <- structure(
    list(
      specification_files = NULL,
      col_names = mockup_struc$col_names,
      col_types = mockup_struc$col_types,
      col_start = mockup_struc$col_start,
      col_end = NULL,
      col_widths = mockup_struc$col_widths,
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
      sep_width = NULL,
      skip_rows = 1,
      na = "na",
      decimal_mark = ",",
      big_mark = ".",
      trim_ws = TRUE,
      n_max = Inf,
      encoding = "latin1",
      to_lower = TRUE,
      adapters = new_adapters(sin)
    ),
    class = c("file_structure_fwf", "file_structure")
  )
  expect_class_objects_equal(struc, struc_comp)
})

