test_that("'new_file_collection()' works", {
  file_collection <- new_file_collection(
    do.call(
      new_file_definition_fwf,
      args = mockup_new_file_structure_fwf_args(
        mockup_file_structures[[2]]$struc,
        use_cols = FALSE,
        file_path = "test",
        cols_keep = c("a1", "b1"),
        extra_col_name = "x1",
        extra_col_val = "val1",
        extra_col_file_path = "y1",
        adapters = new_adapters(tan, function(x) x)
      )
    ),
    do.call(
      new_file_definition_dsv,
      args = mockup_new_file_structure_dsv_args(
        mockup_file_structures[[4]]$struc,
        use_cols = FALSE,
        file_path = "test",
        cols_keep = c("a2", "b2"),
        extra_col_val = 2,
        extra_col_name = "x2",
        extra_col_file_path = "y2",
        adapters = new_adapters(cos, tan)
      )
    ),
    do.call(
      new_file_definition_excel,
      args = mockup_new_file_structure_excel_args(
        mockup_file_structures[[6]]$struc,
        file_path = "test",
        cols_keep = c("a3", "b3"),
        extra_col_name = "x3",
        extra_col_val = 3,
        extra_col_file_path = "y3",
        adapters = new_adapters(sin, cos)
      )
    ),
    do.call(
      new_file_definition_sas,
      args = mockup_new_file_structure_sas_args(
        mockup_file_structures[[8]]$struc,
        file_path = "test",
        cols_keep = c("a4", "b4"),
        extra_col_name = "x4",
        extra_col_val = "val4",
        extra_col_file_path = "y4",
        adapters = new_adapters(atan, function(x) x)
      )
    ),
    cols_keep = c("c1", "c2", "c3"),
    extra_col_file_path = "file",
    extra_adapters = new_adapters(acos)
  )
  file_collection_comp <- list(
    structure(
      list(
        specification_files = NULL,
        col_names = mockup_file_structures[[2]]$struc$col_names,
        col_types = mockup_file_structures[[2]]$struc$col_types,
        col_start = mockup_file_structures[[2]]$struc$col_start,
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
        adapters = new_adapters(tan, function(x) x, acos),
        file_path = "test",
        extra_col_name = "x1",
        extra_col_val = "val1",
        cols_keep = c("c1", "c2", "c3"),
        extra_col_file_path = "file"
      ),
      class = c("file_definition_fwf", "file_definition", "file_structure_fwf", "file_structure")
    ),
    structure(
      list(
        specification_files = NULL,
        col_names = mockup_file_structures[[4]]$struc$col_names,
        col_types = mockup_file_structures[[4]]$struc$col_types,
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
        sep = "|",
        na = "na",
        decimal_mark = ",",
        big_mark = ".",
        trim_ws = TRUE,
        n_max = Inf,
        encoding = "UTF-8",
        to_lower = TRUE,
        rename_cols = FALSE,
        adapters = new_adapters(cos, tan, acos),
        file_path = "test",
        extra_col_name = "x2",
        extra_col_val = 2,
        cols_keep = c("c1", "c2", "c3"),
        extra_col_file_path = "file"
      ),
      class = c("file_definition_dsv", "file_definition", "file_structure_dsv", "file_structure")
    ),
    structure(
      list(
        specification_files = NULL,
        sheet = "Tabelle2",
        range = "B2:F6",
        col_names = mockup_file_structures[[6]]$struc$col_names,
        col_types = mockup_file_structures[[6]]$struc$col_types,
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
        adapters = new_adapters(sin, cos, acos),
        file_path = "test",
        extra_col_name = "x3",
        extra_col_val = 3,
        cols_keep = c("c1", "c2", "c3"),
        extra_col_file_path = "file"
      ),
      class = c("file_definition_excel", "file_definition", "file_structure_excel", "file_structure")
    ),
    structure(
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
        adapters = new_adapters(atan, function(x) x, acos),
        file_path = "test",
        extra_col_name = "x4",
        extra_col_val = "val4",
        cols_keep = c("c1", "c2", "c3"),
        extra_col_file_path = "file"
      ),
      class = c("file_definition_sas", "file_definition", "file_structure_sas", "file_structure")
    )
  )
  expect_file_collection_equal(file_collection, file_collection_comp)
})

