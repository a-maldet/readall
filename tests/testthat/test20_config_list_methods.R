mock_file_collection <- new_file_collection(
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

test_that("`[.file_collection` works", {
  file_collection <- mock_file_collection[c(2, 3)]
  expect_s3_class(file_collection, "file_collection")
  expect_true(is.list(file_collection) && length(file_collection) == 2)
})

test_that("'set_cols_keep()' works", {
  file_collection <- mock_file_collection %>%
    set_cols_keep(c("r", "t"))
  lapply(
    file_collection,
    function(file_definition) expect_identical(file_definition$cols_keep, c("r", "t"))
  )
  file_collection %<>%
    set_cols_keep(TRUE)
  lapply(
    file_collection,
    function(file_definition) expect_true(isTRUE(file_definition$cols_keep))
  )
  file_definition <- file_collection[[2]] %>%
    set_cols_keep(c("i", "j"))
  expect_identical(file_definition$cols_keep, c("i", "j"))
  file_definition %<>%
    set_cols_keep(TRUE)
  expect_true(isTRUE(file_definition$cols_keep))
})

test_that("'set_extra_col_file_path()' works", {
  file_collection <- mock_file_collection %>%
    set_extra_col_file_path("xx")
  lapply(
    file_collection,
    function(file_definition) expect_identical(file_definition$extra_col_file_path, "xx")
  )
  file_collection %<>%
    set_extra_col_file_path(FALSE)
  lapply(
    file_collection,
    function(file_definition) expect_true(isFALSE(file_definition$extra_col_file_path))
  )
  file_definition <- file_collection[[2]] %>%
    set_extra_col_file_path("xx")
  expect_identical(file_definition$extra_col_file_path, "xx")
  file_definition %<>%
    set_extra_col_file_path(FALSE)
  expect_true(isFALSE(file_definition$extra_col_file_path))
})

test_that("'set_n_max()' works", {
  file_collection <- mock_file_collection %>%
    set_n_max(Inf)
  lapply(
    file_collection,
    function(file_definition) expect_identical(file_definition$n_max, Inf)
  )
  file_collection %<>%
    set_n_max(3)
  lapply(
    file_collection,
    function(file_definition) expect_identical(file_definition$n_max, 3)
  )
  file_definition <- file_collection[[2]] %>%
    set_n_max(Inf)
  expect_identical(file_definition$n_max, Inf)
  file_definition %<>%
    set_n_max(3)
  expect_identical(file_definition$n_max, 3)
})

test_that("'set_adapters()' works", {
  file_collection <- mock_file_collection %>%
    set_adapters(new_adapters(asin))
  lapply(
    file_collection,
    function(file_definition) expect_identical(file_definition$adapters, new_adapters(asin))
  )
  file_definition <- file_collection[[2]] %>%
    set_adapters(new_adapters(acos))
  expect_identical(file_definition$adapters, new_adapters(acos))
})

