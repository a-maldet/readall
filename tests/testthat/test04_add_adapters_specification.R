test_that("'add_adapters()' works with file_structure", {
  mockup_struc <- mockup_file_structures[[2]]$struc
  struc <- do.call(
    new_file_structure_fwf,
    args = mockup_new_file_structure_fwf_args(
      mockup_struc,
      use_cols = T,
      use_col_pos = c("col_widths", "col_start"),
      adapters = new_adapters(sin)
    )
  ) %>%
    add_adapters(new_adapters(cos, tan, description = c("cos", "tan")))
  expect_class_objects_equal(
    struc$adapters,
    new_adapters(
      sin, cos, tan, description = c("Funktion 'sin'", "cos", "tan")
    )
  )
})
