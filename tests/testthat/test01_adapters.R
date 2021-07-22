test_that("'new_adapters()' works without 'description'", {
  adapters <- new_adapters(sin, cos)
  expect_class_objects_equal(
    adapters,
    structure(
      list(
        list(
          fun = sin,
          desc = "Funktion 'sin'"
        ),
        list(
          fun = cos,
          desc = "Funktion 'cos'"
        )
      ),
      class = "adapters"
    )
  )
})
test_that("'new_adapters()' works with 'description'", {
  adapters <- new_adapters(sin, cos, description = c("sin", "cos"))
  expect_class_objects_equal(
    adapters,
    structure(
      list(
        list(
          fun = sin,
          desc = "sin"
        ),
        list(
          fun = cos,
          desc = "cos"
        )
      ),
      class = "adapters"
    )
  )
})


test_that("'new_adapters()' can produce empty lists", {
  adapters <- new_adapters()
  expect_class_objects_equal(
    adapters,
    structure(
      list(),
      class = "adapters"
    )
  )
})


test_that("'add_adapters()' works for 'adapters' class objects", {
  adapters <- new_adapters(sin, cos, description = c("sin", "cos")) %>%
    add_adapters(new_adapters(tan, atan, description = c("tan", "atan")))
  expect_class_objects_equal(
    adapters,
    structure(
      list(
        list(
          fun = sin,
          desc = "sin"
        ),
        list(
          fun = cos,
          desc = "cos"
        ),
        list(
          fun = tan,
          desc = "tan"
        ),
        list(
          fun = atan,
          desc = "atan"
        )
      ),
      class = "adapters"
    )
  )
})

test_that("'add_adapters()' works for empty 'adapters' class objects", {
  adapters <- new_adapters() %>%
    add_adapters(new_adapters(tan, atan, description = c("tan", "atan")))
  expect_class_objects_equal(
    adapters,
    structure(
      list(
        list(
          fun = tan,
          desc = "tan"
        ),
        list(
          fun = atan,
          desc = "atan"
        )
      ),
      class = "adapters"
    )
  )
  adapters %<>%
    add_adapters(new_adapters())
  expect_class_objects_equal(
    adapters,
    structure(
      list(
        list(
          fun = tan,
          desc = "tan"
        ),
        list(
          fun = atan,
          desc = "atan"
        )
      ),
      class = "adapters"
    )
  )
})
