## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
#library(readall)

## ----eval = FALSE-------------------------------------------------------------
#  devtools::install_github('a-maldet/readall', build_vignettes = TRUE)

## ----eval = FALSE-------------------------------------------------------------
#  g <- function(x) {
#    x[x$a > 1,]
#  }

## ----eval = FALSE-------------------------------------------------------------
#  structure_excel_1 <- new_file_structure_excel(
#    col_names = c("city", "adult"),
#    col_types = c("character", "logical"),
#    adapters = new_adapters(
#      function(x) {
#        names(x) <- c("CITY", "ADULT")
#        x
#      },
#      function(x) {
#        x <- x[!is.na(x$CITY),]
#        x$CITY <- trimws(x$CITY)
#        x
#      }
#    )
#  )

## ----eval = FALSE-------------------------------------------------------------
#  structure_excel_1 <- new_file_structure_excel(
#    col_names = c("city", "adult"),
#    col_types = c("character", "logical"),
#    meta_list = new_file_meta(
#      new_col_meta(
#        desc = "National and international city codes",
#        values = c("XXXX", "A", NA),
#        values_desc = c("4 digit city code", "abroad", "missing")
#      ),
#      new_col_meta(
#        desc = "Is the person an adult",
#        values = c(TRUE, FALSE, NA),
#        values_desc = c("The person is an adult", "The person is a child", "unknown")
#      )
#    )
#  )

## ----eval = FALSE-------------------------------------------------------------
#  structure_excel_1 <- new_file_structure_excel(
#    cols = list(
#      list(
#        name = "city",
#        type = "character",
#        new_col_meta(
#          desc = "National and international city codes",
#          values = c("XXXX", "A", NA),
#          values_desc = c("4 digit city code", "abroad", "missing")
#        )
#      ),
#      list(
#        name = "adult",
#        type = "logical",
#        new_col_meta(
#          desc = "Is the person an adult",
#          values = c(TRUE, FALSE, NA),
#          values_desc = c("The person is an adult", "The person is a child", "unknown")
#        )
#      )
#    )
#  )

## ----eval = FALSE-------------------------------------------------------------
#  df_meta <- get_meta(file_definition_1, cols = c("city", "adult"))

## ----eval = FALSE-------------------------------------------------------------
#  data <- read_data(file_definition_1)
#  df_meta <- get_meta(data, cols = c("city", "adult"))

## ----eval = FALSE-------------------------------------------------------------
#  structure_fwf_1 <- new_file_structure_fwf(
#    cols = list(
#      list(
#        type = "character",
#        name = "sex",
#        start = 1
#      ),
#      list(
#        type = "numeric",
#        name = "age",
#        start = 3
#      ),
#      list(
#        typ = "numeric",
#        name = "city",
#        start = 7
#  
#      )
#    ),
#    sep_width = 1,
#    adapters = new_adapters(
#      function(x) {
#        x$sex <- ifelse(x$sex == "m", "male", "female")
#        x
#      }
#    )
#  )

## ----eval = FALSE-------------------------------------------------------------
#  file_definition_file_1 <- new_file_definition(
#    file_path = "C:/file1.dat",
#    file_structure = structure_fwf_1,
#    extra_adapters = new_adapters(
#      function(x) {
#        x[x$age >= 30 && x$age < 40,]
#      }
#    )
#  )

## ----eval = FALSE-------------------------------------------------------------
#  file_collection_1 <- new_file_collection(
#    file_definition_file_1,
#    file_definition_file_2,
#    file_definition_file_3,
#    cols_keep = c("sex", "age"),
#    extra_adapters = new_adapters(
#      function(x) {
#        x[x$sex == "male",]
#      }
#    )
#    extra_col_file_path = "file"
#  )

## ----eval = TRUE, echo = FALSE------------------------------------------------
data.frame(
  sex = rep("male", 7),
  age = c(16, 67, 31, 84, 47, 33, 98),
  file = c("R:/daten/file1.dat", "R:/daten/file1.dat", "R:/daten/file2.sas", "R:/daten/file2.sas", "R:/daten/file3.excel", "R:/daten/file3.excel", "R:/daten/file3.excel")
)

## ----eval = FALSE-------------------------------------------------------------
#  df1 <- read_data(file_definition_file_1)

## ----eval = FALSE-------------------------------------------------------------
#  df2 <- read_data_fwf(
#    file_path = "R:/daten/file1.dat",
#    cols = list(
#      list(
#        type = "character",
#        name = "sex",
#        start = 1
#      ),
#      list(
#        type = "numeric",
#        name = "age",
#        start = 3
#      ),
#      list(
#        typ = "numeric",
#        name = "city",
#        start = 7
#  
#      )
#    ),
#    sep_width = 1,
#    adapters = new_adapters(
#      function(x) {
#        x$sex <- ifelse(x$sex == "m", "male", "female")
#        x
#      },
#      function(x) {
#        x[x$city >= 70000 && x$city < 80000,]
#      }
#    ),
#    cols_keep = c("sex", "age"),
#    extra_col_file_path = "file"
#  )

## ----eval = FALSE-------------------------------------------------------------
#  df_gesamt <- read_data(file_collection_1)

