add_fwf_col_end_widths <- function(struc) {
  sep_width <- struc$sep_width
  if (is.null(sep_width))
    sep_width <- 0
  struc$col_end <- c(struc$col_start[2:length(struc$col_start)] - 1 - sep_width, NA)
  struc$col_widths <- struc$col_end - struc$col_start + 1
  struc
}

#' create a list of mockup file_structures
#'
mockup_file_structures <- list(
  list(
    file_type = enum_file_types$fwf,
    file = "fwf1_default.txt",
    struc = list(
      col_names = c("x1", "x2", "x3", "x4", "x5"),
      col_types = c("numeric", "character", "logical", "NULL", "integer"),
      col_start = c(1, 10, 17, 24, 31),
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
      sep_width = 0
    ) %>% add_fwf_col_end_widths,
    data = data.frame(
      x1 = c(1000.1, 2.2, -3),
      x2 = c("    ä1 ", NA, "ä3     "),
      x3 = c(T, NA, F),
      x5 = c(1000L, NA, -3L)
    )
  ),
  list(
    file_type = enum_file_types$fwf,
    file = "fwf2_utf8_sep1_decbig_na.txt",
    struc = list(
      col_names = c("b1", "b2", "b3", "b4", "b5"),
      col_types = c("numeric", "character", "logical", "NULL", "integer"),
      col_start = c(1, 10, 15, 23, 31),
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
      na = "na",
      decimal_mark = ",",
      big_mark = ".",
      skip_rows = 1,
      encoding = "latin1"
    ) %>% add_fwf_col_end_widths,
    data = data.frame(
      b1 = c(1000.1, NA, -3, NA),
      b2 = c("  ä1", NA, "ä3  ", NA),
      b3 = c(T, F, NA, F),
      b5 = c(1000L, -2L, NA, NA)
    )
  ),
  list(
    file_type = enum_file_types$dsv,
    file = "dsv1_default.txt",
    struc = list(
      col_types = c("numeric", "character", "logical", "NULL", "integer"),
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
      )
    ),
    data = data.frame(
      a1 = c(1000.1, 2.2, -3),
      a2 = c("    ä1 ", NA, "    ä3 "),
      a3 = c(T, NA, F),
      a5 = c(1000L, NA, -3L)
    )
  ),
  list(
    file_type = enum_file_types$dsv,
    file = "dsv2_utf8_sep_decbig_na.txt",
    struc = list(
      col_names = c("b1", "b2", "b3", "b4", "b5"),
      col_types = c("numeric", "character", "logical", "NULL", "integer"),
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
      na = "na",
      decimal_mark = ",",
      big_mark = ".",
      header = F,
      encoding = "UTF-8"
    ),
    data = data.frame(
      b1 = c(1000.1, NA, -3, NA),
      b2 = c("  ä1", NA, "ä3  ", NA),
      b3 = c(T, F, NA, F),
      b5 = c(1000L, -2L, NA, NA)
    )
  ),
  list(
    file_type = enum_file_types$excel,
    file = "excel1_default.xlsx",
    struc = list(
      col_types = c("numeric", "character", "logical", "NULL", "integer"),
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
      )
    ),
    data = data.frame(
      a1 = c(1000.1, 2.2, -3),
      a2 = c("    ä1", NA, "    ä3 "),
      a3 = c(T, NA, F),
      a5 = c(1000L, NA, -3L)
    )
  ),
  list(
    file_type = enum_file_types$excel,
    file = "excel2_utf8_sheet_range_na.xlsx",
    struc = list(
      sheet = "Tabelle2",
      range = "B2:F6",
      col_names = c("b1", "b2", "b3", "b4", "b5"),
      col_types = c("numeric", "character", "logical", "NULL", "integer"),
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
      header = F,
      na = "na"
    ),
    data = data.frame(
      b1 = c(1000.1, NA, -3, NA, NA),
      b2 = c("  ä1", NA, "ä3", NA, NA),
      b3 = c(T, F, NA, F, NA),
      b5 = c(1000L, -2L, NA, NA, NA)
    )
  ),
  list(
    file_type = enum_file_types$sas,
    file = "sas1.sas7bdat",
    struc = list(
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
      )
    ),
    data = data.frame(
      a1 = c(1000.1, 2.2, -3),
      a2 = c("    ä1", NA, "    ä3"),
      a3 = c(T, NA, F),
      a4 = c(" skip", "skip", "  skip"),
      a5 = c(1000L, NA, -3L)
    )
  ),
  list(
    file_type = enum_file_types$sas,
    file = "sas2.sas7bdat",
    struc = list(
      skip_rows = 2,
      n_max = 6,
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
      )
    ),
    data = data.frame(
      a1 = c(-3, NA, 1000.1, NA, -3, NA),
      a2 = c("ä3", NA, "  ä1", NA, "ä3", NA),
      a3 = c(NA, F, T, F, NA, F),
      a4 = c("  skip", "  skip", " skip",  "  skip", "  skip", "  skip"),
      a5 = c(NA, NA, 1000L, -2L, NA, NA)
    )
  )
) %>%
  lapply(
    function(x) {
      x$file_path <- system.file(
        "extdata",
        file.path("testdata", x$file),
        package="readall"
      )
      x
    }
  )

mockup_new_file_structure_fwf_args <- function(
  struc,
  use_cols = T,
  use_col_pos = c("sep_width", "col_start"),
  ...
) {
  add_args <- list(...)
  args <- struc
  if (use_cols) {
    args$cols <- lapply(
      seq_len(length(args$col_types)),
      function(i) {
        list(
          name = if (!is.null(args$col_names)) args$col_names[i] else NULL,
          type = args$col_types[i],
          start = if ("col_start" %in% use_col_pos) args$col_start[i] else NULL,
          end = if ("col_end" %in% use_col_pos) args$col_end[i] else NULL,
          width = if ("col_widths" %in% use_col_pos) args$col_widths[i] else NULL,
          meta = if (!is.null(args$file_meta)) args$file_meta[[i]] else NULL
        ) %>% remove_null_entries
      }
    )
    args$col_names <- NULL
    args$col_types <- NULL
    args$col_start <- NULL
    args$col_end <- NULL
    args$col_widths <- NULL
    args$file_meta <- NULL
  }
  lapply(
    setdiff(c("col_start", "col_end", "col_widths", "sep_width"), use_col_pos),
    function(var) args[[var]] <<- NULL
  ) %>% invisible
  lapply(
    names(add_args),
    function(var) args[[var]] <<- add_args[[var]]
  ) %>% invisible
  args %<>% remove_null_entries
  args
}

mockup_new_file_structure_dsv_args <- function(
  struc,
  use_cols = T,
  ...
) {
  add_args <- list(...)
  args <- struc
  if (use_cols) {
    args$cols <- lapply(
      seq_len(length(args$col_types)),
      function(i) {
        list(
          name = if (!is.null(args$col_names)) args$col_names[i] else NULL,
          type = args$col_types[i],
          meta = if (!is.null(args$file_meta)) args$file_meta[[i]] else NULL
        ) %>% remove_null_entries
      }
    )
    args$col_names <- NULL
    args$col_types <- NULL
    args$file_meta <- NULL
  }
  lapply(
    names(add_args),
    function(var) args[[var]] <<- add_args[[var]]
  ) %>% invisible
  args %<>% remove_null_entries
  args
}

mockup_new_file_structure_excel_args <- function(
  struc,
  use_cols = T,
  ...
) {
  mockup_new_file_structure_dsv_args(
    struc = struc,
    use_cols = use_cols,
    ...
  )
}

mockup_new_file_structure_sas_args <- function(
  struc,
  ...
) {
  mockup_new_file_structure_dsv_args(
    struc = struc,
    use_cols = FALSE,
    ...
  )
}

add_attr <- function(x, var, val) {
  if (!is.null(x[[var]]))
    x[[var]] <- val
  x
}

mockup_compare_struc_fwf <- function(mockup, struc) {
  obj_comp <- mockup$struc %<>%
    add_attr("skip_rows", 0) %>%
    add_attr("na", "") %>%
    add_attr("decimal_mark", ".") %>%
    add_attr("big_mark", ",") %>%
    add_attr("trim_ws", TRUE) %>%
    add_attr("n_max", Inf) %>%
    add_attr("encoding", "latin1") %>%
    
    structure(class = c("file_structure_fwf", "file_structure"))
  expect_equal()
}
