
<!-- README.md is generated from README.Rmd. Please edit that file -->

# readall

<!-- badges: start -->

<!-- badges: end -->

This package offers a single interface for reading:

  - **FWF files** (fixed width files): The data is stored in columns
    with a fixed character number (width) in a text file (ANSI or
    UTF-8).
  - **DSV files** (delimiter separated values): The data is stored in
    columns separated by a delimiter symbol (like a semicolon etc.) in a
    text file (ANSI or UTF-8).
  - **EXCEL files**: `*.xlsx` or `*.xlsm` files
  - **SAS data files**: `*.sas7bdat` or `*.sas7bcat` files

Furthermore, with `readall` you can define **file structure objects**,
which describe the structure of your data files. You can use this
objects to read all data files, which have this structure. You also can
add all available meta data to this file structure objects (E.g.: A
detailed description of the data columns and its value levels). When
reading the data files, this meta data will automatically be appended to
the columns of the resulting data.frame.

Beside that, you can also define a **collection of files** (can also be
a mixture of different file types). When reading this file collection
the resulting data sets will automatically be concatenated into a single
data.frame.

For many data sets, it is necessary to do some pre processing before
concatenating them together. For this purpose `readall` supplies the
possibility to define so called **adapter functions**, which will be
automatically applied to the read data sets before concatenating them.

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("a-maldet/readall", build_vignettes = TRUE)
```

## Usage

For a detailed description of the `readall` package and how to use it,
please read the vignette:

``` r
vignette("readall")
```

## License

GPL-3

Please feel free to post issues or fork the repo.
