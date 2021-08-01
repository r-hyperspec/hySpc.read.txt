# Function -------------------------------------------------------------------

#' Read Horiba Labspec files (ASCII/txt)
#'
#' Read ASCII (`.txt`) files exported by Horiba's Labspec software
#' (LabRAM spectrometers).
#'
#' - `read_txt_Horiba()`
#' - `read_txt_Horiba_xy()` reads maps, i.e. `.txt` files where the first two
#'    columns give x and y coordinates.
#' - `read_txt_Horiba_t()` reads time series, i.e. `.txt` files with the time
#'    in the first column.
#'
#' @rdname read_txt_Horiba
#'
#' @param file Path of connection to `.txt` file.
#' @param cols,header,sep,row.names,check.names,... further parameters are
#'        handed over to [hyperSpec::read_txt_wide()]
#'
#' @return `hyperSpec` object.
#'
#' @author C. Beleites
#'
#' @concept io
#'
#' @export
#'
read_txt_Horiba <- function(file,
                            cols = c(
                              spc = "I / a.u.",
                              .wavelength = expression(Delta * tilde(nu) / cm^-1)
                            ),
                            header = TRUE,
                            sep = "\t",
                            row.names = NULL,
                            check.names = FALSE,
                            ...) {
  spc <- read_txt_wide(file,
    cols = cols,
    header = header, sep = sep, row.names = row.names,
    check.names = check.names, ...
  )

  ## consistent file import behaviour across import functions
  ## is already provided by read_txt_wide

  spc
}

# Function -------------------------------------------------------------------

#' @rdname read_txt_Horiba
#' @export

read_txt_Horiba_xy <- function(file, ...) {
  read_txt_Horiba(
    file = file,
    cols = c(
      x = expression(x / mu * m),
      y = expression(y / mu * m),
      spc = "I / a.u.",
      .wavelength = expression(Delta * tilde(nu) / cm^-1)
    ),
    ...
  )
}

# Function -------------------------------------------------------------------

#' @rdname read_txt_Horiba
#' @export


read_txt_Horiba_t <- function(file, header = TRUE, sep = "\t", row.names = NULL,
                              check.names = FALSE, ...) {
  read_txt_Horiba(
    file,
    cols = c(
      t = "t / s",
      spc = "I / a.u.",
      .wavelength = expression(Delta * tilde(nu) / cm^-1)
    ),
    ...
  )
}

# Unit tests -----------------------------------------------------------------

# FIXME: add unit tests

hySpc.testthat::test(read_txt_Horiba_t) <- function() {
  context("read_txt_Horiba_t")

  ts <- system.file("extdata/fileio/txt.HoribaJobinYvon/", "ts.txt",package = "hySpc.read.txt")
  spc <- read_txt_Horiba_t(ts)

  test_that("Horiba time series .txt labels are correct",{
    expect_true(is.expression(spc@label$.wavelength))
    expect_true(is.expression(spc@label$spc))
    expect_equal(spc@label$filename, "filename")
  })

  test_that("Horiba time series .txt spectral data", {
    expect_equal(dim(spc@data$spc), c(100,1024))

    expect_equal(colnames(spc@data$spc)[[974]], "5.9526")
    expect_equal(colnames(spc@data$spc)[[657]], "772.27")
  })

  test_that("Horiba time series .txt wavelength", {
    expect_equal(length(spc@wavelength), 1024)

    expect_equal(round(spc@wavelength[[79]], 3), 1986.863)
    expect_equal(round(spc@wavelength[[1011]], 3), -88.828)
  })
}

