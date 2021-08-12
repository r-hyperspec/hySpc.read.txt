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
#'
#' @return [hyperSpec][hyperSpec::hyperSpec-class()] object.
#'
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

hySpc.testthat::test(read_txt_Horiba) <- function() {
  context("read_txt_Horiba")

  local_edition(3)

  # read_txt_Horiba_t()  -----------------------------------------------------

  f_ts <- system.file(
    "extdata",
    "txt.HoribaJobinYvon/ts.txt",
    package = "hySpc.read.txt"
  )

  expect_silent(spc <- read_txt_Horiba_t(f_ts))

  n_wl <- nwl(spc)
  n_rows <- nrow(spc)
  n_clos <- ncol(spc)

  test_that("Horiba t .txt: hyperSpec obj. dimensions are correct", {
    expect_equal(n_wl, 1024)
    expect_equal(n_rows, 100)
    expect_equal(n_clos, 3)
  })

  test_that("Horiba t .txt: extra data are correct", {
    # @data colnames
    expect_equal(colnames(spc), c("t", "spc", "filename"))
  })

  test_that("Horiba t .txt: labels are correct", {
    expect_equal(spc@label$.wavelength, expression(Delta * tilde(nu) / cm^-1))
    expect_equal(spc@label$spc, expression("I / a.u."))
    expect_equal(spc@label$filename, "filename")
  })

  test_that("Horiba t .txt: spectra are correct", {
    # Dimensions of spectra matrix (@data$spc)
    expect_equal(dim(spc@data$spc), c(100, 1024))

    # Column names of spectra matrix
    expect_equal(colnames(spc@data$spc)[1], "2135.19")
    expect_equal(colnames(spc@data$spc)[10], "2118.25")
    expect_equal(colnames(spc@data$spc)[n_wl], "-122.41") # last name

    # Values of spectra matrix
    expect_equal(unname(spc@data$spc[1, 1]), 6244)
    expect_equal(unname(spc@data$spc[1, 10]), 5961)
    expect_equal(unname(spc@data$spc[n_rows, n_wl]), 117) # last spc value

    # Time series read correctly
    expect_equal(unname(spc@data$t)[1], "0")
    expect_equal(unname(spc@data$t)[10], "51.46")
    expect_equal(unname(spc@data$t)[n_rows], "566.766") # last time value

  })

  test_that("Horiba t .txt: wavelengths are correct", {
    expect_equal(spc@wavelength[1], 2135.1912)
    expect_equal(spc@wavelength[10], 2118.2483)
    expect_equal(spc@wavelength[n_wl], -122.41003)
  })

  # Cleanup
  rm(f_ts, spc, n_wl, n_rows, n_clos)


  # read_txt_Horiba_xy() -----------------------------------------------------

  f_map <- system.file(
    "extdata",
    "txt.HoribaJobinYvon/map.txt",
    package = "hySpc.read.txt"
  )
  expect_silent(spc <- read_txt_Horiba_xy(f_map))

  n_wl <- nwl(spc)
  n_rows <- nrow(spc)
  n_clos <- ncol(spc)

  test_that("Horiba xy .txt: hyperSpec obj. dimensions are correct", {
    expect_equal(n_wl, 616)
    expect_equal(n_rows, 141)
    expect_equal(n_clos, 4)
  })

  test_that("Horiba xy .txt: extra data are correct", {
    # @data colnames
    expect_equal(colnames(spc), c("x", "y", "spc", "filename"))

    # @data values
    # (Add tests, if relevant or remove this row)
  })

  test_that("Horiba xy .txt: labels are correct", {
    expect_equal(spc@label$.wavelength, expression(Delta * tilde(nu) / cm^-1))
    expect_equal(spc@label$spc, expression("I / a.u."))
    expect_equal(spc@label$filename, "filename")
  })

  test_that("Horiba xy .txt: spectra are correct", {
    # Dimensions of spectra matrix (@data$spc)
    expect_equal(dim(spc@data$spc), c(141, 616))

    # Column names of spectra matrix
    expect_equal(colnames(spc@data$spc)[1], "50")
    expect_equal(colnames(spc@data$spc)[10], "100.488")
    expect_equal(colnames(spc@data$spc)[n_wl], "3500") # last name

    # Values of spectra matrix
    expect_equal(unname(spc@data$spc[1, 1]), 3126.31)
    expect_equal(unname(spc@data$spc[1, 10]), 1958.41)
    expect_equal(unname(spc@data$spc[n_rows, n_wl]), 898.605) # last spc value
  })

  test_that("Horiba xy .txt: wavelengths are correct", {
    expect_equal(spc@wavelength[1], 50)
    expect_equal(spc@wavelength[10], 100.48781)
    expect_equal(spc@wavelength[n_wl], 3500)
  })
}
