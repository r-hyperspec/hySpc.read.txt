# Function -------------------------------------------------------------------

#' Import Andor Cameras/Solis files (ASCII/txt)
#'
#' Import Raman spectra/maps from Andor Cameras/Solis ASCII files.
#'
#' `read_asc_Andor()` reads Andor Solis ASCII (`.asc`) files where the first
#' column gives the wavelength  axes and the other columns the spectra.
#'
#' @param file filename or connection to ASCII file.
#' @param ...,quiet,dec,sep handed to [base::scan()].
#'
#' @return [hyperSpec][hyperSpec::hyperSpec-class()] object.
#'
#' @author Claudia Beleites
#'
#'
#' @concept io
#'
#' @export
#'
read_asc_Andor <- function(file = stop("filename or connection needed"),
                           ..., quiet = TRUE, dec = ".", sep = ",") {

  ## check for valid data connection
  check_con(file = file)

  ## read spectra
  tmp <- readLines(file)
  nwl <- length(tmp)
  txt <- scan(text = tmp, dec = dec, sep = sep, quiet = quiet, ...)

  dim(txt) <- c(length(txt) / nwl, nwl)

  ## fix: Andor Solis may have final comma without values
  if (all(is.na(txt[nrow(txt), ]))) {
    txt <- txt[-nrow(txt), ]
  }

  spc <- new("hyperSpec", wavelength = txt[1, ], spc = txt[-1, ])

  ## consistent file import behaviour across import functions
  .spc_io_postprocess_optional(spc, file)
}


# Unit tests -----------------------------------------------------------------

hySpc.testthat::test(read_asc_Andor) <- function() {
  local_edition(3)

  filename <- system.file(
    "extdata",
    "asc.Andor/ASCII-Andor-Solis.asc",
    package = "hySpc.read.txt"
  )

  expect_silent(spc <- read_asc_Andor(filename))

  n_wl <- nwl(spc)
  n_rows <- nrow(spc)
  n_clos <- ncol(spc)

  test_that("Andor Solis .asc: hyperSpec obj. dimensions are correct", {
    expect_equal(n_wl, 63)
    expect_equal(n_rows, 5)
    expect_equal(n_clos, 2)
  })

  test_that("Andor Solis .asc: extra data are correct", {
    # @data colnames
    expect_equal(colnames(spc), c("spc", "filename"))

    # @data values
    # (Add tests, if relevant or remove this row)
  })

  test_that("Andor Solis .asc: labels are correct", {
    expect_equal(spc@label$.wavelength, NULL)
    expect_equal(spc@label$spc, NULL)
    expect_equal(spc@label$filename, "filename")
  })

  test_that("Andor Solis .asc: spectra are correct", {
    # Dimensions of spectra matrix (@data$spc)
    expect_equal(dim(spc@data$spc), c(5, 63))

    # Column names of spectra matrix
    expect_equal(colnames(spc@data$spc)[1], "161.408")
    expect_equal(colnames(spc@data$spc)[10], "200.184")
    expect_equal(colnames(spc@data$spc)[n_wl], "423.651") # last name

    # Values of spectra matrix
    expect_equal(unname(spc@data$spc[1, 1]), 3404)
    expect_equal(unname(spc@data$spc[2, 10]), 3405)
    expect_equal(unname(spc@data$spc[n_rows, n_wl]), 3415) # last spc value
  })

  test_that("Andor Solis .asc: wavelengths are correct", {
    expect_equal(spc@wavelength[1], 161.40845)
    expect_equal(spc@wavelength[10], 200.18387)
    expect_equal(spc@wavelength[n_wl], 423.65106)
  })
}
