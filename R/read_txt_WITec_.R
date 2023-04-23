
# Function -------------------------------------------------------------------

#' Read WITec files (ASCII/txt)
#'
#' Import Raman spectra/maps from WITec Instrument via ASCII files
#'
#' - [read_txt_WITec()] reads WITec ASCII files where the first column gives
#'   the wavelength axes and the other columns the spectra.
#'
#' - [read_dat_WITec()] reads WITec's ASCII exported data which comes in
#'   separate files with x and y data.
#'
#' @param file Path or connection to ASCII file.
#' @param points.per.line Number of spectra in x direction of the map.
#' @param lines.per.image Number of spectra in y direction.
#' @param type Type of spectra:
#' - `single` for single spectra (including time series);
#' - `map` for imaging data.
#' @param hdr.label WITec Project exports the spectra names (contain information
#'        of map position or number of spectra) within the `file`.
#' @param hdr.units WITec Project exports the spectra units within the `file`.
#' @param encoding character encoding, see [base::readLines()].
#' @param ...,quiet handed to [base::scan()].
#'
#'
#' @return [hyperSpec][hyperSpec::hyperSpec-class()] object.
#'
#'
#' @author Claudia Beleites and Marcel Dahms
#'
#'
#' @importFrom hySpc.testthat test<-
#' @importFrom utils head
#'
#' @export
#'
read_txt_WITec <- function(file = stop("filename or connection needed"),
                           points.per.line = NULL,
                           lines.per.image = NULL,
                           type = c("single", "map"),
                           hdr.label = FALSE,
                           hdr.units = FALSE,
                           encoding = "unknown",
                           ...,
                           quiet = TRUE) {
  ## check for valid data connection
  check_con(file = file)

  ## check for valid input
  type <- check_valid(type, hdr = NULL, points.per.line, lines.per.image)

  ## manage possible header lines by export function 'Table' in
  ## WITec Control/Project (version 4)
  skip <- hdr.label + hdr.units

  ## read spectra
  tmp <- readLines(file, encoding = encoding)
  nwl <- length(tmp) - skip
  txt <- scan(text = tmp, skip = skip, quiet = quiet, encoding = encoding, ...)

  dim(txt) <- c(length(txt) / nwl, nwl)

  hdr <- head(tmp, skip)

  ## fix: WITec/Andor may have final comma without values
  if (all(is.na(txt[nrow(txt), ]))) {
    txt <- txt[-nrow(txt), ]
  }

  spc <- new("hyperSpec", wavelength = txt[1, ], spc = txt[-1, ])

  ## add header information
  if (hdr.label | hdr.units) {
    spc <- parse_hdr(spc, hdr, hdr.label)
  }

  ## add map information
  if (type == "map") {
    spc <- parse_xy(spc, hdr, hdr.label, points.per.line, lines.per.image)
  }

  ## consistent file import behavior across import functions
  .spc_io_postprocess_optional(spc, file)
}


# Unit tests -----------------------------------------------------------------

hySpc.testthat::test(read_txt_WITec) <- function() {
  context("read_txt_WITec")

  local_edition(3)

  filename <- system.file(
    "extdata",
    "txt.Witec/Witec-timeseries_full.txt",
    package = "hySpc.read.txt"
  )

  expect_silent(spc <- read_txt_WITec(filename, hdr.units = TRUE, hdr.label = TRUE))

  n_wl <- nwl(spc)
  n_rows <- nrow(spc)
  n_clos <- ncol(spc)

  test_that("WITec .txt: hyperSpec obj. dimensions are correct", {
    expect_equal(n_wl, 1024)
    expect_equal(n_rows, 100)
    expect_equal(n_clos, 3)
  })

  test_that("WITec .txt: extra data are correct", {
    # @data colnames
    expect_equal(colnames(spc), c("spc", "spcname", "filename"))
  })

  test_that("WITec .txt: labels are correct", {
    expect_equal(spc@label$.wavelength, "rel. 1/cm")
    expect_equal(spc@label$spc, "CCD cts")
    expect_equal(spc@label$filename, "filename")
  })

  test_that("Andor Solis .asc: spectra are correct", {
    # Dimensions of spectra matrix (@data$spc)
    expect_equal(dim(spc@data$spc), c(100, 1024))

    # Column names of spectra matrix
    expect_equal(colnames(spc@data$spc)[1], "87.7367")
    expect_equal(colnames(spc@data$spc)[10], "117.333")
    expect_equal(colnames(spc@data$spc)[n_wl], "2821.31") # last name

    # Values of spectra matrix
    expect_equal(unname(spc@data$spc[1, 1]), 995)
    expect_equal(unname(spc@data$spc[2, 10]), 1002)
    expect_equal(unname(spc@data$spc[n_rows, n_wl]), 985) # last spc value
  })

  test_that("Andor Solis .asc: wavelengths are correct", {
    expect_equal(spc@wavelength[1], 87.7367)
    expect_equal(spc@wavelength[10], 117.333)
    expect_equal(spc@wavelength[n_wl], 2821.31)
  })
}
