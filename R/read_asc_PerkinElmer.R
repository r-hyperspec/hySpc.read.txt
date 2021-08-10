# Function -------------------------------------------------------------------

#' Read single spectrum form PerkinElmer file (ASCII/txt)
#'
#' **EXPERIMENTAL FUNCTION.**\cr
#' Import a single spectrum from a file in PerkinElmer's ASCII format.
#' This function is experimental.
#'
#' @param file Path  or connection to file.
#' @param ... Further argument handed to [hyperSpec::read_txt_long()].
#'
#' @return `hyperSpec` object.
#'
#' @concept io
#'
#' @importFrom utils packageDescription
#' @export
#'
read_asc_PerkinElmer <- function(file = stop("filename or connection needed"),
                                 ...) {
  content <- readLines(con = file)

  message(
    "\nread_asc_PerkinElmer() is experimental. Package 'hySpc.read.txt' so far ",
    "has no test data for PerkinElmer .asc files. ",
    msg_open_issue_to_contribute_file()
  )

  ## find beginning of DATA section
  startDATA <- grep("DATA", content)

  if (length(startDATA) != 1L) {
    stop(
      "read_asc_PerkinElmer() so far can deal with single spectra files only. ",
      "It seems that your file contains more than one spectrum. ",
      msg_open_issue_to_contribute_file()
    )
  }

  ## Spectra values are stored
  content <- content[-seq_len(startDATA)]

  spc <- hyperSpec::read_txt_long(
    textConnection(content),
    header = FALSE,
    sep = "\t",
    ...
  )
  spc$filename <- NULL # not meaningful due to textConnection use

  ## consistent file import behaviour across import functions
  .spc_io_postprocess_optional(spc, file)
}


# Unit tests -----------------------------------------------------------------
hySpc.testthat::test(read_asc_PerkinElmer) <- function() {
  context("read_asc_PerkinElmer")

  local_edition(3)

  path <- system.file("extdata", "txt.PerkinElmer", package = "hySpc.read.txt")
  spc <- paste0(path, "/flu1.txt")
  expect_message(spc <- read_asc_PerkinElmer(spc))

  n_wl <- nwl(spc)
  n_rows <- nrow(spc)
  n_clos <- ncol(spc)

  test_that("PerkinElmer .asc: hyperSpec obj. dimensions are correct", {
    expect_equal(n_wl, 181)
    expect_equal(n_rows, 1)
    expect_equal(n_clos, 2)

  })

  test_that("PerkinElmer .asc: extra data are correct", {
    # @data colnames
    expect_equal(colnames(spc), c("spc", "filename"))
  })

  test_that("PerkinElmer .asc: labels are correct", {
    expect_equal(spc@label$.wavelength, expression(lambda/nm))
    expect_equal(spc@label$spc, expression("I / a.u."))
    expect_equal(spc@label$filename, "filename")
  })

  test_that("PerkinElmer .asc: spectra are correct", {
    # Dimensions of spectra matrix (@data$spc)
    expect_equal(dim(spc@data$spc), c(1, 181))

    # Column names of spectra matrix
    expect_equal(colnames(spc@data$spc)[1], "405")
    expect_equal(colnames(spc@data$spc)[10], "409.5")
    expect_equal(colnames(spc@data$spc)[n_wl], "495") # last name

    # Values of spectra matrix
    expect_equal(unname(spc@data$spc[1, 1]), 27.15)
    expect_equal(unname(spc@data$spc[1, 10]), 41.381333)
    expect_equal(unname(spc@data$spc[n_rows, n_wl]), 45.256333) # last spc value

  })

  test_that("PerkinElmer .asc: wavelengths are correct", {
    expect_equal(spc@wavelength[1], 405)
    expect_equal(spc@wavelength[10], 409.5)
    expect_equal(spc@wavelength[n_wl], 495)
  })
}
