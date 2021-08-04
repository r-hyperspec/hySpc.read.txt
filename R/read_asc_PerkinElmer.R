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
    "read_asc_PerkinElmer() is experimental, ",
    "'hyperSpec' so far has no test data for PE .asc files. ",
    "Please consider submitting your spectrum in an enhancement request to ",
    packageDescription("hyperSpec")$BugReports,
    " in order to help the development of hyperSpec."
  )

  ## find beginning of DATA section
  startDATA <- grep("DATA", content)

  if (length(startDATA) != 1L) {
    stop(
      "read_asc_PerkinElmer() so far can deal with single spectra files only.",
      " Please file an enhancement request at",
      packageDescription("hyperSpec")$BugReports,
      " with your file as an example or contact the maintainer (",
      maintainer("hyperSpec"), ")."
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

  path <- system.file("extdata/txt.PerkinElmer",package = "hySpc.read.txt")
  flu_file <- paste0(path, c("/flu1.txt", "/flu2.txt",
                             "/flu3.txt", "/flu4.txt", "/flu5.txt", "/flu6.txt"))

  test_that("PerkinElmer .txt: labels are correct", {
    for (flu in flu_file) {
      expect_message(spc <- read_asc_PerkinElmer(flu))

      expect_true(is.expression(spc@label$spc))
      expect_true(is.expression(spc@label$.wavelength))
      expect_equal(spc@label$filename, "filename")
    }
  })

  test_that("PerkinElmer .txt: spectra are correct", {
    for(flu in flu_file){
      expect_message(spc <- read_asc_PerkinElmer(flu))

      expect_equal(colnames(spc@data$spc), as.character(seq(from = 405, to = 495, by = .5)))
    }
  })

  test_that("PerkinElmer .txt wavelength",{
    for(flu in flu_file){
      expect_message(spc <- read_asc_PerkinElmer(flu))

      expect_equal(length(spc@wavelength), 181)
    }
  })
}
