# Function -------------------------------------------------------------------

#' Read WITec TrueMatch files (ASCII/txt)
#'
#' Import spectra from WITec ASCII/txt files exported by WITec TrueMatch.
#'
#' Function [read_txt_WITec_TrueMatch()] reads WITec ASCII files exported by
#' WITec TrueMatch. These files are ini-like: ASCII files with meta data
#' sections and spectra data sections.
#'
#' @param file Path or connection to ASCII TrueMatch file.
#' @param keys_2header
#'        All meta data will be preserved if `key_2header = "all"` (default);
#'        only `spc`  file name will be preserved if `key_2header = "none"`;
#'        only specified header information will be saved otherwise
#'        e.g., `key_2header=c("key1", "key2", ...)`.
#'
#'
#' @return [hyperSpec][hyperSpec::hyperSpec-class()] object.
#'
#'
#' @author Claudia Beleites and Erick Oduniyi
#'
#' @include messages.R
#' @importFrom hySpc.testthat test<-
#'
#' @export
read_txt_WITec_TrueMatch <- function(file, keys_2header = "all") {

  # Get file
  filename <- file
  file <- read_ini(filename)

  # Get header information
  spc_hdr <- which(names(file) == "SpectrumHeader")

  # Calculate spectrum size (number of wavelengths)
  nwl <- sapply(file[spc_hdr], function(hdr) hdr$SpectrumSize)

  # Check if the number of wavelengths is the same for all the spectra
  if (!all(nwl == nwl[1])) {
    stop(
      "This file contains spectra with unequal length. \n",
      "This is not yet supported by 'read_txt_WITec()'. \n ",
      msg_open_issue_and_add_file()
    )
  }
  nwl <- nwl[1]

  # Create spectra
  spc <- matrix(NA_real_, nrow = length(spc_hdr), ncol = nwl)
  wl <- NA

  # Check if the SpectrumData is in the correct position
  # (should appear 2 header positions after the SpectrumHeader)
  if (!all(names(file[spc_hdr + 2]) == "SpectrumData")) {
    stop(
      "This file does not contain the SpectrumData at the expected positions.\n",
      "This is not yet supported by 'read_txt_WITec()'. \n ",
      msg_open_issue_and_add_file()
    )
  }

  # Parse SpectrumData
  for (s in seq_along(spc_hdr)) {
    data <- unlist(file[[spc_hdr[s] + 2]])
    data <- scan(text = data, quiet = TRUE)
    data <- matrix(data, nrow = nwl, ncol = 2L, byrow = TRUE)

    # Compare spectra
    if (s == 1) {
      wl <- data[, 1]
    } else
    if (!all(wl == data[, 1])) {
      stop("Spectrum ", s, " has different wavelength axis.")
    }

    spc[s, ] <- data[, 2]
  }

  # Create hyperSpec object
  spc <- new("hyperSpec", spc = spc, wavelength = wl)

  # Update labels
  # By default, the wavelength is assumed to be measured in lambda/nm
  # However, the user has the options to specify different units
  # at object creation
  if (file$SpectrumHeader$XDataKind == "nm") {
    labels(spc, ".wavelength") <-
      paste0("lambda/", file$SpectrumHeader$XDataKind)
  } else {
    choice <- tolower(
      readline(prompt = "Do you want to enter units for x-axis (wavelength)?: ")
    )
    if (choice == "yes" || choice == "y") {
      label <- readline(prompt = "Enter x-axis (wavelength) units: ")
      labels(spc, ".wavelength") <- label
    } else {
      labels(spc, ".wavelength") <- file$SpectrumHeader$XDataKind
    }
  }

  # Parse SpectrumHeader
  header <- file$SpectrumHeader
  header <- header[which(nzchar(names(header)))]
  for (d in seq_along(names(header))) {
    spc[, names(header)[d]] <- header[[d]]
  }

  # Parse SampleMetaData
  header_meta_data <- file$SampleMetaData
  header_meta_data <- header_meta_data[which(nzchar(names(header_meta_data)))]
  for (d in seq_along(names(header_meta_data))) {
    spc[, names(header_meta_data)[d]] <- header_meta_data[[d]]
  }

  # Collect the keys_2header information
  if ("all" %in% keys_2header) {
    .spc_io_postprocess_optional(spc, filename)
  } else if ("none" %in% keys_2header) {
    .spc_io_postprocess_optional(spc[, c("spc")], filename)
  } else if (!"all" %in% keys_2header && !"none" %in% keys_2header) {
    .spc_io_postprocess_optional(spc[, c("spc", keys_2header)], filename)
  } else {
    .spc_io_postprocess_optional(spc, filename)
  }
}

# Unit tests -----------------------------------------------------------------

hySpc.testthat::test(read_txt_WITec_TrueMatch) <- function() {
  context("read_txt_WITec_TrueMatch")

  local_edition(3)

  filename <- system.file(
    "extdata",
    "txt.Witec/Witec_TrueMatch.txt",
    package = "hySpc.read.txt"
  )

  expect_silent(spc <- read_txt_WITec_TrueMatch(filename))

  n_wl <- nwl(spc)
  n_rows <- nrow(spc)
  n_clos <- ncol(spc)

  test_that("WITec TrueMatch .txt: hyperSpec obj. dimensions are correct", {
    expect_equal(n_wl, 63)
    expect_equal(n_rows, 5)
    expect_equal(n_clos, 2)
  })

  test_that("WITec TrueMatch .txt: extra data are correct", {
    # @data colnames
    expect_equal(colnames(spc), c("spc", "filename"))
  })

  test_that("WITec TrueMatch .txt: labels are correct", {
    expect_equal(spc@label$.wavelength, NULL)
    expect_equal(spc@label$spc, NULL)
    expect_equal(spc@label$filename, "filename")
  })

  test_that("WITec TrueMatch .txt: spectra are correct", {
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

  test_that("WITec TrueMatch .txt: wavelengths are correct", {
    expect_equal(spc@wavelength[1], 161.40845)
    expect_equal(spc@wavelength[10], 200.18387)
    expect_equal(spc@wavelength[n_wl], 423.65106)
  })
}
