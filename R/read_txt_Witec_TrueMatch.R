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
#' @importFrom hySpc.testthat test<-
#'
#' @export
read_txt_WITec_TrueMatch <- function(file, keys_2header = "all") {

  # Get file
  filename <- file
  file <- read.ini(filename)

  # Get header information
  spc_hdr <- which(names(file) == "SpectrumHeader")

  # Calculate spectrum size (number of wavelengths)
  nwl <- sapply(file[spc_hdr], function(hdr) hdr$SpectrumSize)

  # Check if the number of wavelengths is the same for all the spectra
  if (!all(nwl == nwl[1])) {
    stop(
      "This file contains spectra with unequal length. \n",
      "This is not yet supported by 'read_txt_WITec_ASCII()'. \n ",
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
      "This is not yet supported by 'read_txt_WITec_ASCII()'. \n ",
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

  tmpdir <- paste0(tempdir(), "/test_Witec_txt_TrueMatch")
  untar("testfiles_Witec.tar.gz",
    files = c("Witec_TrueMatch.txt"),
    exdir = tmpdir
  )

  on.exit(unlink(tmpdir))

  test_that("WITec TrueMatch example file", {
    spc <- read_txt_WITec_TrueMatch(paste0(tmpdir, "/Witec_TrueMatch.txt"))

    expect_equal(
      dim(spc),
      c(nrow = 2L, ncol = length(colnames(spc)), nwl = 1024L)
    )
    expect_equal(spc$filename, rep(paste0(tmpdir, "/Witec_TrueMatch.txt"), 2))

    expect_equivalent(spc[[, , 610]], c(902, 732))
  })

  test_that("multiple spectra with varying wavelengths return error", {
    spc <- read_txt_WITec_TrueMatch(paste0(tmpdir, "/Witec_TrueMatch.txt"))

    expect_equivalent(
      length(spc@data[1, c("spc")]),
      length(spc@data[2, c("spc")])
    )
  })

  test_that("spectra are in correct positions", {
    spc <- read_txt_WITec_TrueMatch(paste0(tmpdir, "/Witec_TrueMatch.txt"))

    expect_equivalent(
      is.matrix(spc@data[1, c("spc")]),
      is.matrix(spc@data[2, c("spc")])
    )
  })

  test_that("spectra data is correctly parsed", {
    file <- hyperSpec::read.ini(paste0(tmpdir, "/Witec_TrueMatch.txt"))
    ini_spc <- file[which(names(file) == "SpectrumData")]
    spc <- read_txt_WITec_TrueMatch(paste0(tmpdir, "/Witec_TrueMatch.txt"))

    for (s in seq_along(ini_spc)) {
      data <- unlist(ini_spc[s])
      data <- scan(text = data, quiet = TRUE)
      expect_equivalent(data[c(TRUE, FALSE)], spc@wavelength) # wavelength
      expect_equivalent(data[c(FALSE, TRUE)], spc@data[s, c("spc")]) # intensity
    }
  })

  test_that("spectra meta data is correctly parsed", {
    file <- read.ini(paste0(tmpdir, "/Witec_TrueMatch.txt"))
    ini_meta <- file[which(names(file) == "SampleMetaData")]
    spc <- read_txt_WITec_TrueMatch(paste0(tmpdir, "/Witec_TrueMatch.txt"))
    A <- names(file$SampleMetaData)
    A <- A[A != ""]
    A <- intersect(names(spc@data), A)
    expect_equivalent(A, gsub("SampleMetaData.", "", names(unlist(ini_meta[1]))))
    expect_equivalent(A, gsub("SampleMetaData.", "", names(unlist(ini_meta[2]))))
  })

  test_that("spectra header is correctly parsed", {
    file <- read.ini(paste0(tmpdir, "/Witec_TrueMatch.txt"))
    ini_meta <- file[which(names(file) == "SpectrumHeader")]
    spc <- read_txt_WITec_TrueMatch(paste0(tmpdir, "/Witec_TrueMatch.txt"))
    A <- names(file$SpectrumHeader)
    A <- A[A != ""]
    A <- intersect(names(spc@data), A)
    expect_equivalent(A, gsub("SpectrumHeader.", "", names(unlist(ini_meta[1]))))
    expect_equivalent(A, gsub("SpectrumHeader.", "", names(unlist(ini_meta[2]))))
  })

  test_that("users can specify extra data to keep", {
    file <- read.ini(paste0(tmpdir, "/Witec_TrueMatch.txt"))

    A <- c(names(file$SpectrumHeader), names(file$SampleMetaData))
    A <- A[A != ""]
    spc <- read_txt_WITec_TrueMatch(
      paste0(tmpdir, "/Witec_TrueMatch.txt"),
      keys_2header = "all"
    )
    expect_equal(sort(colnames(spc)), sort(c("filename", "spc", A)))

    spc <- read_txt_WITec_TrueMatch(
      paste0(tmpdir, "/Witec_TrueMatch.txt"),
      keys_2header = "none"
    )
    expect_equivalent(sort(colnames(spc)), c("filename", "spc"))

    A <- c(names(file$SpectrumHeader), names(file$SampleMetaData))
    A <- A[A != ""]
    spc <- read_txt_WITec_TrueMatch(
      paste0(tmpdir, "/Witec_TrueMatch.txt"),
      keys_2header = c("Length")
    )
    expect_equivalent(
      sort(colnames(spc)),
      sort(c("filename", "spc", A[which(A == "Length")]))
    )
  })

  test_that("labels are correctly assigned to wavelength", {
    spc <- read_txt_WITec_TrueMatch(paste0(tmpdir, "/Witec_TrueMatch.txt"))
    expect_equivalent(labels(spc, ".wavelength"), "lambda/nm")
  })

  test_that("a valid hyperSpec object is returned", {
    spc_test <- read_txt_WITec_TrueMatch(paste0(tmpdir, "/Witec_TrueMatch.txt"))
    expect(
      assert_hyperSpec(spc_test),
      failure_message = "hyperSpec object was not returned"
    )
  })
}
