#' Read Witec ASCII/txt files exported by Witec TrueMatch
#'
#' `read_txt_Witec_TrueMatch()` reads Witec ASCII files exported by Witec
#' TrueMatch. These files are ini-like: ASCII files with meta data sections and
#' spectra data sections.
#'TODO: implement keys2header param
#'TODO: order additional data columns
#'TODO: prepare labels of spc
#'TODO: create hyperSpec object after parsing header information
#'
#' @param file file name or connection to file
#'
#' @return hyperSpec object
#' @export
read_txt_Witec_TrueMatch <- function(file) {

  # Get file
  filename <- file
  file <- read.ini(filename)

  # Get header information
  i_spectra <- which(names(file) == "SpectrumHeader")

  # Calculate spectrum size
  nwl <- sapply(file[i_spectra], function(hdr) hdr$SpectrumSize)

  # Check if the number of wavelengths is the same for all the spectra
  if (!all(nwl == nwl[1])) {
    stop("This file contains spectra with unequal length.\n",
         "This is not yet supported by read_txt_Witec_ASCII, ",
         "please report an issue at:\n",
         packageDescription("hySpc.read.Witec")$BugReport,
         " including\n",
         "- the output of `sessionInfo()` and\n",
         "- an example file.")
  }

  # Otherwise, set the first wavelength to the first wavelength
  nwl <- nwl[1]

  # Create spectra
  spc <- matrix(NA_real_, nrow = length(i_spectra), ncol = nwl)
  wl <- NA

  # Check if the SpectrumData is in the correct position (should appear 2 header positions after the SpectrumHeader)
  if (!all(names(file[i_spectra + 2]) == "SpectrumData")) {
    stop("This file does not contain the SpectrumData at the expected positions,\n",
         "please report an issue at:\n",
         packageDescription("hySpc.read.Witec")$BugReport,
         " including\n",
         "- the output of `sessionInfo()` and\n",
         "- an example file.")
  }

  # Parse SpectrumData
  for (s in seq_along(i_spectra)) {
    data <- unlist(file[[i_spectra[s] + 2]])
    data <- scan(text = data, quiet = TRUE)
    data <- matrix(data, nrow = nwl, ncol = 2L, byrow = TRUE)

    #
    if (s == 1)
      wl <- data[, 1]
    else
      if (!all(wl == data[,1]))
        stop("Spectrum ", s, " has different wavelength axis.")

    spc[s,] <- data[, 2]
  }

  # Create hyperSpec object
  spc <- new("hyperSpec", spc = spc, wavelength = wl)

  # Parse SampleMetaData
  header_meta_data <- file$SampleMetaData
  header_meta_data <- header_meta_data[which(nzchar(names(header_meta_data)))]
  for (d in seq_along(names(header_meta_data))) {
    spc[, names(header_meta_data)[d]] <- header_meta_data[[d]]
  }

  # Parse SpectrumHeader
  header <- file$SpectrumHeader
  header <- header[which(nzchar(names(header)))]
  for(d in seq_along(names(header))) {
    spc[, names(header)[d]] <- header[[d]]
  }

  # Return hyperSpec object
  .fileio.optional(spc, filename)
}

test(read_txt_Witec_TrueMatch) <- function() {
  context("read_txt_Witec_TrueMatch")

  test_that("Witec TrueMatch example file", {
    spc <- read_txt_Witec_TrueMatch("Witec_TrueMatch.txt")

    expect_equal(dim(spc), c(nrow = 2L, ncol = length(colnames(spc)), nwl = 1024L))
    expect_equal(spc$filename, rep("Witec_TrueMatch.txt", 2))

    expect_equivalent(spc [[,, 610]], c(902, 732))
  })

  test_that("multiple spectra with varying wavelengths return error", {
    spc <- read_txt_Witec_TrueMatch("Witec_TrueMatch.txt")

    expect_equivalent(length(spc@data[1, c("spc")]),length(spc@data[2, c("spc")]))
  })

  test_that("spectra are in correct positions", {
    spc <- read_txt_Witec_TrueMatch("Witec_TrueMatch.txt")

    expect_equivalent(is.matrix(spc@data[1, c("spc")]), is.matrix(spc@data[2, c("spc")]))
  })

  test_that("spectra data is correctly parsed", {
    file <- hyperSpec::read.ini("Witec_TrueMatch.txt")
    ini_spc <- file[which(names(file) == "SpectrumData")]
    spc <- read_txt_Witec_TrueMatch("Witec_TrueMatch.txt")

    for (s in seq_along(ini_spc)) {
      data <- unlist(ini_spc[s])
      data <- scan(text = data, quiet = TRUE)
      expect_equivalent(data[c(TRUE, FALSE)], spc@wavelength) # wavelength
      expect_equivalent(data[c(FALSE, TRUE)], spc@data[s, c("spc")]) # intensity
    }
  })

  test_that("spectra meta data is correctly parsed", {
    file <- read.ini("Witec_TrueMatch.txt")
    ini_meta <- file[which(names(file) == "SampleMetaData")]
    spc <- read_txt_Witec_TrueMatch("Witec_TrueMatch.txt")
    A <- names(file$SampleMetaData)
    A <- A[A != ""]
    A <- intersect(names(spc@data), A)
    expect_equivalent(A, gsub("SampleMetaData.", "", names(unlist(ini_meta[1]))))
    expect_equivalent(A, gsub("SampleMetaData.", "", names(unlist(ini_meta[2]))))
  })

  test_that("spectra header is correctly parsed", {
    file <- read.ini("Witec_TrueMatch.txt")
    ini_meta <- file[which(names(file) == "SpectrumHeader")]
    spc <- read_txt_Witec_TrueMatch("Witec_TrueMatch.txt")
    A <- names(file$SpectrumHeader)
    A <- A[A != ""]
    A <- intersect(names(spc@data), A)
    expect_equivalent(A, gsub("SpectrumHeader.", "", names(unlist(ini_meta[1]))))
    expect_equivalent(A, gsub("SpectrumHeader.", "", names(unlist(ini_meta[2]))))
  })

  test_that("a valid hyperSpec object is returned", {
    spc <- read_txt_Witec_TrueMatch("Witec_TrueMatch.txt")
    expect(chk.hy(spc))
  })

}
