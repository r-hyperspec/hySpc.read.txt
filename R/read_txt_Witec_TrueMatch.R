#' Read Witec ASCII/txt files exported by Witec TrueMatch
#'
#' `read_txt_Witec_TrueMatch()` reads Witec ASCII files exported by Witec
#' TrueMatch. These files are ini-like ASCII files with meta data sections and
#' spectra data sections.
#'
#' TODO: import also meta data
#'
#' @param file file name or connection to file
#'
#' @return hyperSpec object
#' @export
read_txt_Witec_TrueMatch <- function(file) {

  file <- read.ini(file)

  i_spectra <- which(names(file) == "SpectrumHeader")

  nwl <- sapply(file[i_spectra], function(hdr) hdr$SpectrumSize)

  if (!all(nwl == nwl[1]))
    stop("This file contains spectra with unequal length.\n",
         "This is not yet supported by read_txt_Witec_ASCII, ",
         "please report an issue at:\n",
         packageDescription("hySpc.read.Witec")$BugReport,
         " including\n",
         "- the output of `sessionInfo()` and\n",
         "- an example file.")
  nwl <- nwl[1]

  spc <- matrix(NA_real_, nrow = length(i_spectra), ncol = nwl)
  wl <- NA

  #
  if (!all(names(file[i_spectra + 2]) == "SpectrumData"))
    stop("This file does not contain the SpectrumData at the expected positions,\n",
          "please report an issue at:\n",
          packageDescription("hySpc.read.Witec")$BugReport,
          " including\n",
          "- the output of `sessionInfo()` and\n",
          "- an example file.")
  #
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

  #
  spc <- new("hyperSpec", spc = spc, wavelength = wl)

  #
  .fileio.optional(spc)
}

test(read_txt_Witec_TrueMatch) <- function() {
  context("read_txt_Witec_TrueMatch")

  test_that("Witec TrueMatch example file", {
    spc <- read_txt_Witec_TrueMatch("Witec_TrueMatch.txt")
  })
}
