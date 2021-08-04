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
#' @return `hyperSpec` object
#'
#' @author Claudia Beleites
#'
#' @seealso `vignette ("fileio")` for more information on file import and
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
  context("read_asc_Andor")
  andor <- system.file("extdata/asc.Andor/", "ASCII-Andor-Solis.asc",package = "hySpc.read.txt")
  spc <- read_asc_Andor(andor)

  test_that("Andor Solis .asc labels are correct",{
    expect_equal(spc@label$.wavelength, NULL)
    expect_equal(spc@label$spc, NULL)
    expect_equal(spc@label$filename, "filename")
  })

  test_that("Andor Solis .asc: spectra are correct", {
    expect_equal(dim(spc@data$spc), c(5,63))
    expect_equal(colnames(spc@data$spc),
                 c("161.408","165.729","170.046","174.361","178.672","182.981","187.285","191.588","195.887",
                   "200.184","204.477","208.767","213.054","217.339","221.62", "225.898","230.173","234.445",
                   "238.714","242.981","247.244","251.505","255.762","260.016","264.268","268.516","272.762",
                   "277.004","281.244","285.48", "289.714","293.945","298.172","302.397","306.619","310.838",
                   "315.054","319.267","323.477","327.685","331.889","336.091","340.289","344.484","348.677",
                   "352.867","357.054","361.238","365.419","369.596","373.772","377.945","382.114","386.281",
                   "390.444","394.605","398.763","402.919","407.071","411.22", "415.366","419.51", "423.651"))
  })

  test_that("Andor Solis .asc wavelength", {
    expect_equal(length(spc@wavelength), 63)

    expect_equal(round(spc@wavelength[47], 3), 357.054)
    expect_equal(round(spc@wavelength[23], 3), 255.762)
  })
}
