# Function -------------------------------------------------------------------

#' @rdname read_txt_WITec
#' @param filex filename wavelength axis file
#' @param filey filename intensity file
#' @export
read_dat_WITec <- function(filex = stop("filename or connection needed"),
                           filey = sub("-x", "-y", filex),
                           points.per.line = NULL,
                           lines.per.image = NULL,
                           type = c("single", "map"),
                           encoding = "unknown",
                           ...,
                           quiet = hy_get_option("debuglevel") < 1L) {
  ## check valid data connection
  check_con(filex = filex, filey = filey)

  ## check valid input
  type <- check_valid(
    type = type, points.per.line = points.per.line,
    lines.per.image = lines.per.image
  )

  ## read data
  wl <- scan(file = filex, ..., quiet = quiet, encoding = encoding)
  spc <- scan(file = filey, ..., quiet = quiet, encoding = encoding)

  dim(spc) <- c(length(wl), length(spc) / length(wl))

  spc <- new("hyperSpec", wavelength = wl, spc = t(spc))

  ## add map information
  if (type == "map") {
    spc <- parse_xy(spc = spc, points.per.line = points.per.line, lines.per.image = lines.per.image)
  }

  ## consistent file import behaviour across import functions
  .spc_io_postprocess_optional(spc, filey)
}


# Unit tests -----------------------------------------------------------------

hySpc.testthat::test(read_dat_WITec) <- function() {
  context("read_dat_WITec")

  local_edition(3)

  filename <- system.file(
    "extdata",
    "txt.Witec/Witec-timeseries-x.dat",
    package = "hySpc.read.txt"
  )

  expect_silent(spc <- read_dat_WITec(filename))

  n_wl <- nwl(spc)
  n_rows <- nrow(spc)
  n_clos <- ncol(spc)

  test_that("WITec .dat: hyperSpec obj. dimensions are correct", {
    expect_equal(n_wl, 1024)
    expect_equal(n_rows, 100)
    expect_equal(n_clos, 2)
  })

  test_that("WITec .dat: extra data are correct", {
    # @data colnames
    expect_equal(colnames(spc), c("spc", "filename"))
  })

  test_that("WITec .dat: labels are correct", {
    expect_equal(spc@label$.wavelength, NULL)
    expect_equal(spc@label$spc, NULL)
    expect_equal(spc@label$filename, "filename")
  })

  test_that("WITec .dat: spectra are correct", {
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

  test_that("WITec .dat: wavelengths are correct", {
    expect_equal(spc@wavelength[1], 87.736672)
    expect_equal(spc@wavelength[10], 117.332683)
    expect_equal(spc@wavelength[n_wl], 2821.3062)
  })
}
