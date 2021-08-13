# Function -------------------------------------------------------------------

#' Read PerkinElmer file (ASCII/txt)
#'
#' **EXPERIMENTAL FUNCTION**.
#' To read example files correctly, pay attention to `skip = 54` in the
#' examples.
#'
#' @param file Path to file (or several files).
#' @param ... Passed to [base::scan()].
#' @param label Labels.
#'
#'
#' @return [hyperSpec][hyperSpec::hyperSpec-class()] object.
#'
#'
#' @importFrom utils modifyList
#'
#' @noRd
#'
#' @examples
#' path <- system.file("extdata", "txt.PerkinElmer", package = "hySpc.read.txt")
#' f_flu <- Sys.glob(paste0(path, "/flu?.txt"))
#'
#' spc_flu <- read_txt_PerkinElmer(f_flu, skip = 54)
#' plot(spc_flu)

read_txt_PerkinElmer <- function(file = stop("filenames needed"),
                                 ...,
                                 label = list()) {


  ##  set default labels
  label <- modifyList(
    list(
      .wavelength = expression(lambda / nm),
      spc = expression(I / a.u.)
    ),
    label
  )

  if (length(file) == 0) {
    warning("No files found.")
    return(new("hyperSpec"))
  }

  ## read the first file
  buffer <- matrix(scan(file[1], ..., quiet = TRUE), ncol = 2, byrow = TRUE)

  ## first column gives the wavelength vector
  wavelength <- buffer[, 1]

  ## preallocate the spectra matrix:
  ##  one row per file x as many columns as the first file has
  spc <- matrix(ncol = nrow(buffer), nrow = length(file))

  ## the first file's data goes into the first row
  spc[1, ] <- buffer[, 2]

  ## now read the remaining files
  for (f in seq(along = file)[-1]) {
    buffer <- matrix(scan(file[f], ...), ncol = 2, byrow = TRUE)

    ## check whether they have the same wavelength axis
    if (!all.equal(buffer[, 1], wavelength)) {
      stop(paste(file[f], "has different wavelength axis."))
    }

    spc[f, ] <- buffer[, 2]
  }

  ## make the hyperSpec object
  spc <- new("hyperSpec", wavelength = wavelength, spc = spc, label = label)

  ## consistent file import behavior across import functions
  .spc_io_postprocess_optional(spc, file)
}


# Unit tests -----------------------------------------------------------------
hySpc.testthat::test(read_txt_PerkinElmer) <- function() {
  context("read_txt_PerkinElmer")

  local_edition(3)

  path <- system.file("extdata", "txt.PerkinElmer", package = "hySpc.read.txt")
  f_spc <- paste0(path, "/flu1.txt")
  expect_silent(spc <- read_txt_PerkinElmer(f_spc, skip = 54))

  n_wl <- nwl(spc)
  n_rows <- nrow(spc)
  n_clos <- ncol(spc)

  test_that("PerkinElmer .txt: hyperSpec obj. dimensions are correct", {
    expect_equal(n_wl, 181)
    expect_equal(n_rows, 1)
    expect_equal(n_clos, 2)

  })

  test_that("PerkinElmer .txt: extra data are correct", {
    # @data colnames
    expect_equal(colnames(spc), c("spc", "filename"))
  })

  test_that("PerkinElmer .txt: labels are correct", {
    expect_equal(spc@label$.wavelength, expression(lambda/nm))
    expect_equal(spc@label$spc, expression(I / a.u.))
    expect_equal(spc@label$filename, "filename")
  })

  test_that("PerkinElmer .txt: spectra are correct", {
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

  test_that("PerkinElmer .txt: wavelengths are correct", {
    expect_equal(spc@wavelength[1], 405)
    expect_equal(spc@wavelength[10], 409.5)
    expect_equal(spc@wavelength[n_wl], 495)
  })
}
