# Function -------------------------------------------------------------------

#' Import Renishaw files (ASCII/txt)
#'
#' Import Raman measurements from Renishaw (possibly compressed) `.txt` file.
#'
#' The file may be of any file type that can be read by
#' [base::gzfile()] (i.e. text, or zipped by gzip, bzip2, xz or
#' lzma). .zip zipped files need to be read using `read_zip_Renishaw()`.
#'
#' Renishaw .wxd files are converted to .txt ASCII files by their batch
#' converter. They come in a "long" format with columns (y x | time | z)?
#' wavelength intensity.  The first columns depend on the data type.
#'
#' The corresponding possibilities for the `data` argument are:
#' \tabular{lll}{ `data` \tab columns \tab \cr `"spc"` \tab wl int
#' \tab single spectrum \cr `"zspc"`, `"depth"` \tab z wl int \tab
#' depth profile\cr `"ts"` \tab t wl int \tab time series\cr
#' `"xyspc"` \tab y x wl int \tab 2d map\cr }
#'
#' This function allows reading very large ASCII files, but it does not work
#' on files with missing values (`NA`s are allowed).
#'
#' If the file is so large that it should be read in chunks and `nspc` is
#' not given, `read_txt_Renishaw` tries to guess it by using [count_lines()].
#'
#' @aliases read_txt_Renishaw read_zip_Renishaw
#'
#' @param file file name or connection
#' @param data type of file, one of "spc", "xyspc", "zspc", "depth", "ts",
#'        see  details.
#' @param nlines number of lines to read in each chunk, if 0 or less read
#'        whole file at once.
#'
#' `nlines` must cover at least one complete spectrum,i.e. `nlines`
#'   must be at least the number of data points per spectrum. Reasonable
#'   values start at `1e6`.
#' @param nspc number of spectra in the file
#' @param ... Arguments for `read_txt_Renishaw`
#'
#' @return  `hyperSpec` object.
#'
#'
#' @author C. Beleites
#'
#' @seealso [hyperSpec::read_txt_long()], [hyperSpec::read_txt_wide()],
#'   [base::scan()]
#'
#' @keywords IO file
#' @concept io
#'
#' @importFrom utils head
#' @export

read_txt_Renishaw <- function(file = stop("file is required"),
                              data = "xyspc", nlines = 0, nspc = NULL) {
  cols <- switch(data,
    spc = NULL,
    xyspc = list(
      y = expression("/"(y, mu * m)),
      x = expression("/"(x, mu * m))
    ),
    zspc = ,
    depth = list(z = expression("/"(z, mu * m))),
    ts = list(t = "t / s"),
    stop("unknown format for Renishaw .txt files.")
  )
  cols <- c(cols, list(
    .wavelength = expression(Delta * tilde(nu) / cm^-1),
    spc = "I / a.u."
  ))

  if (!is(file, "connection")) {
    file <- gzfile(file, "r")
  }

  on.exit(close(file))

  first <- scan(file, nlines = 1, quiet = TRUE)

  ncol <- length(first)

  if (ncol == 0) {
    return(new("hyperSpec"))
  }

  if (ncol != length(cols)) {
    stop(paste("File has", ncol, "columns, while 'cols' gives", length(cols)))
  }

  fbuf <- matrix(c(first, scan(file, quiet = TRUE, nlines = nlines)),
    ncol = ncol, byrow = TRUE
  )

  ## wavelength axis
  wl <- rep(TRUE, nrow(fbuf))
  for (i in seq_len(ncol(fbuf) - 2)) {
    wl[wl] <- fbuf[wl, i] == fbuf[1, i]
  }

  wl <- fbuf[wl, ncol - 1]

  ## if the file is to be read in chunks
  ## try to find out how many lines it has
  if (is.null(nspc)) {
    if (nlines > 0) {
      nspc <- count_lines(summary(file)$description, nlines)
      if (is.null(nspc)) {
        stop("Failed guessing nspc.")
      } else {
        message("Counted ", nspc, " lines = ", nspc / length(wl), " spectra.")
        nspc <- nspc / length(wl)
      }
    } else {
      nspc <- nrow(fbuf) / length(wl)
    }
  }

  data <- matrix(NA, ncol = ncol - 2, nrow = nspc)
  colnames(data) <- head(names(cols), -2)
  pos.data <- 0

  spc <- numeric(nspc * length(wl))
  pos.spc <- 0

  while (length(fbuf > 0)) {
    if (nlines > 0) cat(".")
    spc[pos.spc + seq_len(nrow(fbuf))] <- fbuf[, ncol]
    pos.spc <- pos.spc + nrow(fbuf)

    tmp <- fbuf[fbuf[, ncol - 1] == wl[1], seq_len(ncol - 2), drop = FALSE]

    data[pos.data + seq_len(nrow(tmp)), ] <- tmp
    pos.data <- pos.data + nrow(tmp)

    fbuf <- matrix(scan(file, quiet = TRUE, nlines = nlines),
      ncol = ncol,
      byrow = TRUE
    )

    if (length(fbuf > 0) & !all(unique(fbuf[, ncol - 1]) %in% wl)) {
      stop(
        "Wavelengths do not correspond to that of the other chunks. ",
        "Is the size of the first chunk large enough to cover a complete ",
        "spectrum?"
      )
    }
  }
  if (nlines > 0) cat("\n")

  spc <- matrix(spc, ncol = length(wl), nrow = nspc, byrow = TRUE)

  spc <- wl_sort(new("hyperSpec",
    spc = spc, data = as.data.frame(data),
    wavelength = wl, label = cols
  ))

  ## consistent file import behaviour across import functions
  .spc_io_postprocess_optional(spc, file)
}

# Unit tests -----------------------------------------------------------------

hySpc.testthat::test(read_txt_Renishaw) <- function() {
  local_edition(3)

  path <- system.file("extdata", "txt.Renishaw", package = "hySpc.read.txt")


  # Test with "paracetamol.txt" ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  f_paracetamol <- paste0(path, "/paracetamol.txt")
  expect_silent(spc <- read_txt_Renishaw(f_paracetamol, "spc"))

  n_wl <- nwl(spc)
  n_rows <- nrow(spc)
  n_clos <- ncol(spc)

  test_that("Renishaw .txt: hyperSpec obj. dimensions are correct", {
    expect_equal(n_wl, 4064)
    expect_equal(n_rows, 1)
    expect_equal(n_clos, 2)
  })

  test_that("Renishaw .txt: extra data are correct", {
    # @data colnames
    expect_equal(colnames(spc), c("spc", "filename"))

    # @data values
    # (Add tests, if relevant or remove this row)
  })

  test_that("Renishaw .txt: labels are correct", {
    expect_equal(spc@label$.wavelength, expression(Delta * tilde(nu) / cm^-1))
    expect_equal(spc@label$spc, expression("I / a.u."))
    expect_equal(spc@label$filename, "filename")
  })

  test_that("Renishaw .txt: spectra are correct", {
    # Dimensions of spectra matrix (@data$spc)
    expect_equal(dim(spc@data$spc), c(1, 4064))

    # Column names of spectra matrix
    expect_equal(colnames(spc@data$spc)[1], "96.7865")
    expect_equal(colnames(spc@data$spc)[10], "108.982")
    expect_equal(colnames(spc@data$spc)[n_wl], "3200.07") # last name

    # Values of spectra matrix
    expect_equal(unname(spc@data$spc[1, 1]), 2056.5)
    expect_equal(unname(spc@data$spc[1, 10]), 7248.13)
    expect_equal(unname(spc@data$spc[n_rows, n_wl]), 299.229) # last spc value
  })

  test_that("Renishaw .txt: wavelengths are correct", {
    expect_equal(spc@wavelength[1], 96.7865)
    expect_equal(spc@wavelength[10], 108.982)
    expect_equal(spc@wavelength[n_wl], 3200.07)
  })


  # Test with "laser.txt.gz" and "chondro.txt"

  f_laser <- paste0(path, "/laser.txt.gz")
  f_chondro <- paste0(path, "/chondro.txt")

  test_that("single spectrum", {
    tmp <- read_txt_Renishaw(f_paracetamol, "spc")
    expect_equal(dim(tmp), c(nrow = 1L, ncol = 2L, nwl = 4064L))
  })

  test_that("time series spectrum, gzipped", {
    tmp <- read_txt_Renishaw(f_laser, "ts")
    expect_equal(dim(tmp), c(nrow = 84L, ncol = 3L, nwl = 140L))
    expect_equal(colnames(tmp), c("t", "spc", "filename"))
  })

  test_that("map (= default)", {
    tmp <- read_txt_Renishaw(f_chondro, "xyspc")
    expect_equal(dim(tmp), c(nrow = 875L, ncol = 4L, nwl = 1272L))
    expect_equal(colnames(tmp), c("y", "x", "spc", "filename"))

    tmp <- read_txt_Renishaw(f_chondro)
    expect_equal(dim(tmp), c(nrow = 875L, ncol = 4L, nwl = 1272L))
    expect_equal(colnames(tmp), c("y", "x", "spc", "filename"))
  })

  test_that("chunked reading", {
    ## error on too small chunk size
    expect_error(
      read_txt_Renishaw(f_chondro, nlines = 10),
      "Wavelengths do not correspond"
    )

    tmp <- read_txt_Renishaw(f_chondro, nlines = 1e5)
    expect_equal(dim(tmp), c(nrow = 875L, ncol = 4L, nwl = 1272L))
  })

  test_that("compressed files", {
    files <- Sys.glob(paste0(path, "/chondro.*"))
    # .zip is tested with read_zip_Renishaw
    files <- grep("[.]zip", files, invert = TRUE, value = TRUE)
    for (f in files) {
      expect_silent(spc_chondro_i <- read_txt_Renishaw(f))
      expect_equal(dim(spc_chondro_i), c(nrow = 875L, ncol = 4L, nwl = 1272L))
    }
  })
}


# Function -------------------------------------------------------------------

#' @rdname read_txt_Renishaw
#'
#' @param txt.file name of the .txt file in the .zip archive. Defaults to zip
#'   file's name with extension .txt instead of .zip.
#'
#' @concept io
#'
#' @export
read_zip_Renishaw <- function(file = stop("filename is required"),
                              txt.file = sub("[.]zip", ".txt", basename(file)), ...) {
  read_txt_Renishaw(file = unz(file, filename = txt.file, "r"), ...)
}

# Unit tests -----------------------------------------------------------------

hySpc.testthat::test(read_zip_Renishaw) <- function() {
  local_edition(3)

  path <- system.file("extdata", "txt.Renishaw", package = "hySpc.read.txt")
  expect_silent(spc <- read_zip_Renishaw(paste0(path, "/chondro.zip")))

  n_wl <- nwl(spc)
  n_rows <- nrow(spc)
  n_clos <- ncol(spc)

  test_that("Renishaw .zip: hyperSpec obj. dimensions are correct", {
    expect_equal(n_wl, 1272)
    expect_equal(n_rows, 875)
    expect_equal(n_clos, 4)
  })

  test_that("Renishaw .zip: extra data are correct", {
    # @data colnames
    expect_equal(colnames(spc), c("y", "x", "spc", "filename"))

    # @data values
    # (Add tests, if relevant or remove this row)
  })

  test_that("Renishaw .zip: labels are correct", {
    expect_equal(spc@label$.wavelength, expression(Delta * tilde(nu) / cm^-1))
    expect_equal(spc@label$spc, expression("I / a.u."))
    expect_equal(spc@label$filename, "filename")
  })

  test_that("Renishaw .txt: spectra are correct", {
    # Dimensions of spectra matrix (@data$spc)
    expect_equal(dim(spc@data$spc), c(875, 1272))

    # Column names of spectra matrix
    expect_equal(colnames(spc@data$spc)[1], "601.622")
    expect_equal(colnames(spc@data$spc)[10], "610.989")
    expect_equal(colnames(spc@data$spc)[n_wl], "1802.15") # last name

    # Values of spectra matrix
    expect_equal(unname(spc@data$spc[1, 1]), 501.723)
    expect_equal(unname(spc@data$spc[1, 10]), 444.748)
    expect_equal(unname(spc@data$spc[n_rows, n_wl]), 151.921) # last spc value
  })

  test_that("Renishaw .txt: wavelengths are correct", {
    expect_equal(spc@wavelength[1], 601.622)
    expect_equal(spc@wavelength[10], 610.989)
    expect_equal(spc@wavelength[n_wl], 1802.15)
  })
}
