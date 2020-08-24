#' Import Raman Spectra/Maps from Witec Instrument via ASCII files
#'
#' \code{read_txt_Witec} reads Witec ASCII files where the first column gives the wavelength
#' axes and the other columns the spectra. \code{read_dat_Witec} reads Witec's ASCII exported data
#' which comes in separate files with x and y data.
#'
#' @title File Import Witec Raman
#' @param file filename or connection to ASCII file
#' @param points.per.line number of spectra in x direction of the map
#' @param lines.per.image number of spectra in y direction
#' @param type type of spectra: \code{single} for single spectra (including time series), \code{map} for imaging data.
#' @param hdr.label WITec Project exports the spectra names (contain information of map position or number of spectra) within the \code{file}.
#' @param hdr.units WITec Project exports the spectra units within the \code{file}.
#' @param encoding character encoding, see \code{\link[base]{readLines}}
#' @param ...,quiet handed to \code{\link[base]{scan}}
#' @return a hyperSpec object
#' @author Claudia Beleites and Marcel Dahms
#' @importFrom hySpc.testthat test<-
#' @importFrom utils head
#' @seealso \code{vignette ("fileio")} for more information on file import and
#' \code{\link{options}} for details on options.
#' @export
read_txt_Witec <- function(file = stop("filename or connection needed"),
                           points.per.line = NULL,
                           lines.per.image = NULL,
                           type = c("single", "map"),
                           hdr.label = FALSE,
                           hdr.units = FALSE,
                           encoding = "unknown",
                           ...,
                           quiet = TRUE) {

  ## check for valid data connection
  check_con(file = file)

  ## check for valid input
  type <- check_valid(type, hdr = NULL, points.per.line, lines.per.image)

  ## manage possible header lines by export function 'Table' in WITec Control/Project (version 4)
  skip <- hdr.label + hdr.units

  ## read spectra
  tmp <- readLines(file, encoding = encoding)
  nwl <- length(tmp) - skip
  txt <- scan(text = tmp, skip = skip, quiet = quiet, encoding = encoding, ...)

  dim(txt) <- c(length(txt) / nwl, nwl)

  hdr <- head(tmp, skip)

  ## fix: Witec/Andor may have final comma without values
  if (all(is.na(txt [nrow(txt), ]))) {
    txt <- txt [-nrow(txt), ]
  }

  spc <- new("hyperSpec", wavelength = txt [1, ], spc = txt [-1, ])

  ## add header information
  if (hdr.label | hdr.units) {
    spc <- parse_hdr(spc, hdr, hdr.label)
  }

  ## add map information
  if (type == "map") {
    spc <- parse_xy(spc, hdr, hdr.label, points.per.line, lines.per.image)
  }

  ## consistent file import behavior across import functions
  .fileio.optional(spc, file)
}

test(read_txt_Witec) <- function() {
  context("read_txt_Witec")

  test_that("Map with neither header nor label lines", {
    skip("TODO: adapt to new package")
    expect_error(suppressWarnings(read_txt_Witec("fileio/txt.Witec/Witec-Map_no.txt",
      type = "map", hdr.units = TRUE, hdr.label = TRUE
    )))
    expect_warning(read_txt_Witec("fileio/txt.Witec/Witec-Map_no.txt", type = "map"))

    spc <- read_txt_Witec("fileio/txt.Witec/Witec-Map_no.txt", type = "map", points.per.line = 5, lines.per.image = 5)
    expect_known_hash(spc, hash = "6816a87cf3")
  })

  test_that("Map: one of points.per.line and lines.per.image is sufficient", {
    skip("TODO: adapt to new package")
    spc <- read_txt_Witec("fileio/txt.Witec/Witec-Map_no.txt", type = "map", lines.per.image = 5)
    expect_known_hash(spc, hash = "6816a87cf3")

    spc <- read_txt_Witec("fileio/txt.Witec/Witec-Map_no.txt", type = "map", points.per.line = 5)
    expect_known_hash(spc, hash = "6816a87cf3")
  })

  test_that("Map with label line but no units header", {
    skip("TODO: adapt to new package")
    spc <- read_txt_Witec("fileio/txt.Witec/Witec-Map_label.txt", type = "map", hdr.units = FALSE, hdr.label = TRUE)
    expect_known_hash(spc, "c4a384d6b2")
  })

  test_that("Map with units header line but no labels", {
    skip("TODO: adapt to new package")
    expect_warning(spc <- read_txt_Witec("fileio/txt.Witec/Witec-Map_unit.txt", type = "map", hdr.units = TRUE, hdr.label = FALSE))
    expect_null(spc$x)
    expect_null(spc$y)

    spc <- read_txt_Witec("fileio/txt.Witec/Witec-Map_unit.txt",
      type = "map", hdr.units = TRUE, hdr.label = FALSE,
      points.per.line = 5, lines.per.image = 5
    )
    expect_known_hash(spc, "86ecc17360")
  })

  test_that("Map with header and label lines", {
    skip("TODO: adapt to new package")
    spc <- read_txt_Witec("fileio/txt.Witec/Witec-Map_full.txt", type = "map", hdr.units = TRUE, hdr.label = TRUE)
    expect_known_hash(spc, "76db6397fc")
  })

  test_that("Map can be read as time series", {
    skip("TODO: adapt to new package")
    spc <- read_txt_Witec("fileio/txt.Witec/Witec-Map_no.txt")
    expect_known_hash(spc, "6213aefc6b")
    expect_null(spc$x)
    expect_null(spc$y)
  })

  test_that("parameter default type = 'single'", {
    skip("TODO: adapt to new package")
    spc <- read_txt_Witec("fileio/txt.Witec/Witec-timeseries_no.txt")
    expect_known_hash(spc, "1a8c3be079")
  })

  test_that("Time series with neither header nor label lines", {
    skip("TODO: adapt to new package")
    spc <- read_txt_Witec("fileio/txt.Witec/Witec-timeseries_no.txt")
    expect_known_hash(spc, "1a8c3be079")
  })

  test_that("Time series with label line but no units header", {
    skip("TODO: adapt to new package")
    spc <- read_txt_Witec("fileio/txt.Witec/Witec-timeseries_label.txt", hdr.units = FALSE, hdr.label = TRUE)
    expect_known_hash(spc, "4cb098a671")
  })

  test_that("Time series with units header line but no labels", {
    skip("TODO: adapt to new package")
    spc <- read_txt_Witec("fileio/txt.Witec/Witec-timeseries_unit.txt", hdr.units = TRUE, hdr.label = FALSE)

    expect_known_hash(spc, "6b6abac4e8")
  })

  test_that("Time series with header and label lines", {
    skip("TODO: adapt to new package")
    expect_error(spc <- read_txt_Witec("fileio/txt.Witec/Witec-timeseries_full.txt"))

    spc <- read_txt_Witec("fileio/txt.Witec/Witec-timeseries_full.txt", hdr.units = TRUE, hdr.label = TRUE)
    expect_known_hash(spc, "db5b1a5db0")
  })

  test_that("encoding", {
    skip("TODO: adapt to new package")
    spc <- read_txt_Witec("fileio/txt.Witec/Witec-timeseries_full.txt",
      hdr.units = TRUE, hdr.label = TRUE,
      encoding = "ascii"
    )
    expect_known_hash(spc, "db5b1a5db0")
  })
}

