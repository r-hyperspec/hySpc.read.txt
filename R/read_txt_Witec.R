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
  if (all(is.na(txt[nrow(txt), ]))) {
    txt <- txt[-nrow(txt), ]
  }

  spc <- new("hyperSpec", wavelength = txt[1, ], spc = txt[-1, ])

  ## add header information
  if (hdr.label | hdr.units) {
    spc <- parse_hdr(spc, hdr, hdr.label)
  }

  ## add map information
  if (type == "map") {
    spc <- parse_xy(spc, hdr, hdr.label, points.per.line, lines.per.image)
  }

  ## consistent file import behavior across import functions
  .spc_io_postprocess_optional(spc, file)
}

hySpc.testthat::test(read_txt_Witec) <- function() {
  context("read_txt_Witec")

  tmpdir <- paste0(tempdir(), "/test_Witec_txt")
  untar("testfiles_Witec.tar.gz",
    files = c(
      "image2x3_Table.Data 1_F.txt", "timeseries3x_Table.Data 1.txt",
      "Witec-Map_label.txt", "Witec-Map_unit.txt", "Witec-Map_full.txt",
      "Witec-timeseries_label.txt", "Witec-timeseries_unit.txt", "Witec-timeseries_full.txt"
    ),
    exdir = tmpdir
  )
  on.exit(unlink(tmpdir))

  test_that("Map with neither header nor label lines", {
    expect_error(
      suppressWarnings(
        read_txt_Witec(paste0(tmpdir, "/image2x3_Table.Data 1_F.txt"),
          type = "map", hdr.units = TRUE, hdr.label = TRUE
        )
      )
    )
    expect_warning(
      read_txt_Witec(paste0(tmpdir, "/image2x3_Table.Data 1_F.txt"),
        type = "map"
      ),
      "no spatial information provided"
    )

    spc <- read_txt_Witec(paste0(tmpdir, "/image2x3_Table.Data 1_F.txt"),
      type = "map", points.per.line = 3, lines.per.image = 2
    )
    spc$filename <- gsub("^.*/", "", spc$filename)
    expect_setequal(spc$x, 1:3)
    expect_setequal(spc$y, -1:-2)
    expect_known_hash(spc, hash = "a74cdcd428")
  })

  test_that("Map: one of points.per.line and lines.per.image is sufficient", {
    spc <- read_txt_Witec(
      paste0(tmpdir, "/image2x3_Table.Data 1_F.txt"),
      type = "map",
      lines.per.image = 2
    )
    spc$filename <- gsub("^.*/", "", spc$filename)
    expect_known_hash(spc, hash = "a74cdcd428")

    spc <- read_txt_Witec(paste0(tmpdir, "/image2x3_Table.Data 1_F.txt"),
      type = "map", points.per.line = 3
    )
    spc$filename <- gsub("^.*/", "", spc$filename)
    expect_known_hash(spc, hash = "a74cdcd428")
  })

  test_that("Map with label line but no units header", {
    spc <- read_txt_Witec(paste0(tmpdir, "/Witec-Map_label.txt"),
      type = "map",
      hdr.units = FALSE, hdr.label = TRUE
    )
    spc$filename <- gsub("^.*/", "", spc$filename)
    expect_known_hash(spc, "5d1ed15baf")
  })

  test_that("Map with units header line but no labels", {
    expect_warning(
      spc <- read_txt_Witec(paste0(tmpdir, "/Witec-Map_unit.txt"),
        type = "map", hdr.units = TRUE, hdr.label = FALSE
      ),
      "no spatial information provided"
    )
    expect_null(spc$x)
    expect_null(spc$y)
    spc$filename <- gsub("^.*/", "", spc$filename)
    expect_known_hash(spc, "123fc77bdf")

    spc <- read_txt_Witec(paste0(tmpdir, "/Witec-Map_unit.txt"),
      type = "map", hdr.units = TRUE, hdr.label = FALSE,
      points.per.line = 5, lines.per.image = 5
    )
    spc$filename <- gsub("^.*/", "", spc$filename)
    expect_known_hash(spc, "f43487ae66")
  })

  test_that("Map with header and label lines", {
    spc <- read_txt_Witec(paste0(tmpdir, "/Witec-Map_full.txt"),
      type = "map",
      hdr.units = TRUE, hdr.label = TRUE
    )

    spc$filename <- gsub("^.*/", "", spc$filename)
    expect_known_hash(spc, "60a1e20338")
  })

  test_that("Map can be read as time series", {
    spc <- read_txt_Witec(paste0(tmpdir, "/image2x3_Table.Data 1_F.txt"))
    expect_null(spc$x)
    expect_null(spc$y)
    spc$filename <- gsub("^.*/", "", spc$filename)
    expect_known_hash(spc, "40030b99dc")
  })

  test_that("parameter default type = 'single'", {
    spc <- read_txt_Witec(paste0(tmpdir, "/timeseries3x_Table.Data 1.txt"))
    spc$filename <- gsub("^.*/", "", spc$filename)
    expect_known_hash(spc, "37fc7cadcc")
  })

  test_that("Time series with neither header nor label lines", {
    spc <- read_txt_Witec(paste0(tmpdir, "/timeseries3x_Table.Data 1.txt"))
    spc$filename <- gsub("^.*/", "", spc$filename)
    expect_known_hash(spc, "37fc7cadcc")
  })

  test_that("Time series with label line but no units header", {
    spc <- read_txt_Witec(paste0(tmpdir, "/Witec-timeseries_label.txt"), hdr.units = FALSE, hdr.label = TRUE)
    spc$filename <- gsub("^.*/", "", spc$filename)
    expect_known_hash(spc, "d102a0b244")
  })

  test_that("Time series with units header line but no labels", {
    spc <- read_txt_Witec(paste0(tmpdir, "/Witec-timeseries_unit.txt"), hdr.units = TRUE, hdr.label = FALSE)

    spc$filename <- gsub("^.*/", "", spc$filename)
    expect_known_hash(spc, "375a63ae31")
  })

  test_that("Time series with header and label lines", {
    expect_error(spc <- read_txt_Witec(paste0(tmpdir, "/Witec-timeseries_full.txt")))

    spc <- read_txt_Witec(paste0(tmpdir, "/Witec-timeseries_full.txt"),
      hdr.units = TRUE, hdr.label = TRUE
    )
    spc$filename <- gsub("^.*/", "", spc$filename)
    expect_known_hash(spc, "7fa6a0be645")
  })

  test_that("encoding", {
    spc <- read_txt_Witec(paste0(tmpdir, "/Witec-timeseries_full.txt"),
      hdr.units = TRUE, hdr.label = TRUE,
      encoding = "ascii"
    )
    spc$filename <- gsub("^.*/", "", spc$filename)
    expect_known_hash(spc, "7fa6a0be645")
  })
}
