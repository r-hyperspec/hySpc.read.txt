# Function -------------------------------------------------------------------

#' @rdname read_txt_WITec
#'
#' @param headerfile Path or connection to ASCII file with header information.
#'
#' @importFrom methods new
#' @export
read_txt_WITec_Graph <- function(headerfile = stop("filename or connection needed"),
                                 filex = gsub("Header", "X-Axis", headerfile),
                                 filey = gsub("Header", "Y-Axis", headerfile),
                                 type = c("single", "map"),
                                 encoding = "unknown",
                                 ...,
                                 quiet = TRUE) {
  ## check for valid data connection
  check_con(headerfile, filex, filey)

  ## processing headerfile
  hdr <- read_ini(headerfile, skip = 1, encoding = encoding)
  # returns a matrix with colnames and rownames for better addressing
  hdr <- sapply(hdr, function(x) unlist(x, recursive = FALSE))

  ## check valid input
  type <- check_valid(
    type = type, hdr = hdr,
    ...
  )

  ## read spectra and header
  wl <- scan(filex, quiet = quiet, encoding = encoding)
  nwl <- length(wl)

  txt <- scan(filey, quiet = quiet, encoding = encoding)
  dim(txt) <- c(nwl, length(txt) / nwl)

  spc <- new("hyperSpec", wavelength = wl, spc = t(txt))

  ## cross validation of parameters and information provided by header file
  if (nwl != hdr["SizeGraph", ]) {
    stop(paste("length of wavelength axis in file '", filex,
      "' differs from 'SizeGraph' in header file '", headerfile, "'",
      sep = ""
    ))
  }

  ## add header information
  spc <- parse_hdr(spc, hdr)

  ## add map information
  if (type == "map") {
    spc <- parse_xy(spc, hdr, ...)
  }

  ## consistent file import behaviour across import functions
  .spc_io_postprocess_optional(spc, filey)
}


# Unit tests -----------------------------------------------------------------

hySpc.testthat::test(read_txt_WITec_Graph) <- function() {
  context("read_txt_WITec_Graph")

  local_edition(3)

  filename <- system.file(
    "extdata",
    "txt.Witec/nofilename_Header.txt",
    package = "hySpc.read.txt"
  )

  expect_silent(spc <- read_txt_WITec_Graph(filename, encoding = "latin1"))

  n_wl <- nwl(spc)
  n_rows <- nrow(spc)
  n_clos <- ncol(spc)

  test_that("WITec Graph .txt: hyperSpec obj. dimensions are correct", {
    expect_equal(n_wl, 1024)
    expect_equal(n_rows, 1)
    expect_equal(n_clos, 3)
  })

  test_that("WITec Graph .txt: extra data are correct", {
    # @data colnames
    expect_equal(colnames(spc), c("spc", "spcname", "filename"))
  })

  test_that("WITec Graph .txt: labels are correct", {
    expect_equal(spc@label$.wavelength, NULL)
    expect_equal(spc@label$spc, "CCD cts")
    expect_equal(spc@label$filename, "filename")
  })

  test_that("WITec Graph .txt: spectra are correct", {
    # Dimensions of spectra matrix (@data$spc)
    expect_equal(dim(spc@data$spc), c(1, 1024))

    # Column names of spectra matrix
    expect_equal(colnames(spc@data$spc)[1], "-105.146")
    expect_equal(colnames(spc@data$spc)[10], "-61.0613")
    expect_equal(colnames(spc@data$spc)[n_wl], "3811.88") # last name

    # Values of spectra matrix
    expect_equal(unname(spc@data$spc[1, 1]), 561)
    expect_equal(unname(spc@data$spc[1, 10]), 567)
    expect_equal(unname(spc@data$spc[n_rows, n_wl]), 651) # last spc value
  })

  test_that("WITec Graph .txt: wavelengths are correct", {
    expect_equal(spc@wavelength[1], -105.146)
    expect_equal(spc@wavelength[10], -61.0613)
    expect_equal(spc@wavelength[n_wl], 3811.88)
  })
}

# Old unit tests -----------------------------------------------------------------

# hySpc.testthat::test(read_txt_WITec_Graph) <- function() {
#   local_edition(2)
#
#   context("read_txt_WITec_Graph")
#
#   tmpdir <- paste0(tempdir(), "/test_Witec_txt_Graph")
#   untar("testfiles_Witec.tar.gz",
#         files = c(
#           "nofilename (Header).txt",
#           "nofilename (X-Axis).txt",
#           "nofilename (Y-Axis).txt",
#           "timeseries3x_GraphASCII.Data 1 (Header).txt",
#           "timeseries3x_GraphASCII.Data 1 (X-Axis).txt",
#           "timeseries3x_GraphASCII.Data 1 (Y-Axis).txt",
#           "image2x3_GraphASCII.Data 1_F (Header).txt",
#           "image2x3_GraphASCII.Data 1_F (X-Axis).txt",
#           "image2x3_GraphASCII.Data 1_F (Y-Axis).txt"
#         ),
#         exdir = tmpdir
#   )
#
#   on.exit(unlink(tmpdir))
#
#   test_that("defaults and (X-Axis)/(Y-Axis) file guessing", {
#     spc <- read_txt_WITec_Graph(
#       paste0(tmpdir, "/timeseries3x_GraphASCII.Data 1 (Header).txt")
#     )
#
#     expect_equal(dim(spc), c(nrow = 3L, ncol = 4L, nwl = 1024L))
#
#     expect_equal(
#       spc$filename,
#       rep(paste0(tmpdir, "/timeseries3x_GraphASCII.Data 1 (Y-Axis).txt"), 3)
#     )
#
#     expect_equal(
#       spc$WIPname,
#       rep("F:\\Acetamidophenol samples_20200703.wip", 3)
#     )
#
#     expect_equal(
#       spc$spcname,
#       rep("4Acetamidophenol_timeseries3x_20200703_001_Spec.Data 1", 3)
#     )
#
#     expect_equivalent(spc[[2, , 1650]], 2427)
#   })
#
#   test_that("encoding", {
#     spc <- read_txt_WITec_Graph(
#       paste0(tmpdir, "/nofilename (Header).txt"),
#       encoding = "latin1"
#     )
#     spc$filename <- gsub("^.*/", "", spc$filename)
#
#     expect_known_hash(spc, "b273847b9c")
#   })
#
#   test_that("Time Series", {
#     spc <- read_txt_WITec_Graph(
#       paste0(tmpdir, "/timeseries3x_GraphASCII.Data 1 (Header).txt"),
#       type = "single"
#     )
#     spc$filename <- gsub("^.*/", "", spc$filename)
#
#     expect_known_hash(spc, "a5ff28fc8b")
#   })
#
#   test_that("Map", {
#     expect_warning(
#       read_txt_WITec_Graph(
#         paste0(tmpdir, "/image2x3_GraphASCII.Data 1_F (Header).txt"),
#         encoding = "latin1"
#       ),
#       "header provides spatial information in y direction for single spectra"
#     )
#
#     expect_warning(
#       read_txt_WITec_Graph(
#         paste0(tmpdir, "/image2x3_GraphASCII.Data 1_F (Header).txt"),
#         encoding = "latin1", type = "single"
#       ),
#       "header provides spatial information in y direction for single spectra"
#     )
#
#     spc <- read_txt_WITec_Graph(
#       paste0(tmpdir, "/image2x3_GraphASCII.Data 1_F (Header).txt"),
#       type = "map", encoding = "latin1"
#     )
#     spc$filename <- gsub("^.*/", "", spc$filename)
#
#     expect_known_hash(spc, "1e92447516")
#   })
#
#   test_that("missing filename", {
#     spc <- read_txt_WITec_Graph(paste0(tmpdir, "/nofilename (Header).txt"),
#                                 encoding = "latin1"
#     )
#     spc$filename <- gsub("^.*/", "", spc$filename)
#
#     expect_known_hash(spc, "b273847b9c")
#   })
#
#   test_that("wrong combination of file names", {
#     expect_error(
#       read_txt_WITec_Graph(
#         paste0(tmpdir, "/timeseries3x_GraphASCII.Data 1 (Header).txt"),
#         paste0(tmpdir, "/timeseries3x_GraphASCII.Data 1 (Y-Axis).txt")
#       ),
#       "length of wavelength axis .* differs from 'SizeGraph' in header"
#     )
#   })
# }
