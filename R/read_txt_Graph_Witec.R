#' @rdname read.txt.Witec
#' @param headerfile filename or connection to ASCII file with header information
#' @export
read_txt_Witec_Graph <- function(headerfile = stop("filename or connection needed"),
                                 filex = gsub("Header", "X-Axis", headerfile),
                                 filey = gsub("Header", "Y-Axis", headerfile),
                                 type = c("single", "map"), encoding = "unknown",
                                 ..., quiet = TRUE) {
  ## check for valid data connection
  check_con(headerfile, filex, filey)

  ## processing headerfile
  hdr <- read_ini(headerfile, skip = 1, encoding = encoding)
  hdr <- sapply(hdr, function(x) unlist(x, recursive = FALSE)) # returns a matrix with colnames and rownames for better adressing

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
  .fileio.optional(spc, filey)
}

test(read_txt_Witec_Graph) <- function() {
  context("read_txt_Witec_Graph")

  tmpdir <- paste0(tempdir(), "/Witec_GraphASCII")
  untar("Witec_GraphASCII.tar.gz", exdir = tmpdir)

  on.exit(unlink(tmpdir))

  test_that("defaults and (X-Axis)/(Y-Axis) file guessing", {
    spc <- read_txt_Witec_Graph(paste0(tmpdir, "/timeseries3x_GraphASCII.Data 1 (Header).txt"))

    expect_equal(dim(spc), c(nrow = 3L, ncol = 4L, nwl = 1024L))

    expect_equal(spc$filename,
                 rep(paste0(tmpdir, "/timeseries3x_GraphASCII.Data 1 (Y-Axis).txt"), 3))

    expect_equal(spc$WIPname,
                 rep("F:\\Acetamidophenol samples_20200703.wip", 3))

    expect_equal(spc$spcname,
                 rep("4Acetamidophenol_timeseries3x_20200703_001_Spec.Data 1", 3))

    expect_equivalent(spc[[2,,1650]], 2427)
  })

  test_that("encoding", {
    skip("TODO: adapt to new package")
    expect_warning(read_txt_Witec_Graph("fileio/txt.Witec/nofilename (Header).txt"))

    spc <- read_txt_Witec_Graph("fileio/txt.Witec/nofilename (Header).txt", encoding = "latin1")
    expect_known_hash(spc, "2bad36adb3")
  })

  test_that("Time Series", {
    skip("TODO: adapt to new package")
    spc <- read_txt_Witec_Graph("fileio/txt.Witec/Witec-timeseries (Header).txt", type = "single")
    expect_known_hash(spc, "295499c43c")
  })

  test_that("Map", {
    skip("TODO: adapt to new package")
    expect_warning(read_txt_Witec_Graph("fileio/txt.Witec/Witec-Map (Header).txt"))
    expect_warning(read_txt_Witec_Graph("fileio/txt.Witec/Witec-Map (Header).txt", type = "single"))

    spc <- read_txt_Witec_Graph("fileio/txt.Witec/Witec-Map (Header).txt", type = "map")
    expect_known_hash(spc, "cb9cd9757a")
  })

  test_that("missing filename", {
    skip("TODO: adapt to new package")
    spc <- read_txt_Witec_Graph("fileio/txt.Witec/nofilename (Header).txt", encoding = "latin1")
    expect_known_hash(spc, "2bad36adb3")
  })

  test_that("wrong combination of file names", {
    skip("TODO: adapt to new package")
    expect_error(read_txt_Witec_Graph("fileio/txt.Witec/Witec-timeseries (Header).txt", "fileio/txt.Witec/Witec-timeseries (Y-Axis).txt"))
  })
}
