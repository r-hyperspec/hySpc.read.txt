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
  local_edition(2)

  context("read_dat_WITec")

  tmpdir <- paste0(tempdir(), "/test_Witec_dat")
  on.exit(unlink(tmpdir, force = TRUE))

  untar("testfiles_Witec.tar.gz",
    files = c(
      "Witec-Map-x.dat",
      "Witec-Map-y.dat",
      "Witec-timeseries-x.dat",
      "Witec-timeseries-y.dat"
    ),
    exdir = tmpdir
  )

  test_that("-y file guessing", {
    spc <- read_dat_WITec(paste0(tmpdir, "/Witec-timeseries-x.dat"))
    spc$filename <- gsub("^.*/", "", spc$filename)
    expect_known_hash(spc, "1977fe5997")
  })

  test_that("encoding", {
    spc <- read_dat_WITec(
      paste0(tmpdir, "/Witec-timeseries-x.dat"),
      encoding = "ascii"
    )
    spc$filename <- gsub("^.*/", "", spc$filename)

    expect_known_hash(spc, "1977fe5997")
  })

  test_that("Time series", {
    spc <- read_dat_WITec(
      paste0(tmpdir, "/Witec-timeseries-x.dat"),
      paste0(tmpdir, "/Witec-timeseries-y.dat")
    )
    spc$filename <- gsub("^.*/", "", spc$filename)

    expect_known_hash(spc, "1977fe5997")
  })

  test_that("Map: .dat does not have spatial information", {
    spc <- read_dat_WITec(
      paste0(tmpdir, "/Witec-Map-x.dat"),
      paste0(tmpdir, "/Witec-Map-y.dat")
    )
    expect_null(spc$x)
    expect_null(spc$y)
    spc$filename <- gsub("^.*/", "", spc$filename)

    expect_known_hash(spc, "b523735004")
  })

  test_that("Map", {
    expect_warning(
      read_dat_WITec(
        paste0(tmpdir, "/Witec-Map-x.dat"),
        paste0(tmpdir, "/Witec-Map-y.dat"),
        points.per.line = 5, lines.per.image = 5
      ),
      "points.per.line != 1 given for single spectrum"
    )
    expect_warning(
      read_dat_WITec(
        paste0(tmpdir, "/Witec-Map-x.dat"),
        paste0(tmpdir, "/Witec-Map-y.dat"),
        points.per.line = 5, lines.per.image = 5
      ),
      "lines.per.image != 1 are defined for single spectrum"
    )

    spc <- read_dat_WITec(
      paste0(tmpdir, "/Witec-Map-x.dat"),
      paste0(tmpdir, "/Witec-Map-y.dat"),
      type = "map", points.per.line = 5, lines.per.image = 5
    )
    spc$filename <- gsub("^.*/", "", spc$filename)

    expect_known_hash(spc, "efc28c0d45")
  })
}
