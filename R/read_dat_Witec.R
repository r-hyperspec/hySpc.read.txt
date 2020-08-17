
#' @rdname read_txt_Witec
#' @param filex filename wavelength axis file
#' @param filey filename intensity file
#' @export
read_dat_Witec <- function(filex = stop("filename or connection needed"),
                           filey = sub("-x", "-y", filex),
                           points.per.line = NULL,
                           lines.per.image = NULL,
                           type = c("single", "map"),
                           encoding = "unknown",
                           ...,
                           quiet = hy.getOption("debuglevel") < 1L) {
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
  .fileio.optional(spc, filey)
}

test(read_dat_Witec) <- function() {
  context("read_dat_Witec")

  test_that("-y file guessing", {
    skip("TODO: adapt to new package")
    spc <- read_dat_Witec("fileio/txt.Witec/Witec-timeseries-x.dat")
    expect_known_hash(spc, "9562f59323")
  })

  test_that("encoding", {
    skip("TODO: adapt to new package")
    spc <- read_dat_Witec("fileio/txt.Witec/Witec-timeseries-x.dat", encoding = "ascii")
    expect_known_hash(spc, "9562f59323")
  })

  test_that("Time series", {
    skip("TODO: adapt to new package")
    spc <- read_dat_Witec("fileio/txt.Witec/Witec-timeseries-x.dat", "fileio/txt.Witec/Witec-timeseries-y.dat")
    expect_known_hash(spc, "9562f59323")
  })

  test_that("Map: .dat does not have spatial information", {
    skip("TODO: adapt to new package")
    spc <- read_dat_Witec("fileio/txt.Witec/Witec-Map-x.dat", "fileio/txt.Witec/Witec-Map-y.dat")
    expect_null(spc$x)
    expect_null(spc$y)
    expect_known_hash(spc, "8a7ed06b0b")
  })

  test_that("Map", {
    skip("TODO: adapt to new package")
    expect_warning(read_dat_Witec("fileio/txt.Witec/Witec-Map-x.dat", "fileio/txt.Witec/Witec-Map-y.dat",
      points.per.line = 5, lines.per.image = 5
    ))

    spc <- read_dat_Witec("fileio/txt.Witec/Witec-Map-x.dat", "fileio/txt.Witec/Witec-Map-y.dat",
      type = "map", points.per.line = 5, lines.per.image = 5
    )
    expect_known_hash(spc, "3d6339675b")
  })
}

