tmpdir <- paste0(tempdir(), "/txt_Witec")
untar("txt_Witec.tar.gz", exdir = tmpdir)
on.exit(unlink(tmpdir))

# ******************************************************************************
context("read.dat.Witec")
# ******************************************************************************

test_that("-y file guessing", {
  skip("TODO: adapt to new package")
  spc <- read.dat.Witec("fileio/txt.Witec/Witec-timeseries-x.dat")
  expect_known_hash(spc, "9562f59323")
})

test_that("encoding", {
  skip("TODO: adapt to new package")
  spc <- read.dat.Witec("fileio/txt.Witec/Witec-timeseries-x.dat", encoding = "ascii")
  expect_known_hash(spc, "9562f59323")
})

test_that("Time series", {
  skip("TODO: adapt to new package")
  spc <- read.dat.Witec("fileio/txt.Witec/Witec-timeseries-x.dat", "fileio/txt.Witec/Witec-timeseries-y.dat")
  expect_known_hash(spc, "9562f59323")
})

test_that("Map: .dat does not have spatial information", {
  skip("TODO: adapt to new package")
  spc <- read.dat.Witec("fileio/txt.Witec/Witec-Map-x.dat", "fileio/txt.Witec/Witec-Map-y.dat")
  expect_null(spc$x)
  expect_null(spc$y)
  expect_known_hash(spc, "8a7ed06b0b")
})

test_that("Map", {
  skip("TODO: adapt to new package")
  expect_warning(read.dat.Witec("fileio/txt.Witec/Witec-Map-x.dat", "fileio/txt.Witec/Witec-Map-y.dat",
                                points.per.line = 5, lines.per.image = 5
  ))

  spc <- read.dat.Witec("fileio/txt.Witec/Witec-Map-x.dat", "fileio/txt.Witec/Witec-Map-y.dat",
                        type = "map", points.per.line = 5, lines.per.image = 5
  )
  expect_known_hash(spc, "3d6339675b")
})



# ******************************************************************************
context("read.txt.Witec")
# ******************************************************************************

test_that("Map with neither header nor label lines", {
  skip("TODO: adapt to new package")
  expect_error(suppressWarnings(read.txt.Witec("fileio/txt.Witec/Witec-Map_no.txt",
                                               type = "map", hdr.units = TRUE, hdr.label = TRUE
  )))
  expect_warning(read.txt.Witec("fileio/txt.Witec/Witec-Map_no.txt", type = "map"))

  spc <- read.txt.Witec("fileio/txt.Witec/Witec-Map_no.txt", type = "map", points.per.line = 5, lines.per.image = 5)
  expect_known_hash(spc, hash = "6816a87cf3")
})

test_that("Map: one of points.per.line and lines.per.image is sufficient", {
  skip("TODO: adapt to new package")
  spc <- read.txt.Witec("fileio/txt.Witec/Witec-Map_no.txt", type = "map", lines.per.image = 5)
  expect_known_hash(spc, hash = "6816a87cf3")

  spc <- read.txt.Witec("fileio/txt.Witec/Witec-Map_no.txt", type = "map", points.per.line = 5)
  expect_known_hash(spc, hash = "6816a87cf3")
})

test_that("Map with label line but no units header", {
  skip("TODO: adapt to new package")
  spc <- read.txt.Witec("fileio/txt.Witec/Witec-Map_label.txt", type = "map", hdr.units = FALSE, hdr.label = TRUE)
  expect_known_hash(spc, "c4a384d6b2")
})

test_that("Map with units header line but no labels", {
  skip("TODO: adapt to new package")
  expect_warning(spc <- read.txt.Witec("fileio/txt.Witec/Witec-Map_unit.txt", type = "map", hdr.units = TRUE, hdr.label = FALSE))
  expect_null(spc$x)
  expect_null(spc$y)

  spc <- read.txt.Witec("fileio/txt.Witec/Witec-Map_unit.txt",
                        type = "map", hdr.units = TRUE, hdr.label = FALSE,
                        points.per.line = 5, lines.per.image = 5
  )
  expect_known_hash(spc, "86ecc17360")
})

test_that("Map with header and label lines", {
  skip("TODO: adapt to new package")
  spc <- read.txt.Witec("fileio/txt.Witec/Witec-Map_full.txt", type = "map", hdr.units = TRUE, hdr.label = TRUE)
  expect_known_hash(spc, "76db6397fc")
})

test_that("Map can be read as time series", {
  skip("TODO: adapt to new package")
  spc <- read.txt.Witec("fileio/txt.Witec/Witec-Map_no.txt")
  expect_known_hash(spc, "6213aefc6b")
  expect_null(spc$x)
  expect_null(spc$y)
})


test_that("parameter default type = 'single'", {
  skip("TODO: adapt to new package")
  spc <- read.txt.Witec("fileio/txt.Witec/Witec-timeseries_no.txt")
  expect_known_hash(spc, "1a8c3be079")
})

test_that("Time series with neither header nor label lines", {
  skip("TODO: adapt to new package")
  spc <- read.txt.Witec("fileio/txt.Witec/Witec-timeseries_no.txt")
  expect_known_hash(spc, "1a8c3be079")
})

test_that("Time series with label line but no units header", {
  skip("TODO: adapt to new package")
  spc <- read.txt.Witec("fileio/txt.Witec/Witec-timeseries_label.txt", hdr.units = FALSE, hdr.label = TRUE)
  expect_known_hash(spc, "4cb098a671")
})

test_that("Time series with units header line but no labels", {
  skip("TODO: adapt to new package")
  spc <- read.txt.Witec("fileio/txt.Witec/Witec-timeseries_unit.txt", hdr.units = TRUE, hdr.label = FALSE)

  expect_known_hash(spc, "6b6abac4e8")
})

test_that("Time series with header and label lines", {
  skip("TODO: adapt to new package")
  expect_error(spc <- read.txt.Witec("fileio/txt.Witec/Witec-timeseries_full.txt"))

  spc <- read.txt.Witec("fileio/txt.Witec/Witec-timeseries_full.txt", hdr.units = TRUE, hdr.label = TRUE)
  expect_known_hash(spc, "db5b1a5db0")
})

test_that("encoding", {
  skip("TODO: adapt to new package")
  spc <- read.txt.Witec("fileio/txt.Witec/Witec-timeseries_full.txt",
                        hdr.units = TRUE, hdr.label = TRUE,
                        encoding = "ascii"
  )
  expect_known_hash(spc, "db5b1a5db0")
})


# ******************************************************************************
context("read_txt_Witec_Graph")
# ******************************************************************************

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


# ******************************************************************************
context("read_txt_Witec_TrueMatch")
# ******************************************************************************

test_that("Witec TrueMatch example file", {
  spc <- read_txt_Witec_TrueMatch(paste0(tmpdir, "/Witec_TrueMatch.txt"))

  expect_equal(dim(spc), c(nrow = 2L, ncol = length(colnames(spc)), nwl = 1024L))
  expect_equal(spc$filename, rep(paste0(tmpdir, "/Witec_TrueMatch.txt"), 2))

  expect_equivalent(spc [[,, 610]], c(902, 732))
})

test_that("multiple spectra with varying wavelengths return error", {
  spc <- read_txt_Witec_TrueMatch(paste0(tmpdir, "/Witec_TrueMatch.txt"))

  expect_equivalent(length(spc@data[1, c("spc")]),length(spc@data[2, c("spc")]))
})

test_that("spectra are in correct positions", {
  spc <- read_txt_Witec_TrueMatch(paste0(tmpdir, "/Witec_TrueMatch.txt"))

  expect_equivalent(is.matrix(spc@data[1, c("spc")]), is.matrix(spc@data[2, c("spc")]))
})

test_that("spectra data is correctly parsed", {
  file <- hyperSpec::read.ini(paste0(tmpdir, "/Witec_TrueMatch.txt"))
  ini_spc <- file[which(names(file) == "SpectrumData")]
  spc <- read_txt_Witec_TrueMatch(paste0(tmpdir, "/Witec_TrueMatch.txt"))

  for (s in seq_along(ini_spc)) {
    data <- unlist(ini_spc[s])
    data <- scan(text = data, quiet = TRUE)
    expect_equivalent(data[c(TRUE, FALSE)], spc@wavelength) # wavelength
    expect_equivalent(data[c(FALSE, TRUE)], spc@data[s, c("spc")]) # intensity
  }
})

test_that("spectra meta data is correctly parsed", {
  file <- read.ini(paste0(tmpdir, "/Witec_TrueMatch.txt"))
  ini_meta <- file[which(names(file) == "SampleMetaData")]
  spc <- read_txt_Witec_TrueMatch(paste0(tmpdir, "/Witec_TrueMatch.txt"))
  A <- names(file$SampleMetaData)
  A <- A[A != ""]
  A <- intersect(names(spc@data), A)
  expect_equivalent(A, gsub("SampleMetaData.", "", names(unlist(ini_meta[1]))))
  expect_equivalent(A, gsub("SampleMetaData.", "", names(unlist(ini_meta[2]))))
})

test_that("spectra header is correctly parsed", {
  file <- read.ini(paste0(tmpdir, "/Witec_TrueMatch.txt"))
  ini_meta <- file[which(names(file) == "SpectrumHeader")]
  spc <- read_txt_Witec_TrueMatch(paste0(tmpdir, "/Witec_TrueMatch.txt"))
  A <- names(file$SpectrumHeader)
  A <- A[A != ""]
  A <- intersect(names(spc@data), A)
  expect_equivalent(A, gsub("SpectrumHeader.", "", names(unlist(ini_meta[1]))))
  expect_equivalent(A, gsub("SpectrumHeader.", "", names(unlist(ini_meta[2]))))
})

test_that("users can specify extra data to keep", {
  file <- read.ini(paste0(tmpdir, "/Witec_TrueMatch.txt"))

  A <- c(names(file$SpectrumHeader), names(file$SampleMetaData))
  A <- A[A != ""]
  spc <- read_txt_Witec_TrueMatch(paste0(tmpdir, "/Witec_TrueMatch.txt"), keys_2header="all")
  expect_equal(sort(colnames(spc)), sort(c("filename", "spc", A)))

  spc <- read_txt_Witec_TrueMatch(paste0(tmpdir, "/Witec_TrueMatch.txt"), keys_2header="none")
  expect_equivalent(sort(colnames(spc)), c("filename", "spc"))

  A <- c(names(file$SpectrumHeader), names(file$SampleMetaData))
  A <- A[A != ""]
  spc <- read_txt_Witec_TrueMatch(paste0(tmpdir, "/Witec_TrueMatch.txt"), keys_2header=c("Length"))
  expect_equivalent(sort(colnames(spc)), sort(c("filename", "spc", A[which(A == "Length")])))
})

test_that("labels are correctly assigned to wavelength", {
  spc <- read_txt_Witec_TrueMatch(paste0(tmpdir, "/Witec_TrueMatch.txt"))
  expect_equivalent(labels(spc, ".wavelength"), "lambda/nm")
})

test_that("a valid hyperSpec object is returned", {
  spc_test <- read_txt_Witec_TrueMatch(paste0(tmpdir, "/Witec_TrueMatch.txt"))
  expect(chk.hy(spc_test), failure_message = "hyperSpec object was not returned")
})



