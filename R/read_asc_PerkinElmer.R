# Function -------------------------------------------------------------------

#' Read single spectrum form PerkinElmer file (ASCII/txt)
#'
#' **EXPERIMENTAL FUNCTION.**\cr
#' Import a single spectrum from a file in PerkinElmer's ASCII format.
#' This function is experimental.
#'
#' @param file Path  or connection to file.
#' @param ... Further argument handed to [hyperSpec::read_txt_long()].
#'
#' @return `hyperSpec` object.
#'
#' @concept io
#'
#' @importFrom utils packageDescription
#' @export
#'
read_asc_PerkinElmer <- function(file = stop("filename or connection needed"),
                                 ...) {
  content <- readLines(con = file)

  message(
    "read_asc_PerkinElmer() is experimental, ",
    "'hyperSpec' so far has no test data for PE .asc files. ",
    "Please consider submitting your spectrum in an enhancement request to ",
    packageDescription("hyperSpec")$BugReports,
    " in order to help the development of hyperSpec."
  )

  ## find beginning of DATA section
  startDATA <- grep("DATA", content)

  if (length(startDATA) != 1L) {
    stop(
      "read_asc_PerkinElmer() so far can deal with single spectra files only.",
      " Please file an enhancement request at",
      packageDescription("hyperSpec")$BugReports,
      " with your file as an example or contact the maintainer (",
      maintainer("hyperSpec"), ")."
    )
  }

  ## Spectra values are stored
  content <- content[-seq_len(startDATA)]

  spc <- hyperSpec::read_txt_long(
    textConnection(content),
    header = FALSE,
    sep = "\t",
    ...
  )
  spc$filename <- NULL # not meaningful due to textConnection use

  ## consistent file import behaviour across import functions
  .spc_io_postprocess_optional(spc, file)
}


# Unit tests -----------------------------------------------------------------

hySpc.testthat::test(read_asc_PerkinElmer) <- function() {
  context("read_asc_PerkinElmer")

  path <- system.file("extdata/txt.PerkinElmer",package = "hySpc.read.txt")
  flu_file <- paste0(path, c("/flu1.txt", "/flu2.txt",
                             "/flu3.txt", "/flu4.txt", "/flu5.txt", "/flu6.txt"))

  test_that("PerkinElmer .txt labels are correct", {
    for(flu in flu_file){
      expect_message(spc <- read_asc_PerkinElmer(flu))

      expect_true(is.expression(spc@label$spc))
      expect_true(is.expression(spc@label$.wavelength))
      expect_equal(spc@label$filename, "filename")
    }
  })

  test_that("PerkinElmer .txt spectra data", {
    for(flu in flu_file){
      expect_message(spc <- read_asc_PerkinElmer(flu))

      expect_equal(colnames(spc@data$spc),
                   c("405",  "405.5","406",  "406.5","407",  "407.5","408",  "408.5","409",  "409.5","410",  "410.5","411",  "411.5","412",  "412.5",
                     "413",  "413.5","414",  "414.5","415",  "415.5","416",  "416.5","417",  "417.5","418",  "418.5","419",  "419.5","420",  "420.5",
                     "421",  "421.5","422",  "422.5","423",  "423.5","424",  "424.5","425",  "425.5","426",  "426.5","427",  "427.5","428",  "428.5",
                     "429",  "429.5","430",  "430.5","431",  "431.5","432",  "432.5","433",  "433.5","434",  "434.5","435",  "435.5","436",  "436.5",
                     "437",  "437.5","438",  "438.5","439",  "439.5","440",  "440.5","441",  "441.5","442",  "442.5","443",  "443.5","444",  "444.5",
                     "445",  "445.5","446",  "446.5","447",  "447.5","448",  "448.5","449",  "449.5","450",  "450.5","451",  "451.5","452",  "452.5",
                     "453",  "453.5","454",  "454.5","455",  "455.5","456",  "456.5","457",  "457.5","458",  "458.5","459",  "459.5","460",  "460.5",
                     "461",  "461.5","462",  "462.5","463",  "463.5","464",  "464.5","465",  "465.5","466",  "466.5","467",  "467.5","468",  "468.5",
                     "469",  "469.5","470",  "470.5","471",  "471.5","472",  "472.5","473",  "473.5","474",  "474.5","475",  "475.5","476",  "476.5",
                     "477",  "477.5","478",  "478.5","479",  "479.5","480",  "480.5","481",  "481.5","482",  "482.5","483",  "483.5","484",  "484.5",
                     "485",  "485.5","486",  "486.5","487",  "487.5","488",  "488.5","489",  "489.5","490",  "490.5","491",  "491.5","492",  "492.5",
                     "493",  "493.5","494",  "494.5","495"))
    }
  })

  test_that("PerkinElmer .txt wavelength",{
    for(flu in flu_file){
      expect_message(spc <- read_asc_PerkinElmer(flu))

      expect_equal(length(spc@wavelength), 181)
    }
  })
}
