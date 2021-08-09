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
    "\nread_asc_PerkinElmer() is experimental. Package 'hySpc.read.txt' so far ",
    "has no test data for PerkinElmer .asc files. ",
    msg_open_issue_to_contribute_file()
  )

  ## find beginning of DATA section
  startDATA <- grep("DATA", content)

  if (length(startDATA) != 1L) {
    stop(
      "read_asc_PerkinElmer() so far can deal with single spectra files only. ",
      "It seems that your file contains more than one spectrum. ",
      msg_open_issue_to_contribute_file()
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

# FIXME: add unit tests, if possible.
