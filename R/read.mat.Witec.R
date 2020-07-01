#' Import Matlab file exported by Witec Project software
#'
#' @param file file or connection from which to import
#'
#' @export
#' @importFrom utils maintainer
#' @importClassesFrom hyperSpec hyperSpec
#' @importFrom methods new
#' @importFrom utils packageDescription
#' @import hyperSpec
read.mat.Witec <- function(file = stop("filename or connection needed")) {
  if (!requireNamespace("R.matlab")) {
    stop("package 'R.matlab' needed.")
  }

  data <- R.matlab::readMat(file)

  if (length(data) > 1L) {
    stop(
      "Matlab file contains more than 1 object. This should not happen.\n",
      "If it is nevertheless a WITec exported .mat file, ",
      "please open an issue at ",
      packageDescription("hySpc.read.Witec")$BugReports,
      " with\n",
      "- output of `sessionInfo ()` and\n",
      "- an example file"
    )
  }
  spcname <- names(data)
  data <- data[[1]]

  spc <- new("hyperSpec", spc = data$data)

  spc$spcname <- spcname

  ## consistent file import behaviour across import functions
  .fileio.optional(spc, file)
}
