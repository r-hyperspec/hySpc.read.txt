#' Title
#'
#' @param file
#'
#' @return
#' @export
read_txt_Witec_ASCII <- function(file) {
}

test(read_txt_Witec_ASCII) <- function() {
  context("read_txt_Witec_ASCII")

  test_that("example file", {
    f <- readLines("Example_ASCII_export.txt")
  })
}
