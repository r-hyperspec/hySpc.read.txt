
msg_open_issue_and_add_file <- function(pkg = "hySpc.read.txt") {
  paste0(
    "Please open an issue at: \n",
    packageDescription(pkg)$BugReport,
    " and include including \n",
    "- the output of `sessionInfo()` and \n",
    "- an example data file."
  )
}

# Unit tests -----------------------------------------------------------------

hySpc.testthat::test(msg_report_issue_and_add_file) <- function() {
  local_edition(3)

  test_that("msg_open_issue_and_add_file() works", {

    expect_silent(msg <- msg_report_issue_and_add_file())
    expect_match(msg, "hySpc.read.txt")
    expect_match(msg, "r-hyperspec")
    expect_match(msg, "file")
  })
}
