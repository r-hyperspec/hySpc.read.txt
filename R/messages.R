
msg_open_issue_and_add_file <- function(pkg = "hySpc.read.txt") {
  paste0(
    "Please open an issue at: \n",
    packageDescription(pkg)$BugReport,
    " and include including \n",
    "- the output of `sessionInfo()` and \n",
    "- an example data file."
  )
}

msg_open_issue_to_contribute_file <- function(pkg = "hySpc.read.txt") {
  paste0(
    "To help improving '", pkg, "', ",
    "please consider submitting your spectra file as an example ",
    "(possibly included in a ZIP archive) by opening an issue at \n",
    packageDescription(pkg)$BugReports
  )
}


# Unit tests -----------------------------------------------------------------

hySpc.testthat::test(msg_open_issue_and_add_file) <- function() {
  local_edition(3)

  test_that("msg_open_issue_and_add_file() works", {

    expect_silent(msg <- msg_open_issue_and_add_file())
    expect_match(msg, "hySpc.read.txt")
    expect_match(msg, "r-hyperspec")
    expect_match(msg, "file")
  })

  test_that("msg_open_issue_to_contribute_file() works", {

    expect_silent(msg <- msg_open_issue_to_contribute_file())
    expect_match(msg, "hySpc.read.txt")
    expect_match(msg, "r-hyperspec")
    expect_match(msg, "file")
  })
}
