##' Run unit tests
##'
##' If [hySpc.testthat::unittest()] is available, run the unit tests and
##' display the results.
##'
##' @rdname unittests
##' @return Invisibly returns a data.frame with the test results
##'
##' @author Claudia Beleites
##'
##' @keywords programming utilities
##' @export
##' @examples
##'
##' hySpc.read.Witec.unittest ()
##'
# hySpc.read.Witec.unittest <- function() {
#   if (!requireNamespace("hySpc.testthat", quietly = TRUE)) {
#     warning("Package hySpc.testthat required to run the unit tests.")
#     return(NA)
#   }
#   if (!"package:hySpc.testthat" %in% search()) {
#     attachNamespace("hySpc.testthat")
#   }
#
#   tests <- eapply(env = getNamespace("hySpc.read.Witec"), FUN = get.test, all.names = TRUE)
#   tests <- tests [!sapply(tests, is.null)]
#
#   reporter <- SummaryReporter$new()
#   lister <- ListReporter$new()
#   reporter <- MultiReporter$new(reporters = list(reporter, lister))
#
#   with_reporter(reporter = reporter, start_end_reporter = TRUE, {
#     for (t in seq_along(tests)) {
#       lister$start_file(names(tests [t]))
#       tests [[t]]()
#     }
#     get_reporter()$.end_context()
#   })
#
#   invisible(lister$get_results())
# }
#
# ##' @noRd
# {
#   `.test<-` <- function(f, value) {
#     attr(f, "test") <- value
#     f
#   }
# }
#
# ##' get test that is attached to object as "test" attribute
# ##' @noRd
# get.test <- function(object) {
#   attr(object, "test")
# }
#
# # internal test data set
# {
#   .testdata <- flu[, , min ~ 410]
#   .testdata[[6, ]] <- NA
#   .testdata[[3:4,, 406~407]] <- NA
#   .testdata$region <- factor (c("a", "a", "b", "b", "a", "a"))
# }
