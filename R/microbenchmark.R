# load the microbenchmark library
library(microbenchmark)

# io_function_1

# io_function_2

# io_function_3

# Aggregate benchmarking scores
scores <- cbind("", ..., "")


# Figure out what I want to test
# test_io(file, function_name)
test_io <- function(file, function_name) {

  # Switch between io packages
  # Calculate performance with io package
  # Output report of performance to a data frame

  # Create data frame to store the benchmarking results
  bm_df <- data.frame(package_name = 1, score = 1)
  bm_df$package_name <- function_name
  bm_df$score <- "Put report value here"

  # Release data frame
  bm_df
}
