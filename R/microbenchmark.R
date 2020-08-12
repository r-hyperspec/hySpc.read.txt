# Load the microbenchmark library
library(microbenchmark)

# Set file to be tested
file <- "/Users/erickoduniyi/Documents/Projects/open-source/hyperspec/hySpc.read.txt/tests/testthat/Witec_TrueMatch.txt"

# Switch between io packages
# Calculate performance with microbenchmark
res <- microbenchmark(
  read.ini(file),
  read.table(file, colClasses),
  read.table(file, header = FALSE, sep="", dec="."),
  readtext(),
)

# Release data
res

