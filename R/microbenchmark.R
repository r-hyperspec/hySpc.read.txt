# Load the microbenchmark library
library(hyperSpec)
library(microbenchmark)
# library(readtext)

# Set file to be tested
file <- "/Users/erickoduniyi/Documents/Projects/open-source/hyperspec/hySpc.read.txt/tests/testthat/Witec_TrueMatch.txt"

# Switch between io packages
# Calculate performance with microbenchmark
microbenchmark(
  read.table(file, header = TRUE, dec = ",", sep = "\t"),
  readLines(file),
  read.ini(file),
  # readtext(file),
  default = read.ini(file)
)

