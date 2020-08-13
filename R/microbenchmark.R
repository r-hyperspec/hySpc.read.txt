# Load the microbenchmark library
library(hyperSpec)
library(microbenchmark)
library(readtext)


# Set file to be tested
file <- "/Users/erickoduniyi/Documents/Projects/open-source/hyperspec/hySpc.read.txt/tests/testthat/Witec_TrueMatch.txt"

# Switch between io packages
# Calculate performance with microbenchmark
res <- microbenchmark(
  read.table(file, header = TRUE, dec = ",", sep = "\t"),
  readLines(file),
  read.ini(file),
  readtext(file),
)

# Throw the Kitchen Sink at it
# read.txt.Witec("fileio/txt.Witec/Witec-Map_full.txt",  type = "map", hdr.label = TRUE, hdr.units = TRUE)
#
# read.txt.Witec("fileio/txt.Witec/Witec-Map_label.txt", type = "map", hdr.label = TRUE, hdr.units = FALSE)
#
# read.txt.Witec("fileio/txt.Witec/Witec-Map_unit.txt",  type = "map", hdr.label = FALSE, hdr.units = TRUE)
#
# read.txt.Witec("fileio/txt.Witec/Witec-Map_unit.txt",
#                type = "map", hdr.label = FALSE, hdr.units = TRUE,
#                points.per.line = 5
# )
#
# read.txt.Witec("fileio/txt.Witec/Witec-Map_no.txt", type = "map", hdr.label = FALSE, hdr.units = FALSE)
#
# read.txt.Witec("fileio/txt.Witec/Witec-Map_no.txt",
#                type = "map", hdr.label = FALSE, hdr.units = FALSE,
#                lines.per.image = 5
# )
#
# read.txt.Witec("fileio/txt.Witec/Witec-Map_no.txt",
#                type = "map", hdr.label = FALSE, hdr.units = FALSE,
#                points.per.line = 5, lines.per.image = 5
# )

# Release data
res

