# some internal helper functions

# checking file connection
.check.con <- function(headerfile, filex, filey, file) {
  ## check for valid data connection
  if (!missing(headerfile) && !file.exists(headerfile)) {
    stop("Header file not found!")
  }

  if (!missing(filex) && !file.exists(filex)) {
    stop("Wavelength axis file not found!")
  }

  if (!missing(filey) && !file.exists(filey)) {
    stop("Intensity file not found!")
  }

  if (!missing(file) && !file.exists(file)) {
    stop("Spectra file not found!")
  }
}

### checking for valid input
.check.valid <- function(type, hdr, points.per.line, lines.per.image) {
  ## check valid input
  type <- match.arg(type, c("single", "map"))

  if (type == "single" && !missing(points.per.line) && !is.null(points.per.line) && points.per.line != 1) { # TODO: better to prove for values > 1?
    warning("points.per.line != 1 given for single spectrum")
  }

  if (type == "single" && !missing(lines.per.image) && !is.null(lines.per.image) && lines.per.image != 1) { # TODO: see above
    warning("lines.per.image != 1 are defined for single spectrum")
  }

  if (type == "single" && !missing(hdr) && !is.null(hdr) && hdr["SizeY", ] != 1) {
    warning("header provides spatial information in y direction for single spectra")
  }

  return(type)
}

### parsing header information
.parse.hdr <- function(spc, hdr, hdr.label) {
  if (!missing(hdr) && !missing(hdr.label)) {
    hdr <- strsplit(hdr, "\t")

    if (length(hdr) == 2) {
      spc@data$spcname <- hdr[[1]][-1]
      labels(spc, ".wavelength") <- hdr[[2]] [1]
      labels(spc, "spc") <- unique(hdr[[2]] [-1])
    } else if (length(hdr) == 1 && hdr.label) {
      spc@data$spcname <- hdr[[1]][-1]
    } else {
      labels(spc, ".wavelength") <- hdr[[1]] [1]
      labels(spc, "spc") <- unique(hdr[[1]] [-1])
    }
  }

  if (!missing(hdr) && missing(hdr.label)) {
    spc@data$spcname <- hdr ["GraphName", ]
    if ("FileName" %in% rownames(hdr)) {
      spc@data$WIPname <- hdr ["FileName", ]
    }
    labels(spc, "spc") <- hdr ["DataUnit", ]
  }
  return(spc)
}

### parsing map information
.parse.xy <- function(spc, hdr, hdr.label, points.per.line, lines.per.image, ...) {

  ## set points.per.line and lines.per.image, if at least one is set unequal NULL
  if (xor(
    !missing(points.per.line) && !is.null(points.per.line),
    !missing(lines.per.image) && !is.null(lines.per.image)
  )) {
    if ((missing(points.per.line) || is.null(points.per.line)) &&
      !is.null(lines.per.image)) {
      points.per.line <- nrow(spc) / lines.per.image
    } else {
      lines.per.image <- nrow(spc) / points.per.line
    }
  } else if (!missing(points.per.line) && !missing(lines.per.image) &&
    is.null(points.per.line) && is.null(points.per.line) &&
    !missing(hdr.label) && hdr.label) { # TODO: only read, if not yet calculated?
    x <- sub("^.*\\(([[:digit:]]+)/[[:digit:]]+\\)$", "\\1", hdr[1])
    y <- sub("^.*\\([[:digit:]]+/([[:digit:]]+)\\)$", "\\1", hdr[1])
    points.per.line <- as.numeric(x) + 1
    lines.per.image <- as.numeric(y) + 1
  } else if ((missing(points.per.line) || missing(lines.per.image)) &&
    !missing(hdr) && missing(hdr.label)) { # TODO: only read, if not yet calculated?
    points.per.line <- as.numeric(hdr ["SizeX", ])
    lines.per.image <- as.numeric(hdr ["SizeY", ])
  } else if (is.null(points.per.line) && is.null(lines.per.image)) {
    warning("no spatial information provided")
    return(spc)
  }

  if (points.per.line * lines.per.image == nrow(spc)) {
    spc@data$x <- rep(seq_len(points.per.line), lines.per.image)
    spc@data$y <- rep(-seq_len(lines.per.image), each = points.per.line)
  } else {
    warning("number of spectra and number of points in map are not equal!")
  }

  return(spc)
}
