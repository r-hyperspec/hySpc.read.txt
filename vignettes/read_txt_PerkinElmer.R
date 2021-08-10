
# Function -------------------------------------------------------------------

#' Read PerkinElmer file (ASCII/txt)
#'
#' @param file Path to file or several files
#' @param ... Passed to [base::scan()].
#' @param label Labels.
#'
#'
#' @return [hyperSpec][hyperSpec::hyperSpec-class()] object.
#'
#'
#' @export
#'
read_txt_PerkinElmer <- function(file = stop("filenames needed"),
                                 ...,
                                 label = list()) {


  ##  set default labels
  label <- modifyList(
    list(
      .wavelength = expression(lambda / nm),
      spc = expression(I[fl] / "a.u.")
    ),
    label
  )

  if (length(file) == 0) {
    warning("No files found.")
    return(new("hyperSpec"))
  }

  ## read the first file
  buffer <- matrix(scan(file[1], ...), ncol = 2, byrow = TRUE)

  ## first column gives the wavelength vector
  wavelength <- buffer[, 1]

  ## preallocate the spectra matrix:
  ##  one row per file x as many columns as the first file has
  spc <- matrix(ncol = nrow(buffer), nrow = length(file))

  ## the first file's data goes into the first row
  spc[1, ] <- buffer[, 2]

  ## now read the remaining files
  for (f in seq(along = file)[-1]) {
    buffer <- matrix(scan(file[f], ...), ncol = 2, byrow = TRUE)

    ## check whether they have the same wavelength axis
    if (!all.equal(buffer[, 1], wavelength)) {
      stop(paste(file[f], "has different wavelength axis."))
    }

    spc[f, ] <- buffer[, 2]
  }

  ## make the hyperSpec object
  spc <- new("hyperSpec", wavelength = wavelength, spc = spc, label = label)

  ## consistent file import behavior across import functions
  .spc_io_postprocess_optional(spc, file)
}


# Unit tests -----------------------------------------------------------------

# FIXME: add unit tests
