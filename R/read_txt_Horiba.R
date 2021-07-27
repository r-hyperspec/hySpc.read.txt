#' Read ASCII (`.txt`) files exported by Horiba's Labspec software (LabRAM spectrometers)
#'
#' `read_txt_Horiba_xy` reads maps, i.e. `.txt` files where the first two columns give x and y coordinates.
#'
#' @title Import Horiba Labspec exported ASCII files
#' @param file connection (file name and path) to the `.txt` file
#' @param cols,header,sep,row.names,check.names,... further parameters are
#'        handed over to [hyperSpec::read_txt_wide()]
#' @rdname read_txt_Horiba
#' @author C. Beleites
#' @return hyperSpec object
#' @concept io
#' @export
read_txt_Horiba <- function(file, cols = c(
                              spc = "I / a.u.",
                              .wavelength = expression(Delta * tilde(nu) / cm^-1)
                            ),
                            header = TRUE, sep = "\t", row.names = NULL,
                            check.names = FALSE, ...) {
  spc <- read_txt_wide(file,
    cols = cols,
    header = header, sep = sep, row.names = row.names,
    check.names = check.names, ...
  )

  ## consistent file import behaviour across import functions
  ## is already provided by read_txt_wide

  spc
}

#' @rdname read_txt_Horiba
#' @export
#'
#' @concept io
#'
read_txt_Horiba_xy <- function(file, ...) {
  read_txt_Horiba(
    file = file,
    cols = c(
      x = expression(x / mu * m),
      y = expression(y / mu * m),
      spc = "I / a.u.",
      .wavelength = expression(Delta * tilde(nu) / cm^-1)
    ),
    ...
  )
}

#' `read_txt_Horiba_t`  reads time series, i.e. .txt files with the time in the first column
#' @rdname read_txt_Horiba
#' @export
#'
#' @concept io
#'
read_txt_Horiba_t <- function(file, header = TRUE, sep = "\t", row.names = NULL,
                              check.names = FALSE, ...) {
  read_txt_Horiba(file,
    cols = c(
      t = "t / s",
      spc = "I / a.u.",
      .wavelength = expression(Delta * tilde(nu) / cm^-1)
    ),
    ...
  )
}
