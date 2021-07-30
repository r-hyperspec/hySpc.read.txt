# Function -------------------------------------------------------------------

#' Read INI files
#'
#' `read_ini()` reads ini files of the form
#' \cr\cr
#' `[section]`\cr
#' `key = value`
#' \cr\cr
#' into a list.
#'
#' `read_ini()` sanitizes the element names and tries to convert scalars and
#'  comma separated numeric vectors to numeric.
#'
#' @rdname read-ini
#'
#' @param con connection or file name.
#' @param skip number of lines to skip before first `[section]` starts.
#' @param encoding see [base::readLines()].
#'
#' @author C. Beleites
#'
#' @return A list with one element per section in the .ini file, each
#'         containing a list with elements for the key-value-pairs.
#'
#' @keywords IO file
#'
#' @export

read_ini <- function(con = stop("Connection con needed."),
                     skip = NULL,
                     encoding = "unknown") {
  Lines <- readLines(con, encoding = encoding)
  ## remove leading lines, if they are not a section
  if (!is.null(skip)) {
    Lines <- Lines[-seq_len(skip)]
  }

  sections <- grep("[[].*[]]", Lines)

  content <- Lines[-sections]
  ini <- as.list(gsub("^.*=[[:blank:]]*", "", content)) # removes blanks behind equal sign
  names(ini) <- sanitize_name(gsub("[[:blank:]]*=.*$", "", content)) # see above: removes in front of equal sign

  # try converting to numeric
  tmp <- lapply(ini, function(x) strsplit(x, ",")[[1]])
  tmp <- suppressWarnings(lapply(tmp, as.numeric))
  numbers <- !sapply(tmp, function(x) any(is.na(x)))
  ini[numbers] <- tmp[numbers]

  tmp <- rep.int(seq_along(sections), diff(c(sections, length(Lines) + 1)) - 1)
  ini <- split(ini, tmp)

  sections <- Lines[sections]
  sections <- sanitize_name(gsub("^.(.*).$", "\\1", sections))
  names(ini) <- sections

  ini
}

# Helper functions -----------------------------------------------------------

sanitize_name <- function(name) {
  gsub("[^a-zA-Z0-9._]", ".", name)
}


# Unit tests -----------------------------------------------------------------

# FIXME: add unit tests
