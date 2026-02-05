# R/utils.R --------------------------------------------------------------

assert_file <- function(path) {
  p <- normalizePath(path, winslash = "/", mustWork = FALSE)
  if (!file.exists(p)) stop("No existe el archivo: ", p, call. = FALSE)
  p
}
