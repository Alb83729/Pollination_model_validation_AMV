# R/utils.R --------------------------------------------------------------

assert_file <- function(path) {
  p <- normalizePath(path, winslash = "/", mustWork = FALSE)
  if (!file.exists(p)) stop("No existe el archivo: ", p, call. = FALSE)
  p
}

ensure_dir <- function(path) {
  if (!dir.exists(path)) dir.create(path, recursive = TRUE)
  invisible(TRUE)
}

msg_section <- function(title) {
  cat("\n==============================================================\n")
  cat(" ", title, "\n")
  cat("==============================================================\n")
}

# Devuelve la primera columna existente en df de una lista de candidatas
pick_first_existing <- function(df, candidates) {
  hit <- candidates[candidates %in% names(df)]
  if (length(hit) == 0) return(NULL)
  hit[1]
}
