# R/deps.R ---------------------------------------------------------------

require_pkgs <- function(pkgs) {
  missing <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing) > 0) {
    stop(
      "Faltan paquetes: ", paste(missing, collapse = ", "),
      "\nInstala con: install.packages(c(", paste(sprintf('"%s"', missing), collapse = ", "), "))",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

load_pkgs <- function() {
  require_pkgs(c("dplyr", "ggplot2", "rlang"))
  library(dplyr)
  library(ggplot2)
  library(rlang)
  invisible(TRUE)
}
