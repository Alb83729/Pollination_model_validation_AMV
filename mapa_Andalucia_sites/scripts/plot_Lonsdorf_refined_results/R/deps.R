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
  pkgs <- c("ggplot2", "readr", "mapSpain", "sf", "wesanderson")
  require_pkgs(pkgs)

  library(ggplot2)
  library(readr)
  library(mapSpain)
  library(sf)
  library(wesanderson)

  invisible(TRUE)
}
