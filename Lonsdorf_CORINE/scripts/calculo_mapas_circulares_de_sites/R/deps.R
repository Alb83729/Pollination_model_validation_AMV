# R/deps.R ---------------------------------------------------------------

require_pkgs <- function(pkgs) {
  missing <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing) > 0) {
    stop(
      "Faltan paquetes R: ", paste(missing, collapse = ", "),
      "\nInstalalos con install.packages(c(", paste(sprintf('"%s"', missing), collapse = ", "), "))",
      call. = FALSE
    )
  }
}

load_pkgs <- function() {
  pkgs <- c(
    "sf", "raster", "dplyr", "foreign", "readr", "ggplot2", "terra",
    "foreach", "doParallel", "cowplot"
  )
  require_pkgs(pkgs)

  # Attach
  library(sf)
  library(raster)
  library(dplyr)
  library(foreign)
  library(readr)
  library(ggplot2)
  library(terra)
  library(foreach)
  library(doParallel)
  library(cowplot)

  invisible(TRUE)
}

source_aux <- function() {
  source("scripts/aux_functions/CopyOfload_expert_tables.R")
  source("scripts/aux_functions/CopyOffeatures_within_circle.R")
  source("scripts/aux_functions/CopyOfcreate_df_features_within_circle_sf.R")
  source("scripts/aux_functions/CopyOfdf_percentage4codigo_inside_circle.R")
  invisible(TRUE)
}
