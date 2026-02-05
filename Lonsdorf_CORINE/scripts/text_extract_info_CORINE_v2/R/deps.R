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
  pkgs <- c("raster","sp","sf","dplyr","terra","foreign","readr","ggplot2","tictoc","foreach","doParallel","parallel")
  require_pkgs(pkgs)

  library(raster)
  library(sp)
  library(sf)
  library(dplyr)
  library(terra)
  library(foreign)
  library(readr)
  library(ggplot2)
  library(tictoc)
  library(foreach)
  library(doParallel)
  library(parallel)

  invisible(TRUE)
}

source_aux <- function() {
  source("scripts/aux_functions/CopyOfload_expert_tables.R")
  source("scripts/aux_functions/CopyOffeatures_within_circle.R")
  source("scripts/aux_functions/CopyOfcreate_df_features_within_circle_sf.R")
  invisible(TRUE)
}
