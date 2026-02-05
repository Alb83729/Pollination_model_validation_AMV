cfg <- list(
  # Paths (ajusta)
  raster_path_tif  = "raster_CORINE/U2018_CLC2018_V2020_20u1.tif",
  legend_path_dbf  = "raster_CORINE/U2018_CLC2018_V2020_20u1.tif.vat.dbf",
  expert_table_csv = "Results/expert_table_JdA_V1.csv",
  validation_sites = "Results/fixed_Junta_validation_data.csv",
  output_dir       = "Results/",

  # Flags
  compute_validation_points = TRUE,
  compute_other_points      = FALSE,

  # Model params
  typical_foraging_dist_big   = 1500,
  typical_foraging_dist_small = 500,
  radius_factor               = 2,
  resolution_raster           = 100,

  # CRS fallback (si el TIFF viene sin CRS)
  fallback_epsg = 25830,

  # Parallel
  n_cores = max(1, parallel::detectCores() - 1)
)
