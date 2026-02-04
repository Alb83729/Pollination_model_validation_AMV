# config.R ---------------------------------------------------------------

cfg <- list(
  # ---- Paths
  raster_path_tif   = "raster_CORINE/U2018_CLC2018_V2020_20u1.tif",
  legend_path_dbf   = "raster_CORINE/U2018_CLC2018_V2020_20u1.tif.vat.dbf",
  validation_csv    = "data/validation_JdA_info_visitation_rate.csv",
  validation_sites  = "Results/fixed_Junta_validation_data.csv",
  expert_table_csv  = "Results/expert_table_JdA_V1.csv",

  # ---- Model parameters
  typical_foraging_dist_big   = 1500,
  typical_foraging_dist_small = 500,

  # ---- Spatial parameters
  buffer_radius_m     = 2000,  # radio del buffer visible (m)
  resolution_raster_m = 100,   # resolución objetivo (m)

  # ---- Execution flags
  compute_validation_points = TRUE,
  compute_other_points      = FALSE,
  do_plots                  = TRUE,
  export_corine_df          = TRUE,

  # ---- Parallel
  n_cores = 5,

  # ---- Debug / run scope
  site_i = 5
)
