# config.R ---------------------------------------------------------------

cfg <- list(
  results_csv = "resources/from_raster_CORINE/raster_125/Lonsdorf_results_2025-06-09_20-24-40.csv",

  # Qué CCAA dibujar (Andalucía = "01" en mapSpain)
  ccaa_code = "01",

  # Qué variable pintar
  value_col = "Lonsdorf_small",

  # estética
  point_size  = 2,
  point_alpha = 0.5,
  title = "Lonsdorf's service map (small bees)",
  xlab  = "Longitude",
  ylab  = "Latitude"
)
