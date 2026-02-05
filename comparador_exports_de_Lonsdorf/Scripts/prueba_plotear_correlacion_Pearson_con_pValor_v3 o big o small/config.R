# config.R ---------------------------------------------------------------

cfg <- list(
  lowres_csv  = "Data/datosDel09062025/Lonsdorf_results_2025-06-09_20-24-40_rast125.csv",
  highres_csv = "Data/DeCorreoAlfon11062025/Junta_validation_data_outputs.csv",
  
  # posibles columnas ID para alinear
  id_candidates = c("site_id", "Sitio", "id", "ID"),
  
  # columnas objetivo
  col_small = "Lonsdorf_small",
  col_big   = "Lonsdorf_big",
  
  # correlación
  cor_method      = "pearson",
  cor_alternative = "greater",
  
  # outputs
  save_plots = FALSE,
  out_dir    = "Results/plots_correlation"
)
