# config.R ---------------------------------------------------------------

cfg <- list(
  corine_csv        = "Data/datosDel09062025/Lonsdorf_results_2025-06-09_20-24-40_rast125.csv",
  junta_results_csv = "Data/DeCorreoAlfon11062025/Junta_validation_data_outputs.csv",
  habitats_csv      = "Data/Junta_validation_data.csv",
  
  join_keys = c("lat", "lon"),
  
  # columnas lonsdorf tras el inner_join (sufijos)
  small_corine = "lonsdorf_small_corine",
  small_junta  = "lonsdorf_small_junta",
  big_corine   = "lonsdorf_big_corine",
  big_junta    = "lonsdorf_big_junta",
  
  # plots
  do_plots   = TRUE,
  save_plots = FALSE,
  out_dir    = "Results/plots_habitat_facet"
)
