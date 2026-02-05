# R/join.R ---------------------------------------------------------------

build_final_dataset <- function(data_corine, data_junta_results, habitats_clean, join_keys = c("lat","lon")) {
  data_comparacion <- data_corine %>%
    dplyr::inner_join(data_junta_results, by = join_keys, suffix = c("_corine", "_junta"))
  
  data_comparacion %>%
    dplyr::left_join(habitats_clean, by = join_keys)
}

# Selecciona la columna de hábitat a usar
get_habitat_colname <- function(df) {
  # Preferimos habitat_oficial (se genera así)
  pick_first_existing(df, c("habitat_oficial.x", "habitat_oficial"))
}
