# R/normalize.R ----------------------------------------------------------

preparar_tabla <- function(df) {
  df <- df %>% dplyr::rename_with(tolower)
  
  if ("latitude" %in% names(df)) df <- df %>% dplyr::rename(lat = latitude)
  else if ("latitud" %in% names(df)) df <- df %>% dplyr::rename(lat = latitud)
  
  if ("longitude" %in% names(df)) df <- df %>% dplyr::rename(lon = longitude)
  else if ("longitud" %in% names(df)) df <- df %>% dplyr::rename(lon = longitud)
  
  if (any(grepl("habit", names(df)))) {
    col_hab <- names(df)[grepl("habit", names(df))][1]
    df <- df %>% dplyr::rename(habitat_oficial = !!rlang::sym(col_hab))
  }
  
  df
}

clean_habitats <- function(habitats_df) {
  habitats_df %>%
    dplyr::select(lat, lon, habitat_oficial) %>%
    dplyr::mutate(
      lat_temp = lat,
      lat = ifelse(lat_temp < 0, lon, lat_temp),
      lon = ifelse(lat_temp < 0, lat_temp, lon)
    ) %>%
    dplyr::select(-lat_temp) %>%
    dplyr::distinct(lat, lon, .keep_all = TRUE)
}
