# Carga dplyr explícitamente si no estás seguro que esté cargado al definir la función
# library(dplyr) # Mejor cargarlo al inicio del script principal

#Intento del 17/04/2025:
CopyOfcreate_df_features_within_circle_sf <- function(raster_df) { # Eliminado resolution_raster si no se usa

  # Comprobamos que hay columna de "codigo" y "cell"
  stopifnot(all(c("codigo", "cell") %in% colnames(raster_df)))

  # Calcular proporción por celda y código
  # --- INICIO CAMBIO ---
  df_features <- raster_df %>%
    # Los siguientes group_by/summarise/mutate parecen resultar en weight=1 siempre
    # si raster_df viene de un raster remuestreado con 'ngb'.
    # Revisa si esta lógica es realmente la que necesitas.
    # Si solo necesitas asegurar las columnas y un peso de 1:
    # dplyr::mutate(weight = 1.0) %>%
    # dplyr::select(cell, x, y, codigo, weight)

    # Manteniendo tu lógica original pero especificando dplyr::select:
    group_by(cell, x, y, codigo) %>%
    summarise(pixel_count = n(), .groups = "drop") %>%
    group_by(cell) %>%
    mutate(weight = pixel_count / sum(pixel_count)) %>% # Esto probablemente da weight=1
    ungroup() %>%
    # Usar dplyr::select explícitamente:
    dplyr::select(cell, x, y, codigo, weight)
  # --- FIN CAMBIO ---

  # Devolver como tibble es opcional si ya es un tibble por dplyr
  return(df_features) # O return(as_tibble(df_features)) si quieres asegurarlo
}
