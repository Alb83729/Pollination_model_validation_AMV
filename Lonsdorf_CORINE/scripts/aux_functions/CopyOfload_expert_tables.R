# Asegúrate de que readr y dplyr están disponibles
# library(readr)
# library(dplyr)

CopyOfload_expert_tables <- function(path_file_expert_table){
  # Leer la tabla experta completa
  # Añadir show_col_types = FALSE para evitar mensajes en la consola
  expert_table <- readr::read_csv(path_file_expert_table, show_col_types = FALSE)

  # --- Idoneidad de Anidación (Nesting Suitability - NS) ---
  # Extraer información NS para polinizadores grandes
  NS_table_big <- expert_table %>%
    # Usar select() de dplyr para seleccionar y renombrar es más idiomático
    dplyr::select(codigo = code, nesting_suitability = nesting_suitability_big) %>%
    # Añadir columna de especie
    dplyr::mutate(species = "big") %>%
    # Asegurarse de que no haya NAs en columnas clave (opcional pero bueno)
    filter(!is.na(codigo) & !is.na(nesting_suitability))

  # Extraer información NS para polinizadores pequeños
  NS_table_small <- expert_table %>%
    dplyr::select(codigo = code, nesting_suitability = nesting_suitability_small) %>%
    dplyr::mutate(species = "small") %>%
    filter(!is.na(codigo) & !is.na(nesting_suitability))


  # --- Idoneidad Floral (Habitat/Floral Suitability - HF) ---
  # Extraer información HF por temporada y combinar

  # Método alternativo usando pivot_longer (más eficiente si tienes muchas temporadas)
  # Esto asume que tus columnas siguen el patrón:
  # foraging_suitability_TIPO_season_NUMERO
  # fl_resource_weight_season_NUMERO (peso es igual para big/small?)

  # Si el peso floral es el mismo para big/small, podríamos hacerlo así:
  # HF_table_long <- expert_table %>%
  #   select(code, starts_with("foraging_suitability_"), starts_with("fl_resource_weight_season_")) %>%
  #   tidyr::pivot_longer(
  #     cols = starts_with("foraging_suitability_"),
  #     names_to = c("species", "season"),
  #     names_pattern = "foraging_suitability_(.*)_season_(.*)",
  #     values_to = "foraging_suitability"
  #   ) %>%
  #   # Necesitaríamos unir o pivotar el peso floral de forma similar o asumir que es el mismo
  #   # Esto se complica si el peso depende solo de la temporada.
  #   # Vamos a mantener tu método original bind_rows que es más explícito

  # --- HF para polinizadores GRANDES (usando bind_rows) ---
  HF_table_big_season_1 <- expert_table %>%
    dplyr::select(codigo = code,
                  foraging_suitability = foraging_suitability_big_season_1,
                  fl_resource_weight = fl_resource_weight_season_1) %>%
    dplyr::mutate(species = "big", season = 1)

  HF_table_big_season_2 <- expert_table %>%
    dplyr::select(codigo = code,
                  foraging_suitability = foraging_suitability_big_season_2,
                  fl_resource_weight = fl_resource_weight_season_2) %>%
    dplyr::mutate(species = "big", season = 2)

  HF_table_big_season_3 <- expert_table %>%
    dplyr::select(codigo = code,
                  foraging_suitability = foraging_suitability_big_season_3,
                  fl_resource_weight = fl_resource_weight_season_3) %>%
    dplyr::mutate(species = "big", season = 3)

  HF_table_big_season_4 <- expert_table %>%
    dplyr::select(codigo = code,
                  foraging_suitability = foraging_suitability_big_season_4,
                  fl_resource_weight = fl_resource_weight_season_4) %>%
    dplyr::mutate(species = "big", season = 4)

  # Combinar temporadas para 'big'
  HF_table_big <- dplyr::bind_rows(HF_table_big_season_1,
                                   HF_table_big_season_2,
                                   HF_table_big_season_3,
                                   HF_table_big_season_4) %>%
    # Filtrar NAs que podrían venir de columnas no existentes o vacías en el CSV
    filter(!is.na(codigo) & !is.na(foraging_suitability) & !is.na(fl_resource_weight))

  # --- HF para polinizadores PEQUEÑOS (usando bind_rows) ---
  HF_table_small_season_1 <- expert_table %>%
    dplyr::select(codigo = code,
                  foraging_suitability = foraging_suitability_small_season_1,
                  fl_resource_weight = fl_resource_weight_season_1) %>%
    dplyr::mutate(species = "small", season = 1)

  HF_table_small_season_2 <- expert_table %>%
    dplyr::select(codigo = code,
                  foraging_suitability = foraging_suitability_small_season_2,
                  fl_resource_weight = fl_resource_weight_season_2) %>%
    dplyr::mutate(species = "small", season = 2)

  HF_table_small_season_3 <- expert_table %>%
    dplyr::select(codigo = code,
                  foraging_suitability = foraging_suitability_small_season_3,
                  fl_resource_weight = fl_resource_weight_season_3) %>%
    dplyr::mutate(species = "small", season = 3)

  HF_table_small_season_4 <- expert_table %>%
    dplyr::select(codigo = code,
                  foraging_suitability = foraging_suitability_small_season_4,
                  fl_resource_weight = fl_resource_weight_season_4) %>%
    dplyr::mutate(species = "small", season = 4)

  # Combinar temporadas para 'small'
  HF_table_small <- dplyr::bind_rows(HF_table_small_season_1,
                                     HF_table_small_season_2,
                                     HF_table_small_season_3,
                                     HF_table_small_season_4) %>%
    filter(!is.na(codigo) & !is.na(foraging_suitability) & !is.na(fl_resource_weight))


  # --- Devolver la lista NOMBRADA ---
  # Modificación clave aquí: asignar nombres a los elementos de la lista
  return(list(
    NS_big = NS_table_big,
    NS_small = NS_table_small,
    HF_big = HF_table_big,
    HF_small = HF_table_small
  ))
}
