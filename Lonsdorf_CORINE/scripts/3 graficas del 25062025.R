# --- 0. Cargar Librerías ---
# Cargar raster ANTES que dplyr es una buena práctica
library(raster)           # Operaciones con datos ráster (paquete más antiguo)
library(sp)               # Dependencia de raster
library(sf)               # Trabaja con datos espaciales (vectoriales)
library(dplyr)            # Manipulación de datos
library(terra)            # Operaciones con datos ráster (paquete más nuevo y rápido)
library(foreign)          # Lectura de archivos DBF
library(readr)            # Lectura rápida de archivos CSV/planos
library(ggplot2)          # Visualización de datos
library(tictoc)           # Para medir tiempos (opcional)

# Verificar e instalar foreach si es necesario
if (!requireNamespace("foreach", quietly = TRUE)) {
  install.packages("foreach")
}
library(foreach)

# Verificar e instalar doParallel si es necesario
if (!requireNamespace("doParallel", quietly = TRUE)) {
  install.packages("doParallel")
}
library(doParallel)
library(iterators)        # Dependencia de doParallel
library(parallel)         # Dependencia de doParallel

## NUEVO ## - Librerías para visualización de mapas
if (!requireNamespace("tidyterra", quietly = TRUE)) install.packages("tidyterra")
library(tidyterra)
if (!requireNamespace("viridis", quietly = TRUE)) install.packages("viridis")
library(viridis)


# --- 1. Cargar Funciones Auxiliares ---
# Asegúrate de que estas rutas son correctas desde la ubicación del script principal
# Asumiendo que están en una carpeta 'aux_functions' relativa al script principal
source("aux_functions/CopyOfload_expert_tables.R")
source("aux_functions/CopyOffeatures_within_circle.R") # Usa st_bbox internamente ahora
source("aux_functions/CopyOfcreate_df_features_within_circle_sf.R") # Usa dplyr::select, no necesita resolution_raster
# source("aux_functions/CopyOfdf_percentage4codigo_inside_circle.R") # No parece usarse

# --- 2. Definir Parámetros ---
# Parámetros espaciales para el modelo Lonsdorf
typical_foraging_dist_big   <- 1500 # Distancia típica de forrajeo (metros) - polinizadores grandes
typical_foraging_dist_small <- 500  # Distancia típica de forrajeo (metros) - polinizadores pequeños
radius_factor               <- 2    # Factor para definir el radio de búsqueda (2 * dist_forrajeo)
resolution_raster           <- 100  # Resolución espacial deseada para el análisis (metros)

# Parámetros de ejecución
compute_validation_points <- TRUE  # ¿Procesar puntos de validación?
compute_other_points      <- FALSE # ¿Procesar otros puntos?

## NUEVO ## - Parámetros para la generación de mapas de ejemplo
generar_mapas_ejemplo <- TRUE # Poner en TRUE para generar los 3 mapas para un sitio de ejemplo
sitio_ejemplo_idx     <- 1     # Qué sitio de la lista usar para los mapas (ej: el primero)
radio_visualizacion   <- 2500  # Radio en metros para crear el mapa (2500m -> 5x5 km)

# --- 3. Cargar Datos de Entrada ---
# Cargar el ráster CORINE Land Cover (Ajusta la ruta si es necesario)
raster_path <- "../raster_CORINE/U2018_CLC2018_V2020_20u1.tif"
if (!file.exists(raster_path)) stop("El archivo ráster TIFF no existe en la ruta especificada: ", raster_path)
CORINE_raster <- raster::raster(raster_path)

# Cargar la leyenda del ráster (Ajusta la ruta si es necesario)
legend_path <- "../raster_CORINE/U2018_CLC2018_V2020_20u1.tif.vat.dbf"
if (!file.exists(legend_path)) stop("El archivo DBF de la leyenda no existe en la ruta especificada: ", legend_path)
CORINE_legend <- foreign::read.dbf(legend_path, as.is = TRUE)

## CORRECCIÓN 1: Bloque para renombrar columnas de la leyenda de forma robusta
# Primero, identifica la columna de valor numérico (ID del píxel)
if (!"Value" %in% names(CORINE_legend) && "VALUE" %in% names(CORINE_legend)) {
  CORINE_legend <- CORINE_legend %>% rename(Value = VALUE)
} else if (!"Value" %in% names(CORINE_legend)) {
  stop("La columna 'Value' o 'VALUE' no se encuentra en la leyenda.")
}

# Segundo, identifica la columna de CÓDIGO CORINE (ej: "111", "243")
# Comprueba si existe 'CLC_CODE' o 'CODE'. Si no, ajusta el nombre a lo que veas en tu archivo.
if ("CLC_CODE" %in% names(CORINE_legend)) {
  CORINE_legend <- CORINE_legend %>% rename(Codigo = CLC_CODE)
} else if ("CODE" %in% names(CORINE_legend)) {
  CORINE_legend <- CORINE_legend %>% rename(Codigo = CODE)
} else {
  warning("No se encontró 'CLC_CODE' ni 'CODE'. Asegúrate de que tus tablas expertas usan el valor numérico ('Value') para los joins.")
  # Si tus tablas expertas usan el valor numérico, esto es seguro:
  CORINE_legend$Codigo <- as.character(CORINE_legend$Value)
}

# Tercero, identifica la columna de ETIQUETA (descripción)
# Comprueba 'LABEL3', 'LABEL2', 'LABEL1', etc. y renómbrala a 'Label'.
if ("LABEL3" %in% names(CORINE_legend)) {
  CORINE_legend <- CORINE_legend %>% rename(Label = LABEL3)
} else if ("LABEL2" %in% names(CORINE_legend)) {
  CORINE_legend <- CORINE_legend %>% rename(Label = LABEL2)
} else if ("LABEL1" %in% names(CORINE_legend)) {
  CORINE_legend <- CORINE_legend %>% rename(Label = LABEL1)
} else {
  warning("No se encontró una columna de etiqueta (LABEL). La leyenda del mapa de cobertura no tendrá descripciones.")
  CORINE_legend$Label <- paste("Código", CORINE_legend$Codigo)
}


# Verificar y asignar CRS al ráster si es necesario (¡VERIFICA EL EPSG!)
if (is.na(crs(CORINE_raster))) {
  warning("El ráster CORINE no tiene CRS definido. Asignando ETRS89 / UTM zone 30N (EPSG:25830) como ejemplo. ¡VERIFICAR!")
  crs(CORINE_raster) <- "+init=epsg:25830"
}
print(paste("CRS del ráster CORINE:", crs(CORINE_raster)))
if (is.na(st_crs(CORINE_raster))) stop("Fallo al asignar/verificar CRS del ráster.")

# Cargar las tablas expertas (Ajusta la ruta si es necesario)
path_file_expert_table <- "../Results/expert_table_JdA_V1.csv"
if (!file.exists(path_file_expert_table)) stop("El archivo de tablas expertas no existe: ", path_file_expert_table)
list_expert_tables <- CopyOfload_expert_tables(path_file_expert_table)
NS_table_big   <- list_expert_tables[["NS_big"]]
NS_table_small <- list_expert_tables[["NS_small"]]
HF_table_big   <- list_expert_tables[["HF_big"]]
HF_table_small <- list_expert_tables[["HF_small"]]
if (!exists("NS_table_big") || !is.data.frame(NS_table_big)) stop("Error al cargar NS_table_big")

# Cargar las coordenadas de los sitios a procesar (Ajusta rutas si es necesario)
selected_sites_coordinates <- NULL
path_output_dir <- "../Results/"
if (!dir.exists(path_output_dir)) dir.create(path_output_dir, recursive = TRUE)

if(compute_validation_points){
  path_file_validation_coordinates <- "../Results/fixed_Junta_validation_data.csv"
  if (!file.exists(path_file_validation_coordinates)) stop("Archivo de coordenadas de validación no encontrado: ", path_file_validation_coordinates)
  validation_sites_coordinates <- readr::read_csv(path_file_validation_coordinates, show_col_types = FALSE) %>%
    dplyr::select(any_of(c("Sitio", "Ano", "Latitud", "Longitud"))) %>%
    dplyr::rename(site_id = Sitio, latitude = Latitud, longitude = Longitud, refYear = Ano) %>%
    dplyr::mutate(latitude = as.numeric(latitude), longitude = as.numeric(longitude))
  selected_sites_coordinates <- validation_sites_coordinates
} else if(compute_other_points){
  # Cargar otros puntos...
}

if (is.null(selected_sites_coordinates) || nrow(selected_sites_coordinates) == 0) {
  stop("No se cargaron coordenadas de sitios para procesar.")
}

selected_sites_coordinates$Lonsdorf_big   <- NA_real_
selected_sites_coordinates$Lonsdorf_small <- NA_real_

# --- 3.5. ## NUEVO ## - GENERACIÓN DE MAPAS DE EJEMPLO PARA UN SITIO ---
if (generar_mapas_ejemplo) {
  message(paste("\n--- Iniciando generación de mapas de ejemplo para el sitio",
                sitio_ejemplo_idx, "---"))

  path_map_output_dir <- file.path(path_output_dir, "mapas_ejemplo")
  if (!dir.exists(path_map_output_dir)) dir.create(path_map_output_dir, recursive = TRUE)

  sitio_ejemplo <- selected_sites_coordinates[sitio_ejemplo_idx, ]
  message(paste("Sitio de ejemplo:", sitio_ejemplo$site_id,
                "(Lon:", round(sitio_ejemplo$longitude, 4),
                ", Lat:", round(sitio_ejemplo$latitude, 4), ")"))

  CORINE_terra <- terra::rast(CORINE_raster)

  punto_sitio_ejemplo <- sf::st_point(c(sitio_ejemplo$longitude, sitio_ejemplo$latitude)) %>%
    st_sfc(crs = 4326) %>%
    st_transform(crs = st_crs(CORINE_terra))

  buffer_circular <- st_buffer(punto_sitio_ejemplo, dist = radio_visualizacion)
  raster_sitio_recortado <- terra::crop(CORINE_terra, buffer_circular, mask = TRUE)

  message("Generando Mapa 1: Cobertura del Suelo...")

  raster_df <- as.data.frame(raster_sitio_recortado, xy = TRUE, na.rm = TRUE)
  names(raster_df)[3] <- "Value"

  ## --- CORRECCIÓN CLAVE ---
  # El ráster se convierte a data.frame con 'Value' como factor. Lo convertimos a numérico.
  # Usamos as.character() primero para evitar problemas de conversión directa de factor a número.
  raster_df$Value <- as.numeric(as.character(raster_df$Value))
  ## --- FIN DE LA CORRECCIÓN ---

  # Ahora, nos aseguramos de que la leyenda también tenga la columna 'Value' como numérica.
  CORINE_legend$Value <- as.numeric(CORINE_legend$Value)

  raster_df <- raster_df %>%
    left_join(CORINE_legend %>% select(Value, Label), by = "Value") %>%
    mutate(Label = factor(Label))

  # ... el resto del bloque continúa igual ...

  mapa_landcover <- ggplot() +
    geom_raster(data = raster_df, aes(x = x, y = y, fill = Label)) +
    scale_fill_viridis_d(na.value = "transparent", name = "Uso del Suelo", drop=FALSE) + # drop=FALSE evita que se pierdan niveles de factor
    coord_equal() +
    labs(
      title = "Mapa de Cobertura del Suelo (CORINE)",
      subtitle = paste("Área circular (radio 2.5 km) alrededor del sitio:", sitio_ejemplo$site_id),
      x = "Coordenada X (UTM)",
      y = "Coordenada Y (UTM)"
    ) +
    theme_minimal()

  ggsave(
    filename = file.path(path_map_output_dir, paste0("1_mapa_cobertura_suelo_", sitio_ejemplo$site_id, ".png")),
    plot = mapa_landcover,
    width = 10, height = 8, dpi = 300
  )
  message("Mapa de cobertura guardado.")

  message("Generando Mapa 2: Idoneidad de Anidación...")

  # Los códigos en el ráster son ahora los 'Value' numéricos, no los 'Codigo' de texto.
  # Debemos usar la columna 'Value' de la leyenda para reclasificar.
  ns_reclass_table <- NS_table_big %>%
    # Unimos con la leyenda para obtener el 'Value' numérico correspondiente a cada 'codigo' de texto.
    left_join(CORINE_legend %>% select(Codigo, Value), by = c("codigo" = "Codigo")) %>%
    filter(!is.na(Value)) %>%
    group_by(Value) %>%
    summarise(ns_valor = max(nesting_suitability, na.rm = TRUE), .groups = 'drop')

  reclass_matrix_ns <- ns_reclass_table %>% as.matrix()

  # Importante: el ráster original recortado tiene los valores originales de CORINE.
  # Usamos el ráster 'U2018_CLC2018_V2020_20u1' que contiene los valores que coinciden con 'Value'
  raster_ns_sitio <- terra::classify(raster_sitio_recortado, rcl = reclass_matrix_ns, others = 0)
  names(raster_ns_sitio) <- "nesting_suitability"

  mapa_nesting <- ggplot() +
    geom_spatraster(data = raster_ns_sitio) +
    scale_fill_viridis_c(option = "cividis", na.value = "transparent", name = "Idoneidad\n(0 a 1)") +
    coord_equal() +
    labs(
      title = "Mapa de Recursos de Anidación",
      subtitle = "Basado en la idoneidad máxima para polinizadores grandes",
      x = "Coordenada X (UTM)",
      y = "Coordenada Y (UTM)"
    ) +
    theme_minimal()

  ggsave(
    filename = file.path(path_map_output_dir, paste0("2_mapa_anidacion_", sitio_ejemplo$site_id, ".png")),
    plot = mapa_nesting,
    width = 10, height = 8, dpi = 300
  )
  message("Mapa de anidación guardado.")

  message("Generando Mapa 3: Idoneidad de Recursos Florales...")

  # Repetimos la misma lógica para los recursos florales
  hf_reclass_table <- HF_table_big %>%
    left_join(CORINE_legend %>% select(Codigo, Value), by = c("codigo" = "Codigo")) %>%
    filter(!is.na(Value)) %>%
    group_by(Value) %>%
    summarise(hf_valor = max(foraging_suitability, na.rm = TRUE), .groups = 'drop')

  reclass_matrix_hf <- hf_reclass_table %>% as.matrix()

  raster_hf_sitio <- terra::classify(raster_sitio_recortado, rcl = reclass_matrix_hf, others = 0)
  names(raster_hf_sitio) <- "floral_suitability"

  mapa_floral <- ggplot() +
    geom_spatraster(data = raster_hf_sitio) +
    scale_fill_viridis_c(option = "inferno", na.value = "transparent", name = "Idoneidad\n(0 a 1)") +
    coord_equal() +
    labs(
      title = "Mapa de Recursos Florales",
      subtitle = "Basado en la idoneidad máxima para polinizadores grandes",
      x = "Coordenada X (UTM)",
      y = "Coordenada Y (UTM)"
    ) +
    theme_minimal()

  ggsave(
    filename = file.path(path_map_output_dir, paste0("3_mapa_floral_", sitio_ejemplo$site_id, ".png")),
    plot = mapa_floral,
    width = 10, height = 8, dpi = 300
  )
  message("Mapa floral guardado.")
  message(paste("--- Mapas de ejemplo generados con éxito en la carpeta:", path_map_output_dir, "---\n"))

} # Fin del bloque de generación de mapas

# --- 4. Bloque de ejemplo/depuración para un solo sitio (Opcional - Comentado) ---
# ...

# --- 5. Preparar Paralelización ---
nCores <- detectCores() - 1
if (nCores < 1) nCores <- 1
registerDoParallel(cores = nCores)
message(paste("Registrando", nCores, "núcleos para paralelización."))


# --- 6. Bucle Principal: Calcular Índice Lonsdorf para cada sitio ---
message(paste("Iniciando cálculo del índice Lonsdorf para", nrow(selected_sites_coordinates), "sitios..."))
tictoc::tic("Procesamiento Total Lonsdorf")

# Obtener timestamp para el nombre del archivo de resultados
estimation_time <- Sys.time()
formatted_time <- format(estimation_time, "%Y-%m-%d_%H-%M-%S")
results_filename <- file.path(path_output_dir, paste0("Lonsdorf_results_", formatted_time, ".csv"))

# CRS del ráster (objeto sf)
crs_raster_sf <- sf::st_crs(CORINE_raster)

for(site_i in 1:nrow(selected_sites_coordinates)){

  # ... (EL RESTO DEL SCRIPT CONTINÚA EXACTAMENTE IGUAL DESDE AQUÍ) ...

  tictoc::tic(paste("Sitio", site_i))

  site_id_current <- selected_sites_coordinates$site_id[site_i]
  latitud_i       <- selected_sites_coordinates$latitude[site_i] %>% as.numeric()
  longitud_i      <- selected_sites_coordinates$longitude[site_i] %>% as.numeric()

  message(paste0("\n--- Procesando Sitio ", site_i, "/", nrow(selected_sites_coordinates),
                 " (ID: ", site_id_current, ", Lon: ", round(longitud_i, 4), ", Lat: ", round(latitud_i, 4), ") ---"))

  # --- 6.1 Calcular punto proyectado del centro del sitio ---
  if (is.na(latitud_i) || is.na(longitud_i) || !is.finite(latitud_i) || !is.finite(longitud_i)) {
    warning(paste("Coordenadas inválidas para sitio", site_i, "- Saltando..."))
    selected_sites_coordinates$Lonsdorf_big[site_i] <- NA
    selected_sites_coordinates$Lonsdorf_small[site_i] <- NA
    next
  }

  point_ll_site <- sf::st_point(c(longitud_i, latitud_i)) %>% st_sfc(crs = 4326)

  point_proj_site <- tryCatch({
    sf::st_transform(point_ll_site, crs = crs_raster_sf)
  }, error = function(e){
    warning(paste("Error transformando CRS para sitio", site_i, ":", e$message, "- Saltando..."))
    return(NULL)
  })

  if(is.null(point_proj_site)){
    selected_sites_coordinates$Lonsdorf_big[site_i] <- NA
    selected_sites_coordinates$Lonsdorf_small[site_i] <- NA
    next
  }

  site_center_coords <- st_coordinates(point_proj_site)
  x_centro_site <- site_center_coords[1, "X"]
  y_centro_site <- site_center_coords[1, "Y"]


  # --- 6.2 Extraer características y crear dataframe base ---
  circle_radius <- radius_factor * typical_foraging_dist_big

  features_dentro_circulo_df <- CopyOffeatures_within_circle(CORINE_raster,
                                                             latitud_i, longitud_i,
                                                             circle_radius,
                                                             resolution_raster)
  if(nrow(features_dentro_circulo_df) == 0){
    warning(paste("No se encontraron características válidas para sitio", site_i, "(posible no solapamiento). - Saltando..."))
    selected_sites_coordinates$Lonsdorf_big[site_i] <- NA
    selected_sites_coordinates$Lonsdorf_small[site_i] <- NA
    tictoc::toc()
    next
  }

  df_features_within_circle_sf <- CopyOfcreate_df_features_within_circle_sf(features_dentro_circulo_df)


  # --- 6.3 Añadir información del sitio y calcular distancia al centro ---
  df_features_within_circle_sf <- df_features_within_circle_sf %>%
    dplyr::mutate(
      latitud = latitud_i,
      longitud = longitud_i,
      x_centro_site = x_centro_site,
      y_centro_site = y_centro_site,
      distancia_centro = sqrt((x - x_centro_site)^2 + (y - y_centro_site)^2)
    )

  # --- 6.4 Preparar datos base para Lonsdorf (GRANDES) ---
  message("Preparando datos para Lonsdorf (Grandes)...")
  coordinates4big <- df_features_within_circle_sf %>%
    dplyr::filter(distancia_centro <= radius_factor * typical_foraging_dist_big) %>%
    dplyr::mutate(exp_decay = exp(-distancia_centro / typical_foraging_dist_big))

  sum_exp_distances_big <- coordinates4big %>%
    dplyr::distinct(cell, .keep_all = TRUE) %>%
    pull(exp_decay) %>%
    sum(na.rm=TRUE)

  if (sum_exp_distances_big > 0) {
    coordinates4big <- coordinates4big %>%
      dplyr::mutate(relative_exp_decay = exp_decay / sum_exp_distances_big)
  } else {
    coordinates4big <- coordinates4big %>% dplyr::mutate(relative_exp_decay = 0)
    warning(paste("Sitio", site_i, ": sum_exp_distances_big es cero o negativo."))
  }

  # --- 6.5 Preparar datos base para Lonsdorf (PEQUEÑOS) ---
  message("Preparando datos para Lonsdorf (Pequeños)...")
  coordinates4small <- df_features_within_circle_sf %>%
    dplyr::filter(distancia_centro <= radius_factor * typical_foraging_dist_small) %>%
    dplyr::mutate(exp_decay = exp(-distancia_centro / typical_foraging_dist_small))

  sum_exp_distances_small <- coordinates4small %>%
    dplyr::distinct(cell, .keep_all = TRUE) %>%
    pull(exp_decay) %>%
    sum(na.rm=TRUE)

  if (sum_exp_distances_small > 0) {
    coordinates4small <- coordinates4small %>%
      dplyr::mutate(relative_exp_decay = exp_decay / sum_exp_distances_small)
  } else {
    coordinates4small <- coordinates4small %>% dplyr::mutate(relative_exp_decay = 0)
    warning(paste("Sitio", site_i, ": sum_exp_distances_small es cero o negativo."))
  }


  # --- 6.6 Calcular Idoneidad de Anidación (HN) por píxel y especie ---
  message("Calculando HN...")
  # Grandes
  HN_pixel_big <- coordinates4big %>%
    dplyr::left_join(NS_table_big %>% select(codigo, species, nesting_suitability), by = "codigo") %>%
    filter(!is.na(nesting_suitability)) %>%
    dplyr::mutate(HN_LC_pixel = weight * nesting_suitability) %>%
    group_by(cell, x, y, latitud, longitud, species) %>%
    summarise(HN_pixel = sum(HN_LC_pixel, na.rm = TRUE), .groups = 'drop') %>%
    ungroup()

  # Pequeños
  HN_pixel_small <- coordinates4small %>%
    dplyr::left_join(NS_table_small %>% select(codigo, species, nesting_suitability), by = "codigo") %>%
    filter(!is.na(nesting_suitability)) %>%
    dplyr::mutate(HN_LC_pixel = weight * nesting_suitability) %>%
    group_by(cell, x, y, latitud, longitud, species) %>%
    summarise(HN_pixel = sum(HN_LC_pixel, na.rm = TRUE), .groups = 'drop') %>%
    ungroup()


  # --- 6.7 Calcular Idoneidad Floral (HF) por píxel (paralelizado) ---
  message("Calculando HF (paralelizado)...")

  # --- 6.7.1 HF para polinizadores GRANDES ---
  HF_pixel_coords_big <- coordinates4big %>%
    dplyr::select(cell, x, y, relative_exp_decay) %>%
    dplyr::distinct(cell, .keep_all = TRUE)

  if (nrow(HF_pixel_coords_big) > 0) {
    results_HF_big <- foreach(i = 1:nrow(HF_pixel_coords_big), .packages = c("dplyr"), .combine = 'c') %dopar% {
      # ... (código HF sin cambios)
      current_nest_cell <- HF_pixel_coords_big[i, ]
      x_nest <- current_nest_cell$x
      y_nest <- current_nest_cell$y
      resources_near_nest <- df_features_within_circle_sf %>%
        dplyr::mutate(dist_from_nest = sqrt((x - x_nest)^2 + (y - y_nest)^2)) %>%
        dplyr::filter(dist_from_nest <= radius_factor * typical_foraging_dist_big) %>%
        dplyr::mutate(exp_decay_from_nest = exp(-dist_from_nest / typical_foraging_dist_big))
      sum_exp_decay_from_nest <- sum(resources_near_nest$exp_decay_from_nest, na.rm = TRUE)
      if (sum_exp_decay_from_nest > 0) {
        resources_near_nest <- resources_near_nest %>%
          dplyr::mutate(relative_exp_decay_from_nest = exp_decay_from_nest / sum_exp_decay_from_nest)
      } else {
        resources_near_nest <- resources_near_nest %>% dplyr::mutate(relative_exp_decay_from_nest = 0)
      }
      HF_value <- resources_near_nest %>%
        dplyr::left_join(HF_table_big, by = "codigo") %>%
        filter(!is.na(foraging_suitability) & !is.na(relative_exp_decay_from_nest)) %>%
        dplyr::mutate(product_pixel = foraging_suitability * weight * relative_exp_decay_from_nest) %>%
        group_by(species, season, fl_resource_weight) %>%
        summarise(sum_product_pixel = sum(product_pixel, na.rm = TRUE), .groups = 'drop') %>%
        mutate(product_season_i = sum_product_pixel * fl_resource_weight) %>%
        group_by(species) %>%
        summarise(sum_product_season_i = sum(product_season_i, na.rm = TRUE), .groups = 'drop') %>%
        summarise(HF_pixel_final = sum(sum_product_season_i, na.rm = TRUE)) %>%
        pull(HF_pixel_final)
      if(length(HF_value) == 0 || is.na(HF_value)) { return(0) } else { return(HF_value) }
    }
    HF_pixel_coords_big$HF_pixel <- results_HF_big
  } else {
    warning(paste("Sitio", site_i, ": No hay celdas candidatas para HF_big."))
    HF_pixel_coords_big <- data.frame(cell=integer(), x=numeric(), y=numeric(), relative_exp_decay=numeric(), HF_pixel=numeric())
  }


  # --- 6.7.2 HF para polinizadores PEQUEÑOS ---
  HF_pixel_coords_small <- coordinates4small %>%
    dplyr::select(cell, x, y, relative_exp_decay) %>%
    dplyr::distinct(cell, .keep_all = TRUE)

  if (nrow(HF_pixel_coords_small) > 0) {
    results_HF_small <- foreach(i = 1:nrow(HF_pixel_coords_small), .packages = c("dplyr"), .combine = 'c') %dopar% {
      # ... (código HF sin cambios)
      current_nest_cell <- HF_pixel_coords_small[i, ]
      x_nest <- current_nest_cell$x
      y_nest <- current_nest_cell$y
      resources_near_nest <- df_features_within_circle_sf %>%
        dplyr::mutate(dist_from_nest = sqrt((x - x_nest)^2 + (y - y_nest)^2)) %>%
        dplyr::filter(dist_from_nest <= radius_factor * typical_foraging_dist_small) %>%
        dplyr::mutate(exp_decay_from_nest = exp(-dist_from_nest / typical_foraging_dist_small))
      sum_exp_decay_from_nest <- sum(resources_near_nest$exp_decay_from_nest, na.rm = TRUE)
      if (sum_exp_decay_from_nest > 0) {
        resources_near_nest <- resources_near_nest %>%
          dplyr::mutate(relative_exp_decay_from_nest = exp_decay_from_nest / sum_exp_decay_from_nest)
      } else {
        resources_near_nest <- resources_near_nest %>% dplyr::mutate(relative_exp_decay_from_nest = 0)
      }
      HF_value <- resources_near_nest %>%
        dplyr::left_join(HF_table_small, by = "codigo") %>%
        filter(!is.na(foraging_suitability) & !is.na(relative_exp_decay_from_nest)) %>%
        dplyr::mutate(product_pixel = foraging_suitability * weight * relative_exp_decay_from_nest) %>%
        group_by(species, season, fl_resource_weight) %>%
        summarise(sum_product_pixel = sum(product_pixel, na.rm = TRUE), .groups = 'drop') %>%
        mutate(product_season_i = sum_product_pixel * fl_resource_weight) %>%
        group_by(species) %>%
        summarise(sum_product_season_i = sum(product_season_i, na.rm = TRUE), .groups = 'drop') %>%
        summarise(HF_pixel_final = sum(sum_product_season_i, na.rm = TRUE)) %>%
        pull(HF_pixel_final)
      if(length(HF_value) == 0 || is.na(HF_value)) { return(0) } else { return(HF_value) }
    }
    HF_pixel_coords_small$HF_pixel <- results_HF_small
  } else {
    warning(paste("Sitio", site_i, ": No hay celdas candidatas para HF_small."))
    HF_pixel_coords_small <- data.frame(cell=integer(), x=numeric(), y=numeric(), relative_exp_decay=numeric(), HF_pixel=numeric())
  }


  # --- 6.8 Calcular Índice de Abundancia Potencial (POS) ---
  message("Calculando POS...")
  # --- 6.8.1 POS para polinizadores GRANDES ---
  POS_big_df <- HN_pixel_big %>%
    dplyr::left_join(HF_pixel_coords_big %>% dplyr::select(cell, relative_exp_decay, HF_pixel), by = "cell") %>%
    filter(!is.na(HF_pixel) & !is.na(relative_exp_decay) & !is.na(HN_pixel)) %>%
    dplyr::mutate(PO_pixel_species_decay = HN_pixel * HF_pixel * relative_exp_decay)

  POS_big_final <- sum(POS_big_df$PO_pixel_species_decay, na.rm = TRUE)

  # --- 6.8.2 POS para polinizadores PEQUEÑOS ---
  POS_small_df <- HN_pixel_small %>%
    dplyr::left_join(HF_pixel_coords_small %>% dplyr::select(cell, relative_exp_decay, HF_pixel), by = "cell") %>%
    filter(!is.na(HF_pixel) & !is.na(relative_exp_decay) & !is.na(HN_pixel)) %>%
    dplyr::mutate(PO_pixel_species_decay = HN_pixel * HF_pixel * relative_exp_decay)

  POS_small_final <- sum(POS_small_df$PO_pixel_species_decay, na.rm = TRUE)

  # --- 6.9 Guardar Resultados para este sitio ---
  selected_sites_coordinates$Lonsdorf_big[site_i]   <- POS_big_final
  selected_sites_coordinates$Lonsdorf_small[site_i] <- POS_small_final

  message(paste("Resultados Sitio", site_i, ": Lonsdorf_big =", round(POS_big_final, 4),
                ", Lonsdorf_small =", round(POS_small_final, 4)))

  tryCatch({
    readr::write_csv(selected_sites_coordinates, results_filename)
  }, error = function(e){
    warning(paste("Sitio", site_i, ": No se pudieron guardar los resultados intermedios. Error:", e$message))
  })

  tictoc::toc()

} # Fin del bucle for sobre los sitios

# --- 7. Finalizar y Limpiar ---
tictoc::toc()

stopImplicitCluster()
message("Clúster paralelo detenido.")
message(paste("Procesamiento completado. Resultados finales guardados en:", results_filename))
print(head(selected_sites_coordinates))

# --- Fin del Script ---
