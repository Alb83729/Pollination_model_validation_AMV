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

# --- 3. Cargar Datos de Entrada ---
# Cargar el ráster CORINE Land Cover (Ajusta la ruta si es necesario)
raster_path <- "../raster_CORINE/U2018_CLC2018_V2020_20u1.tif"
if (!file.exists(raster_path)) stop("El archivo ráster TIFF no existe en la ruta especificada: ", raster_path)
CORINE_raster <- raster::raster(raster_path)

# Cargar la leyenda del ráster (Ajusta la ruta si es necesario)
legend_path <- "../raster_CORINE/U2018_CLC2018_V2020_20u1.tif.vat.dbf"
if (!file.exists(legend_path)) stop("El archivo DBF de la leyenda no existe en la ruta especificada: ", legend_path)
# Usar read.dbf es más robusto para DBF simples si st_read da problemas
CORINE_legend <- foreign::read.dbf(legend_path, as.is = TRUE)
# Renombrar columna de valor si es necesario (ej. si se llama 'VALUE' o similar)
if (!"Value" %in% names(CORINE_legend) && "VALUE" %in% names(CORINE_legend)) {
  CORINE_legend <- CORINE_legend %>% rename(Value = VALUE)
} else if (!"Value" %in% names(CORINE_legend)) {
  # Intenta adivinar la primera columna numérica si 'Value' o 'VALUE' no existen
  num_cols <- sapply(CORINE_legend, is.numeric)
  if(any(num_cols)){
    first_num_col <- names(CORINE_legend)[which(num_cols)[1]]
    warning(paste("Columna 'Value' no encontrada, usando la primera columna numérica:", first_num_col))
    CORINE_legend <- CORINE_legend %>% rename(Value = !!sym(first_num_col))
  } else {
    stop("No se encontró una columna 'Value' o numérica adecuada en la leyenda.")
  }
}


# Verificar y asignar CRS al ráster si es necesario (¡VERIFICA EL EPSG!)
if (is.na(crs(CORINE_raster))) {
  warning("El ráster CORINE no tiene CRS definido. Asignando ETRS89 / UTM zone 30N (EPSG:25830) como ejemplo. ¡VERIFICAR!")
  crs(CORINE_raster) <- "+init=epsg:25830" # ETRS89 / UTM zone 30N (Península Ibérica)
  # crs(CORINE_raster) <- "+init=epsg:32630" # WGS84 / UTM zone 30N
  # crs(CORINE_raster) <- "+init=epsg:4083" # REGCAN95 / UTM zone 28N (Canarias Oeste)
  # crs(CORINE_raster) <- "+init=epsg:4084" # REGCAN95 / UTM zone 27N (Canarias Este)
}
print(paste("CRS del ráster CORINE:", crs(CORINE_raster)))
if (is.na(st_crs(CORINE_raster))) stop("Fallo al asignar/verificar CRS del ráster.") # Doble check

# Plotear el ráster (opcional)
# plot(CORINE_raster)

# Cargar las tablas expertas (Ajusta la ruta si es necesario)
path_file_expert_table <- "../Results/expert_table_JdA_V1.csv"
if (!file.exists(path_file_expert_table)) stop("El archivo de tablas expertas no existe: ", path_file_expert_table)
list_expert_tables <- CopyOfload_expert_tables(path_file_expert_table) # Asume que esta función devuelve una lista nombrada
NS_table_big   <- list_expert_tables[["NS_big"]]   # Nesting Suitability - Grandes
NS_table_small <- list_expert_tables[["NS_small"]] # Nesting Suitability - Pequeños
HF_table_big   <- list_expert_tables[["HF_big"]]   # Habitat/Floral Suitability - Grandes
HF_table_small <- list_expert_tables[["HF_small"]] # Habitat/Floral Suitability - Pequeños
if (!exists("NS_table_big") || !is.data.frame(NS_table_big)) stop("Error al cargar NS_table_big")
# Asegurarse que las tablas HF tengan columna 'codigo' o 'code' para el join
# Renombrar si es necesario, ej: HF_table_big <- HF_table_big %>% rename(codigo = code_column_name)


# Cargar las coordenadas de los sitios a procesar (Ajusta rutas si es necesario)
selected_sites_coordinates <- NULL
path_output_dir <- "../Results/" # Directorio para guardar resultados
if (!dir.exists(path_output_dir)) dir.create(path_output_dir, recursive = TRUE)

if(compute_validation_points){
  path_file_validation_coordinates <- "../Results/fixed_Junta_validation_data.csv"
  if (!file.exists(path_file_validation_coordinates)) stop("Archivo de coordenadas de validación no encontrado: ", path_file_validation_coordinates)
  validation_sites_coordinates <- readr::read_csv(path_file_validation_coordinates, show_col_types = FALSE) %>%
    # Asegúrate que estas columnas existen en tu CSV
    dplyr::select(any_of(c("Sitio", "Ano", "Latitud", "Longitud"))) %>%
    dplyr::rename(site_id = Sitio, latitude = Latitud, longitude = Longitud, refYear = Ano) %>%
    dplyr::mutate(latitude = as.numeric(latitude), longitude = as.numeric(longitude))

  selected_sites_coordinates <- validation_sites_coordinates

} else if(compute_other_points){
  path_file_coordinates <- "../data/sites_AAP2.csv" # Asegúrate que este archivo existe y tiene las columnas
  if (!file.exists(path_file_coordinates)) stop("Archivo de otras coordenadas no encontrado: ", path_file_coordinates)
  other_sites_coordinates <- readr::read_csv(path_file_coordinates, show_col_types = FALSE) %>%
    dplyr::select(any_of(c("site_id", "latitude", "longitude", "refYear"))) %>%
    dplyr::mutate(latitude = as.numeric(latitude), longitude = as.numeric(longitude))

  selected_sites_coordinates <- other_sites_coordinates
}

if (is.null(selected_sites_coordinates) || nrow(selected_sites_coordinates) == 0) {
  stop("No se cargaron coordenadas de sitios para procesar.")
}
if (!all(c("site_id", "latitude", "longitude") %in% names(selected_sites_coordinates))) {
  stop("La tabla de coordenadas no contiene las columnas necesarias: site_id, latitude, longitude")
}


# Añadir columnas para los resultados Lonsdorf
selected_sites_coordinates$Lonsdorf_big   <- NA_real_
selected_sites_coordinates$Lonsdorf_small <- NA_real_


# --- 4. Bloque de ejemplo/depuración para un solo sitio (Opcional - Comentado) ---
# run_single_site_example <- FALSE
# if (run_single_site_example && nrow(selected_sites_coordinates) > 0) {
#   # ... (código del bloque de ejemplo aquí, si lo necesitas para depurar) ...
#   # Asegúrate de que no interfiera con las variables del bucle principal
# }


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

  tictoc::tic(paste("Sitio", site_i))

  site_id_current <- selected_sites_coordinates$site_id[site_i]
  latitud_i       <- selected_sites_coordinates$latitude[site_i] %>% as.numeric()
  longitud_i      <- selected_sites_coordinates$longitude[site_i] %>% as.numeric()

  message(paste0("\n--- Procesando Sitio ", site_i, "/", nrow(selected_sites_coordinates),
                 " (ID: ", site_id_current, ", Lon: ", round(longitud_i, 4), ", Lat: ", round(latitud_i, 4), ") ---"))

  # --- 6.1 Calcular punto proyectado del centro del sitio ---
  # Verificar coordenadas antes de usarlas
  if (is.na(latitud_i) || is.na(longitud_i) || !is.finite(latitud_i) || !is.finite(longitud_i)) {
    warning(paste("Coordenadas inválidas para sitio", site_i, "- Saltando..."))
    selected_sites_coordinates$Lonsdorf_big[site_i] <- NA
    selected_sites_coordinates$Lonsdorf_small[site_i] <- NA
    next
  }

  point_ll_site <- sf::st_point(c(longitud_i, latitud_i)) %>% st_sfc(crs = 4326)

  # Proyectar el punto al CRS del ráster
  point_proj_site <- tryCatch({
    sf::st_transform(point_ll_site, crs = crs_raster_sf)
  }, error = function(e){
    warning(paste("Error transformando CRS para sitio", site_i, ":", e$message, "- Saltando..."))
    return(NULL)
  })

  if(is.null(point_proj_site)){
    selected_sites_coordinates$Lonsdorf_big[site_i] <- NA
    selected_sites_coordinates$Lonsdorf_small[site_i] <- NA
    # Guardar intermedio si falla mucho
    # readr::write_csv(selected_sites_coordinates, results_filename)
    next
  }

  # Coordenadas proyectadas del centro del sitio
  site_center_coords <- st_coordinates(point_proj_site)
  x_centro_site <- site_center_coords[1, "X"]
  y_centro_site <- site_center_coords[1, "Y"]


  # --- 6.2 Extraer características y crear dataframe base ---
  # Definir radio para la extracción de características (ej. 2 * dist max forrajeo)
  circle_radius <- radius_factor * typical_foraging_dist_big

  # Llamar a la función auxiliar para obtener el dataframe de características
  # Pasamos lat/lon porque la función los usa para crear el punto inicial
  features_dentro_circulo_df <- CopyOffeatures_within_circle(CORINE_raster,
                                                             latitud_i, longitud_i,
                                                             circle_radius,
                                                             resolution_raster) # resolution_raster SÍ es necesario aquí

  # Si la función devuelve un dataframe vacío (ej. no solapamiento), saltar
  if(nrow(features_dentro_circulo_df) == 0){
    warning(paste("No se encontraron características válidas para sitio", site_i, "(posible no solapamiento). - Saltando..."))
    selected_sites_coordinates$Lonsdorf_big[site_i] <- NA
    selected_sites_coordinates$Lonsdorf_small[site_i] <- NA
    # readr::write_csv(selected_sites_coordinates, results_filename)
    tictoc::toc() # Detener tic para este sitio
    next
  }

  # Llamar a la segunda función auxiliar (que ahora solo toma el df y añade weight=1)
  df_features_within_circle_sf <- CopyOfcreate_df_features_within_circle_sf(features_dentro_circulo_df)


  # --- 6.3 Añadir información del sitio y calcular distancia al centro ---
  df_features_within_circle_sf <- df_features_within_circle_sf %>%
    dplyr::mutate(
      latitud = latitud_i,
      longitud = longitud_i,
      x_centro_site = x_centro_site, # Centro real proyectado
      y_centro_site = y_centro_site, # Centro real proyectado
      # Distancia euclidiana desde el centro de cada celda al centro REAL del sitio
      distancia_centro = sqrt((x - x_centro_site)^2 + (y - y_centro_site)^2)
    )

  # --- 6.4 Preparar datos base para Lonsdorf (GRANDES) ---
  message("Preparando datos para Lonsdorf (Grandes)...")
  coordinates4big <- df_features_within_circle_sf %>%
    # Filtrar celdas dentro del rango de influencia (desde el centro del sitio)
    dplyr::filter(distancia_centro <= radius_factor * typical_foraging_dist_big) %>%
    # Calcular decaimiento exponencial desde el centro REAL del sitio
    dplyr::mutate(exp_decay = exp(-distancia_centro / typical_foraging_dist_big))

  # Calcular suma de decaimientos para normalización (contando cada celda única una vez)
  sum_exp_distances_big <- coordinates4big %>%
    dplyr::distinct(cell, .keep_all = TRUE) %>% # Usar 'cell' que es el ID único de la celda remuestreada
    pull(exp_decay) %>%
    sum(na.rm=TRUE)

  # Calcular decaimiento relativo (ponderación de cada celda en el índice final del sitio)
  if (sum_exp_distances_big > 0) {
    coordinates4big <- coordinates4big %>%
      dplyr::mutate(relative_exp_decay = exp_decay / sum_exp_distances_big)
  } else {
    coordinates4big <- coordinates4big %>% dplyr::mutate(relative_exp_decay = 0)
    warning(paste("Sitio", site_i, ": sum_exp_distances_big es cero o negativo."))
  }

  # --- 6.5 Preparar datos base para Lonsdorf (PEQUEÑOS) ---
  message("Preparando datos para Lonsdorf (Pequeños)...")
  # --- ADAPTAR PARA SMALL --- (Lógica similar a 'big')
  coordinates4small <- df_features_within_circle_sf %>%
    dplyr::filter(distancia_centro <= radius_factor * typical_foraging_dist_small) %>% # Usar dist small
    dplyr::mutate(exp_decay = exp(-distancia_centro / typical_foraging_dist_small)) # Usar dist small

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

  if (site_i == 1) {
    message("--- DEBUG HN (Sitio 1) ---")
    message("Códigos únicos en coordinates4big:")
    print(unique(coordinates4big$codigo))
    message("Códigos únicos en NS_table_big:")
    print(unique(NS_table_big$codigo))
    # Ver qué pasa en el join
    join_check_hn <- dplyr::left_join(coordinates4big %>% select(cell, codigo) %>% distinct(),
                                      NS_table_big %>% select(codigo, nesting_suitability),
                                      by = "codigo")
    message("Filas después del join HN (antes de filtrar NA):")
    print(nrow(join_check_hn))
    message("Resumen de nesting_suitability después del join:")
    print(summary(join_check_hn$nesting_suitability))
    message("Número de NAs en nesting_suitability después del join:")
    print(sum(is.na(join_check_hn$nesting_suitability)))
    message("--- FIN DEBUG HN (Sitio 1) ---")
  }

  # Grandes
  # Asegurarse que NS_table_big tenga 'codigo' y 'nesting_suitability'
  HN_pixel_big <- coordinates4big %>%
    # rename(code = codigo) # No necesario si la columna ya se llama 'codigo'
    dplyr::left_join(NS_table_big %>% select(codigo, species, nesting_suitability), by = "codigo") %>%
    filter(!is.na(nesting_suitability)) %>% # Ignorar códigos sin valor de anidación
    # HN = peso_pixel(=1) * idon_anidación
    dplyr::mutate(HN_LC_pixel = weight * nesting_suitability) %>% # weight es 1
    # Agrupar por celda y especie para sumar HN si hubiera múltiples entradas (no debería con ngb)
    group_by(cell, x, y, latitud, longitud, species) %>% # Agrupamos por celda única
    summarise(HN_pixel = sum(HN_LC_pixel, na.rm = TRUE), .groups = 'drop') %>%
    ungroup()

  # Pequeños
  # --- ADAPTAR PARA SMALL --- (Lógica similar a 'big')
  HN_pixel_small <- coordinates4small %>%
    dplyr::left_join(NS_table_small %>% select(codigo, species, nesting_suitability), by = "codigo") %>%
    filter(!is.na(nesting_suitability)) %>%
    dplyr::mutate(HN_LC_pixel = weight * nesting_suitability) %>% # weight es 1
    group_by(cell, x, y, latitud, longitud, species) %>%
    summarise(HN_pixel = sum(HN_LC_pixel, na.rm = TRUE), .groups = 'drop') %>%
    ungroup()

  # Sanity check (opcional): HN_pixel no debería ser > 1 si nesting_suitability <= 1
  # if(any(HN_pixel_big$HN_pixel > 1.0001, na.rm=T)) warning(paste("Sitio", site_i, ": HN_pixel_big > 1 encontrado."))
  # if(any(HN_pixel_small$HN_pixel > 1.0001, na.rm=T)) warning(paste("Sitio", site_i, ": HN_pixel_small > 1 encontrado."))


  # --- 6.7 Calcular Idoneidad Floral (HF) por píxel (paralelizado) ---
  message("Calculando HF (paralelizado)...")

  # --- 6.7.1 HF para polinizadores GRANDES ---
  # Celdas candidatas para anidar (donde calcularemos HF)
  HF_pixel_coords_big <- coordinates4big %>%
    dplyr::select(cell, x, y, relative_exp_decay) %>% # Necesitamos el decaimiento relativo al sitio para el cálculo final de POS
    dplyr::distinct(cell, .keep_all = TRUE)

  # --- INICIO BLOQUE DE DEPURACIÓN HF (SOLO SITIO 1, ANTES DEL FOREACH) ---
  if (site_i == 1 && nrow(HF_pixel_coords_big) > 0) {
    message("--- DEBUG HF (Sitio 1, Iteración 1 del foreach - Simulación) ---")
    # Tomar datos de la primera celda candidata
    i_debug = 1 # Ejecutar para la primera celda
    current_nest_cell_debug <- HF_pixel_coords_big[i_debug, ]
    x_nest_debug <- current_nest_cell_debug$x
    y_nest_debug <- current_nest_cell_debug$y
    message(paste("Celda Nido (i=1): x=", round(x_nest_debug,1), "y=", round(y_nest_debug,1)))

    # Filtrar recursos cercanos
    resources_near_nest_debug <- df_features_within_circle_sf %>%
      dplyr::mutate(dist_from_nest = sqrt((x - x_nest_debug)^2 + (y - y_nest_debug)^2)) %>%
      dplyr::filter(dist_from_nest <= radius_factor * typical_foraging_dist_big) %>%
      dplyr::mutate(exp_decay_from_nest = exp(-dist_from_nest / typical_foraging_dist_big))

    message(paste("Número de celdas de recursos encontradas cerca:", nrow(resources_near_nest_debug)))
    if(nrow(resources_near_nest_debug) > 0){
      message("Resumen dist_from_nest:")
      print(summary(resources_near_nest_debug$dist_from_nest))
      message("Resumen exp_decay_from_nest:")
      print(summary(resources_near_nest_debug$exp_decay_from_nest))

      sum_exp_decay_from_nest_debug <- sum(resources_near_nest_debug$exp_decay_from_nest, na.rm = TRUE)
      message(paste("Suma decaimiento desde nido:", sum_exp_decay_from_nest_debug))

      if (sum_exp_decay_from_nest_debug > 0) {
        resources_near_nest_debug <- resources_near_nest_debug %>%
          dplyr::mutate(relative_exp_decay_from_nest = exp_decay_from_nest / sum_exp_decay_from_nest_debug)
      } else {
        resources_near_nest_debug <- resources_near_nest_debug %>% dplyr::mutate(relative_exp_decay_from_nest = 0)
      }
      message("Resumen relative_exp_decay_from_nest:")
      print(summary(resources_near_nest_debug$relative_exp_decay_from_nest))

      # Verificar el join con HF_table_big
      join_check_hf_debug <- dplyr::left_join(resources_near_nest_debug, HF_table_big, by = "codigo")
      message("Filas después del join HF:")
      print(nrow(join_check_hf_debug))
      message("Resumen foraging_suitability después del join:")
      print(summary(join_check_hf_debug$foraging_suitability))
      message("Número de NAs en foraging_suitability:")
      print(sum(is.na(join_check_hf_debug$foraging_suitability)))
      message("Resumen fl_resource_weight después del join:")
      print(summary(join_check_hf_debug$fl_resource_weight))


      # Calcular producto intermedio
      product_df_debug <- join_check_hf_debug %>%
        filter(!is.na(foraging_suitability) & !is.na(relative_exp_decay_from_nest)) %>%
        # ¡Asegúrate que 'weight' existe en join_check_hf_debug! Debería venir de resources_near_nest_debug
        dplyr::mutate(product_pixel = foraging_suitability * weight * relative_exp_decay_from_nest)

      message("Filas después de filtrar NA en tabla HF:")
      print(nrow(product_df_debug))
      message("Resumen product_pixel:")
      print(summary(product_df_debug$product_pixel))

      # Calcular HF final para esta celda
      HF_value_debug <- tryCatch({
        product_df_debug %>%
          group_by(species, season, fl_resource_weight) %>% # Asegúrate que 'species' y 'season' vienen del join HF
          summarise(sum_product_pixel = sum(product_pixel, na.rm = TRUE), .groups = 'drop') %>%
          mutate(product_season_i = sum_product_pixel * fl_resource_weight) %>%
          group_by(species) %>% # Suma sobre estaciones
          summarise(sum_product_season_i = sum(product_season_i, na.rm = TRUE), .groups = 'drop') %>%
          summarise(HF_pixel_final = sum(sum_product_season_i, na.rm = TRUE)) %>% # Suma sobre especies -> HF agregado
          pull(HF_pixel_final)
      }, error = function(e) {message("Error calculando HF_value_debug:", e$message); return(NA)})


      message("Valor HF calculado para esta celda (i=1):")
      print(HF_value_debug)
    } else {
      message("No se encontraron celdas de recursos cerca para la primera celda nido.")
    }
    message("--- FIN DEBUG HF (Sitio 1, Iteración 1) ---")
  }
  # --- FIN BLOQUE DE DEPURACIÓN HF ---

  if (nrow(HF_pixel_coords_big) > 0) {
    # Asegurar que HF_table_big tenga 'codigo', 'species', 'season', 'fl_resource_weight', 'foraging_suitability'
    results_HF_big <- foreach(i = 1:nrow(HF_pixel_coords_big), .packages = c("dplyr"), .combine = 'c') %dopar% {

      current_nest_cell <- HF_pixel_coords_big[i, ]
      x_nest <- current_nest_cell$x
      y_nest <- current_nest_cell$y

      # Filtrar recursos cercanos a ESTA celda de anidación
      resources_near_nest <- df_features_within_circle_sf %>%
        dplyr::mutate(dist_from_nest = sqrt((x - x_nest)^2 + (y - y_nest)^2)) %>%
        dplyr::filter(dist_from_nest <= radius_factor * typical_foraging_dist_big) %>%
        dplyr::mutate(exp_decay_from_nest = exp(-dist_from_nest / typical_foraging_dist_big))

      # Normalización LOCAL del decaimiento desde el nido
      sum_exp_decay_from_nest <- sum(resources_near_nest$exp_decay_from_nest, na.rm = TRUE)

      if (sum_exp_decay_from_nest > 0) {
        resources_near_nest <- resources_near_nest %>%
          dplyr::mutate(relative_exp_decay_from_nest = exp_decay_from_nest / sum_exp_decay_from_nest)
      } else {
        resources_near_nest <- resources_near_nest %>% dplyr::mutate(relative_exp_decay_from_nest = 0)
      }

      # Calcular HF agregado para esta celda de anidación
      HF_value <- resources_near_nest %>%
        dplyr::left_join(HF_table_big, by = "codigo") %>%
        filter(!is.na(foraging_suitability) & !is.na(relative_exp_decay_from_nest)) %>%
        # Producto: IdoneidadFloral * PesoPixel(=1) * DecaimientoRelativoDESDEELNIDO
        dplyr::mutate(product_pixel = foraging_suitability * weight * relative_exp_decay_from_nest) %>%
        group_by(species, season, fl_resource_weight) %>%
        summarise(sum_product_pixel = sum(product_pixel, na.rm = TRUE), .groups = 'drop') %>%
        mutate(product_season_i = sum_product_pixel * fl_resource_weight) %>%
        group_by(species) %>% # Suma sobre estaciones
        summarise(sum_product_season_i = sum(product_season_i, na.rm = TRUE), .groups = 'drop') %>%
        summarise(HF_pixel_final = sum(sum_product_season_i, na.rm = TRUE)) %>% # Suma sobre especies -> HF agregado
        pull(HF_pixel_final)

      # Devolver 0 si no se calculó HF (ningún recurso encontrado o tabla experta vacía)
      if(length(HF_value) == 0 || is.na(HF_value)) { return(0) } else { return(HF_value) }
    } # Fin foreach

    # Asignar resultados
    HF_pixel_coords_big$HF_pixel <- results_HF_big

  } else {
    warning(paste("Sitio", site_i, ": No hay celdas candidatas para HF_big."))
    # Crear dataframe vacío con la estructura esperada para evitar error en join posterior
    HF_pixel_coords_big <- data.frame(cell=integer(), x=numeric(), y=numeric(), relative_exp_decay=numeric(), HF_pixel=numeric())
  }


  # --- 6.7.2 HF para polinizadores PEQUEÑOS ---
  # --- ADAPTAR PARA SMALL --- (Lógica idéntica a 'big', usando parámetros 'small')
  HF_pixel_coords_small <- coordinates4small %>%
    dplyr::select(cell, x, y, relative_exp_decay) %>% # Necesitamos el decaimiento relativo al sitio
    dplyr::distinct(cell, .keep_all = TRUE)

  if (nrow(HF_pixel_coords_small) > 0) {
    results_HF_small <- foreach(i = 1:nrow(HF_pixel_coords_small), .packages = c("dplyr"), .combine = 'c') %dopar% {

      current_nest_cell <- HF_pixel_coords_small[i, ]
      x_nest <- current_nest_cell$x
      y_nest <- current_nest_cell$y

      resources_near_nest <- df_features_within_circle_sf %>%
        dplyr::mutate(dist_from_nest = sqrt((x - x_nest)^2 + (y - y_nest)^2)) %>%
        dplyr::filter(dist_from_nest <= radius_factor * typical_foraging_dist_small) %>% # dist small
        dplyr::mutate(exp_decay_from_nest = exp(-dist_from_nest / typical_foraging_dist_small)) # dist small

      sum_exp_decay_from_nest <- sum(resources_near_nest$exp_decay_from_nest, na.rm = TRUE)

      if (sum_exp_decay_from_nest > 0) {
        resources_near_nest <- resources_near_nest %>%
          dplyr::mutate(relative_exp_decay_from_nest = exp_decay_from_nest / sum_exp_decay_from_nest)
      } else {
        resources_near_nest <- resources_near_nest %>% dplyr::mutate(relative_exp_decay_from_nest = 0)
      }

      HF_value <- resources_near_nest %>%
        dplyr::left_join(HF_table_small, by = "codigo") %>% # tabla small
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
    } # Fin foreach

    HF_pixel_coords_small$HF_pixel <- results_HF_small

  } else {
    warning(paste("Sitio", site_i, ": No hay celdas candidatas para HF_small."))
    HF_pixel_coords_small <- data.frame(cell=integer(), x=numeric(), y=numeric(), relative_exp_decay=numeric(), HF_pixel=numeric())
  }


  # --- 6.8 Calcular Índice de Abundancia Potencial (POS) ---
  # POS = Suma sobre todas las celdas (i) y especies (sp) [ HN(i, sp) * HF(i) * DecaimientoRelativoAlSitio(i) ]
  message("Calculando POS...")

  # --- 6.8.1 POS para polinizadores GRANDES ---
  # Unir HN (por celda y especie) con HF y DecaimientoRelativoAlSitio (por celda)
  POS_big_df <- HN_pixel_big %>% # Contiene: cell, x, y, species, HN_pixel
    # Unimos con HF_pixel_coords_big para obtener HF y el decaimiento relativo al sitio
    dplyr::left_join(HF_pixel_coords_big %>% dplyr::select(cell, relative_exp_decay, HF_pixel), by = "cell") %>%
    filter(!is.na(HF_pixel) & !is.na(relative_exp_decay) & !is.na(HN_pixel)) %>%
    # Calcular el producto para cada especie en cada celda
    # PO = Idoneidad_Anidación * Idoneidad_Floral * Peso_Celda(DecaimientoRelativoAlSitio)
    dplyr::mutate(PO_pixel_species_decay = HN_pixel * HF_pixel * relative_exp_decay)

  # Sumar el producto sobre todas las celdas y todas las especies
  POS_big_final <- sum(POS_big_df$PO_pixel_species_decay, na.rm = TRUE)

  # --- INICIO BLOQUE DE DEPURACIÓN (SOLO PARA SITE 1) ---
  if (site_i == 1) {
    message("--- DEBUG INFO (Sitio 1) ---")

    message("Resumen de HN_pixel_big:")
    print(summary(HN_pixel_big$HN_pixel))
    print(paste("Número de filas en HN_pixel_big:", nrow(HN_pixel_big)))

    message("Resumen de HF_pixel_coords_big:")
    print(summary(HF_pixel_coords_big$HF_pixel))
    print(paste("Número de filas en HF_pixel_coords_big:", nrow(HF_pixel_coords_big)))
    print(summary(HF_pixel_coords_big$relative_exp_decay))


    message("Resumen de POS_big_df (después del join y filter):")
    print(paste("Número de filas en POS_big_df:", nrow(POS_big_df)))
    if(nrow(POS_big_df) > 0) {
      print(summary(POS_big_df$HN_pixel))
      print(summary(POS_big_df$HF_pixel))
      print(summary(POS_big_df$relative_exp_decay))
      print(summary(POS_big_df$PO_pixel_species_decay))
      message("Primeras filas de POS_big_df:")
      print(head(POS_big_df))
    } else {
      message("POS_big_df está vacío después del join/filter.")
      # Intentemos ver qué pasa antes del filter
      POS_big_df_before_filter <- HN_pixel_big %>%
        dplyr::left_join(HF_pixel_coords_big %>% dplyr::select(cell, relative_exp_decay, HF_pixel), by = "cell")
      message(paste("Filas ANTES del filter:", nrow(POS_big_df_before_filter)))
      message("Resumen de HN ANTES del filter:")
      print(summary(POS_big_df_before_filter$HN_pixel))
      message("Resumen de HF ANTES del filter:")
      print(summary(POS_big_df_before_filter$HF_pixel))
      message("Resumen de rel_decay ANTES del filter:")
      print(summary(POS_big_df_before_filter$relative_exp_decay))
      message("Número de NAs en HF ANTES del filter:", sum(is.na(POS_big_df_before_filter$HF_pixel)))
      message("Número de NAs en rel_decay ANTES del filter:", sum(is.na(POS_big_df_before_filter$relative_exp_decay)))
      message("Número de NAs en HN ANTES del filter:", sum(is.na(POS_big_df_before_filter$HN_pixel)))

    }

    message("Valor final POS_big_final:")
    print(POS_big_final)
    message("--- FIN DEBUG INFO (Sitio 1) ---")

    # Podrías añadir un bloque similar para 'small' si también da cero
    # message("--- DEBUG INFO SMALL (Sitio 1) ---")
    # ... (prints y summaries para small) ...
    # message("--- FIN DEBUG INFO SMALL (Sitio 1) ---")
  }
  # --- FIN BLOQUE DE DEPURACIÓN ---

  # --- 6.8.2 POS para polinizadores PEQUEÑOS ---
  # --- ADAPTAR PARA SMALL --- (Lógica similar a 'big')
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

  # Guardar el dataframe completo en cada iteración (más seguro)
  tryCatch({
    readr::write_csv(selected_sites_coordinates, results_filename)
  }, error = function(e){
    warning(paste("Sitio", site_i, ": No se pudieron guardar los resultados intermedios. Error:", e$message))
  })

  tictoc::toc() # Detener cronómetro para este sitio

} # Fin del bucle for sobre los sitios

# --- 7. Finalizar y Limpiar ---
tictoc::toc() # Detener cronómetro general

# Detener el clúster paralelo
stopImplicitCluster()
message("Clúster paralelo detenido.")

message(paste("Procesamiento completado. Resultados finales guardados en:", results_filename))

# Mostrar los resultados finales (primeras filas)
print(head(selected_sites_coordinates))

# --- Fin del Script ---
