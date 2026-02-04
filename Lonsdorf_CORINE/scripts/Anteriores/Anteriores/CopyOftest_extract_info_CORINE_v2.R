library(sf)               # Trabaja con datos espaciales
library(raster)           # Operaciones con datos ráster
library(dplyr)            # Manipulación de datos
library(foreign)          # Lectura de archivos DBF
library(readr)            # Lectura de archivos CSV
library(ggplot2)          # Visualización de datos
library(terra)
#Verificar si foreach esta instalado
if (!requireNamespace("foreach",quietly = TRUE))
{
#Instalar foreach si no esta instalado
install.packages("foreach")
}
library(foreach)
#Verificar si doParallel esta instalado
if (!requireNamespace("doParallel",quietly = TRUE))
{
  #Instalar doParallel si no esta instalado
  install.packages("doParallel")
}
library(doParallel)

source("scripts/aux_functions/CopyOfload_expert_tables.R")
#source("scripts/aux_functions/CopyOfload_and_fix_geometries.R") #No usar con el raster de CORINE (02/03/2025)
source("scripts/aux_functions/CopyOffeatures_within_circle.R")
source("scripts/aux_functions/CopyOfcreate_df_features_within_circle_sf.R")
source("scripts/aux_functions/CopyOfdf_percentage4codigo_inside_circle.R")

# Spatial parameters to compute the Lonsdorf model -----------------------------#22/03/2025
typical_foraging_dist_big = 1500
typical_foraging_dist_small = 500

# Cargar el raster  ########
raster_path <- "raster_CORINE/U2018_CLC2018_V2020_20u1.tif" #ruta relativa: "../raster_CORINE/U2018_CLC2018_V2020_20u1.TIF"
CORINE_raster <- raster(raster_path)

# Cargar la leyenda  ######
legend_path <- "raster_CORINE/U2018_CLC2018_V2020_20u1.tif.vat.dbf"#legend_path <- "raster_CORINE/U2018_CLC2018_V2020_20u1.tif" #ruta relativa: "../raster_CORINE/U2018_CLC2018_V2020_20u1.tif.vat.dbf"
CORINE_legend <- st_read(legend_path, options = "ENCODING=WINDOWS-1252", quiet = TRUE)#CORINE_legend <- read.dbf(legend_path, as.is = TRUE, encoding = "Windows-1252")#CORINE_legend <- read.dbf(legend_path)
# Verificar la ruta por si está mal ######
raster_path <- normalizePath("raster_CORINE/U2018_CLC2018_V2020_20u1.tif.vat.dbf")
if (!file.exists(raster_path)) stop("El archivo TIFF no existe en la ruta especificada.")

#¿Cargar y corregir geometrías? 07/12/2024
#CORINE_validos2 <- CopyOfload_and_fix_geometries(raster_path,legend_path) #No usar con el raster de CORINE (02/03/2025)
# This operation may take up to 10 minutes depending on the CPU's features.

# Plot raster #####
plot(CORINE_raster)

# Cargar los datos de validación de la JdA  #####
validation_data_path <- "data/validation_JdA_info_visitation_rate.csv"
validation_data <- readr::read_csv(validation_data_path)

# Procesar los datos de validación de la JdA para un site específico
site_i <- 5
latitude_i <- validation_data$latitude[site_i]
longitude_i <- validation_data$longitude[site_i]

# Spatial parameters to compute the Lonsdorf model -----------------------------
radius <- 2000  # Define el radio para el buffer (en metros)

# Raster resolution_raster
resolution_raster <- 100  # pixel area: resolution_raster x resolution_raster

compute_validation_points <- TRUE
compute_other_points <- FALSE

# Crear un punto con las coordenadas de latitude_i y longitude_i
point <- sf::st_point(c(longitude_i, latitude_i)) %>%
  st_sfc(crs = 4326)  # Sistema de referencia CRS 4326

#Verificar que "crs(CORINE_raster) no es NA 24/03/2025
if (is.na(crs(CORINE_raster))) {
  crs(CORINE_raster) <- "+proj=utm +zone=30 +datum=WGS84"  # Ajustar según corresponda
}

# Transformar el punto a un sistema de coordenadas proyectadas
point_proj <- sf::st_transform(point, crs = crs(CORINE_raster))

#Verificar unidades para que st_buffer" no falle 24/03/2025
if (st_crs(CORINE_raster)$units == "degree") {
  radius <- radius / 111320  # Aproximación para convertir metros a grados
}

# Crear un buffer de radio 'radius' alrededor del punto
buffer <- sf::st_buffer(point_proj, dist = 2 * radius)
buffer_sp <- as(buffer, "Spatial")  # Convierte el buffer a formato 'Spatial'

# Recortar el raster usando el buffer
CORINE_crop <- raster::crop(CORINE_raster, extent(buffer_sp))
CORINE_visible <- raster::mask(CORINE_crop, buffer_sp)

# Crear un nuevo raster con los píxeles visibles
CORINE_disk_aux <- raster::trim(CORINE_visible)

# Asegurar que el área fuera del buffer esté correctamente enmascarada
CORINE_disk <- raster::mask(CORINE_disk_aux, buffer_sp) #24/03/2025: CORINE_disk <- raster::mask(CORINE_disk_aux, buffer_sp, updatevalue = NA)

# Ajustar la resolución
reference_raster <- raster::raster(extent(CORINE_crop), res = resolution_raster, crs = crs(CORINE_crop))
CORINE_crop_resampled <- raster::resample(CORINE_crop, reference_raster, method = "ngb")
CORINE_visible_resampled <- raster::mask(CORINE_crop_resampled, buffer_sp)

# ¿Load expert tables? 07/12/2024------------------------------------------------
path_file_expert_table <- "Results/expert_table_JdA_V1.csv"
list_expert_tables <- CopyOfload_expert_tables(path_file_expert_table)
NS_table_big <- list_expert_tables[[1]]
NS_table_small <- list_expert_tables[[2]]
HF_table_big <- list_expert_tables[[3]]
HF_table_small <- list_expert_tables[[4]]

# Load sites' coordinates-------------------------------------------------------
selected_sites_coordinates <- NULL

if(compute_validation_points){

  path_file_validation_coordinates <- "Results/fixed_Junta_validation_data.csv"
  validation_sites_coordinates <- readr::read_csv(path_file_validation_coordinates) %>%
    dplyr::select(Sitio,Ano,Latitud, Longitud) %>%
    rename(site_id = Sitio,latitude = Latitud, longitude = Longitud, refYear = Ano)

  selected_sites_coordinates <- validation_sites_coordinates
}else if(compute_other_points){

  path_file_coordinates <- "data/sites_AAP2.csv"
  other_sites_coordinates <- readr::read_csv(path_file_coordinates) %>%
    dplyr::select(site_id,latitude, longitude,refYear)

  selected_sites_coordinates <- dplyr::bind_rows(selected_sites_coordinates,
                                                 other_sites_coordinates)
}

# Crear un nuevo raster de la región visible
CORINE_disk_resampled_aux <- raster::trim(CORINE_visible_resampled)
CORINE_disk_resampled <- raster::mask(CORINE_disk_resampled_aux, buffer_sp, updatevalue = NA)

# Transformar el ráster a un dataframe ---------------------------------
CORINE_df <- as.data.frame(CORINE_disk_resampled, xy = TRUE, na.rm = TRUE)  # Convertir ráster a dataframe
colnames(CORINE_df) <- c("x", "y", "value")  # Renombrar las columnas

# Unir con la leyenda
CORINE_df <- CORINE_df %>%
  left_join(CORINE_legend, by = c("value" = "Value"))  # Combina con la leyenda usando la columna "value"

# Mostrar los primeros valores
head(CORINE_df)

# Guardar el dataframe como un archivo CSV (opcional)
write.csv(CORINE_df, "CORINE_transformed_dataframe.csv", row.names = FALSE)

# Visualizar con ggplot2 (opcional)
ggplot(CORINE_df, aes(x = x, y = y, fill = as.factor(LABEL3))) +
  geom_tile() +
  labs(fill = "Category") +
  theme_minimal()

# Prepare cores for tasks parallelization --------------------------------------#Reincorporado el 30/11/2024
nCores <- 5 # detectCores() - 1
registerDoParallel(cores = nCores)

# Compute Lonsdorf Pollinator service for selected sites------------------------#Reincorporado el 30/11/2024

selected_sites_coordinates$Lonsdorf_big <- NA
selected_sites_coordinates$Lonsdorf_small <- NA

estimation_time <- Sys.time()
formatted_time <- format(estimation_time, "%Y-%m-%d_%H-%M-%S")

for(site_i in 1:nrow(selected_sites_coordinates)){

  # inicio <- system.time({

  latitud_i <- selected_sites_coordinates$latitude[site_i] %>% as.numeric()
  longitud_i <-  selected_sites_coordinates$longitude[site_i] %>% as.numeric()
#-------------------------------------------REVISAR Y MODIFICAR------------------------------------------
  # Distancia desde el centro hasta los bordes del cuadrado (la mitad del lado del cuadrado)
  circle_radius <- 2 * 2 * typical_foraging_dist_big

  features_dentro_circulo_sf <- CopyOffeatures_within_circle(CORINE_raster, #02/03/2025 ¿entiendo que usar aquí "CORINE_raster" está bien?//06/04/2025 tengo que modificar ésta según Alfonso
                                                       latitud_i,longitud_i,
                                                       circle_radius)
  df_features_within_circle_sf <- CopyOfcreate_df_features_within_circle_sf(features_dentro_circulo_sf)
  #df_features_within_circle_sf <- CopyOfcreate_df_features_within_circle_sf(features_dentro_circulo_sf,
                                                                      #resolution_raster) #//06/04/2025 tengo que modificar ésta según Alfonso

  # Sanity check
  df_features_within_circle_sf %>% group_by(x,y) %>%
    count(wt = weight) %>% mutate(n=round(n,2)) %>% filter(n>1) %>% nrow()

  any(duplicated(df_features_within_circle_sf[,c("x","y","codigo")]))

  # Calcular el centro del bounding box
  bbox <- st_bbox(features_dentro_circulo_sf)
  x_centro <- (bbox["xmax"] + bbox["xmin"]) / 2
  y_centro <- (bbox["ymax"] + bbox["ymin"]) / 2


  df_features_within_circle_sf$latitud <- latitud_i
  df_features_within_circle_sf$longitud <- longitud_i
  df_features_within_circle_sf$x_centro <- x_centro
  df_features_within_circle_sf$y_centro <- y_centro
  df_features_within_circle_sf$distancia_centro <-
    sqrt((df_features_within_circle_sf$x - df_features_within_circle_sf$x_centro)^2+
           (df_features_within_circle_sf$y - df_features_within_circle_sf$y_centro)^2)


  coordinates4big <- df_features_within_circle_sf %>%
    filter(distancia_centro <= 2 * typical_foraging_dist_big) %>% #dplyr::select(x,y) %>%
    unique() %>%
    mutate(exp_decay = exp(-distancia_centro/typical_foraging_dist_big))

  sum_exp_distances_big <- coordinates4big %>%
    dplyr::select(x,y,exp_decay) %>%
    unique() %>% ungroup() %>% dplyr::select(exp_decay) %>%
    summarise_all(sum) %>% pull()

  # Sanity check
  sum(coordinates4big$weight*coordinates4big$exp_decay)==sum_exp_distances_big

  coordinates4big$relative_exp_decay <-
    coordinates4big$exp_decay/sum_exp_distances_big


  coordinates4small <- df_features_within_circle_sf %>%
    filter(distancia_centro <= 2 * typical_foraging_dist_small) %>% # dplyr::select(x,y) %>%
    unique() %>%
    mutate(exp_decay = exp(-distancia_centro/typical_foraging_dist_big))

  sum_exp_distances_small <- coordinates4small %>%
    dplyr::select(x,y,exp_decay) %>%
    unique() %>% ungroup() %>% dplyr::select(exp_decay) %>%
    summarise_all(sum) %>% pull()

  # Sanity check
  sum(coordinates4small$weight*coordinates4small$exp_decay)==sum_exp_distances_small

  coordinates4small$relative_exp_decay <-
    coordinates4small$exp_decay/sum_exp_distances_small

  # Adding nesting sutitability

  HN_pixel_big <- coordinates4big %>% rename(code = codigo) %>%
    left_join(NS_table_big, by=c("code")) %>%
    dplyr::select(latitud,longitud,x,y,species,weight,nesting_suitability) %>%
    mutate(HN_LC_pixel = weight * nesting_suitability) %>%
    group_by(latitud,longitud,x,y,species) %>% count(wt = HN_LC_pixel) %>%
    rename(HN_pixel = n) %>%
    ungroup()

  HN_pixel_small <- coordinates4small %>% rename(code = codigo) %>%
    left_join(NS_table_small, by=c("code")) %>%
    dplyr::select(latitud,longitud,x,y,species,weight,nesting_suitability) %>%
    mutate(HN_LC_pixel = weight * nesting_suitability) %>%
    group_by(latitud,longitud,x,y,species) %>% count(wt = HN_LC_pixel) %>%
    rename(HN_pixel = n) %>%
    ungroup()


  # Sanity check
  any(HN_pixel_big$HN_pixel >= 1)
  any(HN_pixel_small$HN_pixel >= 1)

  HF_pixel_big <- unique(coordinates4big[,c("x","y","relative_exp_decay")])

  results_big <- vector("list", nrow(HF_pixel_big))

  results_big <- foreach(cell_i = 1:nrow(HF_pixel_big), .packages = c("dplyr")) %dopar% {

    x_centro <- HF_pixel_big$x[cell_i]
    y_centro <- HF_pixel_big$y[cell_i]

    df_temp <- df_features_within_circle_sf
    df_temp$x_centro <- x_centro
    df_temp$y_centro <- y_centro
    df_temp$distancia_centro <- sqrt((df_temp$x - x_centro)^2 + (df_temp$y - y_centro)^2)

    coordinates4big_cell_i <- df_temp %>%
      filter(distancia_centro <= 2 * typical_foraging_dist_big) %>%
      unique()

    coordinates4big_cell_i$exp_decay <-  exp(-coordinates4big_cell_i$distancia_centro / typical_foraging_dist_big)

    # sum_exp_distances_big <- coordinates4big_cell_i %>%
    #   dplyr::select(x,y,exp_decay) %>%
    #   unique() %>% ungroup() %>% dplyr::select(exp_decay) %>%
    #   summarise_all(sum) %>% pull()

    coordinates4big_cell_i$relative_exp_decay <- coordinates4big_cell_i$exp_decay / sum_exp_distances_big

    HF_pixel_big_cell_i <- coordinates4big_cell_i %>% rename(code = codigo) %>%
      left_join(HF_table_big, by=c("code")) %>%
      mutate(product_pixel = foraging_suitability * weight * relative_exp_decay) %>%
      dplyr::select(species,season,fl_resource_weight,product_pixel) %>%
      group_by(species,season,fl_resource_weight) %>% count(wt = product_pixel) %>%
      rename(sum_product_pixel = n) %>%
      ungroup() %>% mutate(product_season_i = sum_product_pixel * fl_resource_weight) %>%
      group_by(species) %>% count(wt = product_season_i) %>%
      rename(sum_product_season_i = n) %>%
      ungroup() %>% dplyr::select(-species) %>% pull()

    return(HF_pixel_big_cell_i)
  }

  #coordinates4big$HF_pixel <- unlist(results_big)
  HF_pixel_big$HF_pixel <- unlist(results_big)




  HF_pixel_small <- unique(coordinates4small[,c("x","y","relative_exp_decay")])

  results_small <- vector("list", nrow(HF_pixel_small))

  # results_small <- vector("list", nrow(coordinates4small))

  results_small <- foreach(cell_i = 1:nrow(HF_pixel_small), .packages = c("dplyr")) %dopar% {

    x_centro <- HF_pixel_small$x[cell_i]
    y_centro <- HF_pixel_small$y[cell_i]

    df_temp <- df_features_within_circle_sf
    df_temp$x_centro <- x_centro
    df_temp$y_centro <- y_centro
    df_temp$distancia_centro <- sqrt((df_temp$x - x_centro)^2 + (df_temp$y - y_centro)^2)

    coordinates4small_cell_i <- df_temp %>%
      filter(distancia_centro <= 2 * typical_foraging_dist_small) %>%
      unique()

    coordinates4small_cell_i$exp_decay <- exp(-coordinates4small_cell_i$distancia_centro / typical_foraging_dist_small)

    # sum_exp_distances_small <- coordinates4small_cell_i %>%
    #   dplyr::select(x,y,exp_decay) %>%
    #   unique() %>% ungroup() %>% dplyr::select(exp_decay) %>%
    #   summarise_all(sum) %>% pull()

    coordinates4small_cell_i$relative_exp_decay <- coordinates4small_cell_i$exp_decay / sum_exp_distances_small

    HF_pixel_small_cell_i <- coordinates4small_cell_i %>% rename(code = codigo) %>%
      left_join(HF_table_small, by=c("code")) %>%
      mutate(product_pixel = foraging_suitability * weight * relative_exp_decay) %>%
      dplyr::select(species,season,fl_resource_weight,product_pixel) %>%
      group_by(species,season,fl_resource_weight) %>% count(wt = product_pixel) %>%
      rename(sum_product_pixel = n) %>%
      ungroup() %>% mutate(product_season_i = sum_product_pixel * fl_resource_weight) %>%
      group_by(species) %>% count(wt = product_season_i) %>%
      rename(sum_product_season_i = n) %>%
      ungroup() %>% dplyr::select(-species) %>% pull()

    return(HF_pixel_small_cell_i)
  }

  HF_pixel_small$HF_pixel <- unlist(results_small)

  # POS_big <- coordinates4big %>%
  #   left_join(HN_pixel_big, by = c("latitud","longitud","x","y")) %>%
  #   mutate(PO_pixel_decay = HN_pixel * HF_pixel * relative_exp_decay) %>%
  #   group_by(latitud,longitud) %>% count(wt = PO_pixel_decay) %>%
  #   rename(POs_big = n)

  POS_big_df <- HF_pixel_big %>%
    left_join(HN_pixel_big, by = c("x","y")) %>%
    mutate(PO_pixel_decay = HN_pixel * HF_pixel * relative_exp_decay)

  POS_big <- sum(POS_big_df$PO_pixel_decay)


  # POS_small <- coordinates4small %>%
  #   left_join(HN_pixel_small, by = c("latitud","longitud","x","y")) %>%
  #   mutate(PO_pixel_decay = HN_pixel * HF_pixel * relative_exp_decay) %>%
  #   group_by(latitud,longitud) %>% count(wt = PO_pixel_decay) %>%
  #   rename(POs_small = n)

  POS_small_df <- HF_pixel_small %>%
    left_join(HN_pixel_small, by = c("x","y")) %>%
    mutate(PO_pixel_decay = HN_pixel * HF_pixel * relative_exp_decay)

  POS_small <- sum(POS_small_df$PO_pixel_decay)

  # selected_sites_coordinates$Lonsdorf_big[site_i] <- POS_big$POs_big[1]
  # selected_sites_coordinates$Lonsdorf_small[site_i] <- POS_small$POs_small[1]

  selected_sites_coordinates$Lonsdorf_big[site_i] <- POS_big
  selected_sites_coordinates$Lonsdorf_small[site_i] <- POS_small

  # Save results

  readr::write_csv(selected_sites_coordinates, paste0("../Results/Lonsdorf_results_",formatted_time,".csv"))
#-----------------------------------------FIN REVISAR Y MODIFICAR----------------------------------------
}

Sys.time()
