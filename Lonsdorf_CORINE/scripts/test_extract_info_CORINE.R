library(sf)
library(dplyr)
library(raster)
library(foreign)
library(readr)
library(ggplot2)

# Cargar el raster
raster_path <- "../raster_CORINE/U2018_CLC2018_V2020_20u1.TIF"
CORINE_raster <- raster(raster_path)

# Cargar la leyenda
legend_path <- "../raster_CORINE/U2018_CLC2018_V2020_20u1.tif.vat.dbf"
CORINE_legend <- read.dbf(legend_path)

# Plot raster
plot(CORINE_raster)


# Cargar los datos de validación de la JdA
validation_data_path <- "../data/validation_JdA_info_visitation_rate.csv"
validation_data <- readr::read_csv(validation_data_path)

# Procesar los datos de validación de la JdA para un site
site_i <- 7
latitude_i <- validation_data$latitude[site_i]
longitude_i <- validation_data$longitude[site_i]

radius <- 2000
new_res <- 100

# Crear un punto con las coordenadas de latitude_i y longitude_i
point <- sf::st_point(c(longitude_i, latitude_i)) %>%
  st_sfc(crs = 4326)  # CRS 4326 es para coordenadas geográficas (latitud/longitud)

# Transformar el punto a un sistema de coordenadas proyectadas adecuado
# Aquí asumimos que el raster está en una proyección métrica (puedes ajustar si es necesario)
point_proj <- sf::st_transform(point, crs = crs(CORINE_raster))

# Crear un buffer de radio 'radius' alrededor del punto
buffer <- sf::st_buffer(point_proj, dist = 2*radius)
buffer_sp <- as(buffer, "Spatial")

# Recortar el raster usando el buffer
CORINE_crop <- raster::crop(CORINE_raster, extent(buffer_sp))  # Recortar usando el Extent del buffer
CORINE_visible <- raster::mask(CORINE_crop, buffer_sp)  # Aplicar la máscara para obtener la región circular exacta

# Crear un nuevo raster que solo contenga los píxeles visibles
CORINE_disk_aux <- raster::trim(CORINE_visible)

# Asegurar que el área fuera del buffer esté correctamente enmascarada
CORINE_disk <- raster::mask(CORINE_disk_aux, buffer_sp, updatevalue = NA)  # Asegurarse de que los valores fuera del buffer sean NA

# Ajustar la resolución usando resample() y manteniendo la categoría más frecuente
# Crear un nuevo raster de referencia con la resolución de 150m
reference_raster <- raster::raster(extent(CORINE_crop), res = new_res, crs = crs(CORINE_crop))
CORINE_crop_resampled <- raster::resample(CORINE_crop, reference_raster, method = "ngb")  # "ngb" es el vecino más cercano
CORINE_visible_resampled <- raster::mask(CORINE_crop_resampled, buffer_sp)  # Aplicar la máscara para obtener la región circular exacta

# Crear un nuevo raster que solo contenga los píxeles visibles
CORINE_disk_resampled_aux <- raster::trim(CORINE_visible_resampled)

# Asegurar que el área fuera del buffer esté correctamente enmascarada
CORINE_disk_resampled <- raster::mask(CORINE_disk_resampled_aux, buffer_sp, updatevalue = NA)  # Asegurarse de que los valores fuera del buffer sean NA


plot(CORINE_disk, main = "Pixel size = 100 m")  # Visualizar el raster recortado al círculo
plot(CORINE_disk_resampled, main = paste0(main ="Pixel size = ",new_res," m"))




unique_values <- unique(values(CORINE_disk_resampled))  %>% na.omit() %>% sort()
unique_values

legend_filtered <- CORINE_legend %>% filter(Value %in% unique_values) %>%
  dplyr::select(-Count, -Red, -Green, -Blue)

# Calcular el centro del raster
center_x <- (xmin(CORINE_crop_resampled) + xmax(CORINE_crop_resampled)) / 2
center_y <- (ymin(CORINE_crop_resampled) + ymax(CORINE_crop_resampled)) / 2
radius_crop <- (xmax(CORINE_crop_resampled) - xmin(CORINE_crop_resampled)) / 2

CORINE_crop_resampled_df_aux <- as.data.frame(CORINE_crop_resampled, xy = TRUE, na.rm = TRUE) %>%
  mutate(dist_center = sqrt(((x-center_x)^2)+((y-center_y)^2))) %>%
  filter(dist_center <= radius_crop)

colnames(CORINE_crop_resampled_df_aux) <- c("x","y","Value","dist_center")

CORINE_crop_resampled_df <- CORINE_crop_resampled_df_aux %>%
  left_join(legend_filtered, by = "Value")



ggplot(CORINE_crop_resampled_df, aes(x=x,y=y,color=as.factor(LABEL3)))+
  geom_point(size=2)+
  labs(color="")+
  theme_bw()
