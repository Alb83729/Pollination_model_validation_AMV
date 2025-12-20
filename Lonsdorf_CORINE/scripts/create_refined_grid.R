

library(mapSpain)
library(ggplot2)
library(sf)
# Obtener los límites de Andalucía
andalucia <- esp_get_ccaa("01")

# Crear un objeto SpatialPolygonsDataFrame para Andalucía
andalucia_sp <- as(andalucia, 'Spatial')

crs_andalucia <- st_crs(andalucia)

# Definir la resolución del grid (por ejemplo, 0.1 grados)
res <- 0.03

# Obtener los límites de Andalucía
bounds <- st_bbox(andalucia)

# Crear una grilla regular que cubra los límites de Andalucía
grilla <- st_make_grid(andalucia, cellsize = c(res, res), square = TRUE)

# Convertir la grilla a un sf object
puntos_grilla <- st_sf(geometry = st_sfc(grilla, crs = crs_andalucia))

# Intersectar los puntos con el polígono de Andalucía para quedarse solo con los puntos dentro
puntos_andalucia <- st_intersection(puntos_grilla, andalucia)

# Usa st_within para obtener los índices de los puntos que están completamente dentro del polígono de Andalucía
indices_dentro <- st_within(puntos_grilla, andalucia, sparse = FALSE)

# Filtrar los puntos que están dentro de Andalucía
puntos_dentro_andalucia <- puntos_grilla[indices_dentro, ]

# # Eliminar geometrías vacías resultantes de la intersección
# puntos_andalucia <- puntos_andalucia[!st_is_empty(puntos_andalucia), ]

# Si quieres mantener solo los centros de las celdas de la grilla:
centros_grilla <- st_centroid(puntos_dentro_andalucia)

# Extraer las coordenadas
coords <- st_coordinates(centros_grilla)

# Convertir las coordenadas a un data.frame
df_coords <- data.frame(study_id = "JdA",  site_id=1:nrow(coords), longitude = coords[, 'X'], latitude = coords[, 'Y'], refYear=2019)

# Ver el data.frame
print(df_coords)


# Crear el plot
ggplot(data = andalucia) +
  geom_sf() +
  geom_point(data = df_coords, aes(x = longitude, y = latitude), color = "red", size = 2) +
  theme_minimal()



# Definir la resolución del grid (por ejemplo, 0.1 grados)
res2 <- 0.03

# Obtener los límites de Andalucía
bounds <- st_bbox(andalucia)

# Crear una grilla regular que cubra los límites de Andalucía

grilla2 <- st_make_grid(andalucia, cellsize = c(res2, res2), square = TRUE)

# Convertir la grilla a un sf object
puntos_grilla2 <- st_sf(geometry = st_sfc(grilla2, crs = crs_andalucia))

# Intersectar la grilla con el polígono de Andalucía para obtener las celdas que están al menos parcialmente dentro
celdas_intersectadas <- st_intersection(puntos_grilla2, andalucia)

# Calcular los centroides de las celdas intersectadas
centros_grilla2 <- st_centroid(celdas_intersectadas)

# Asegúrate de eliminar geometrías vacías que podrían resultar de la intersección
centros_grilla2 <- centros_grilla2[!st_is_empty(centros_grilla2), ]

# Si es necesario, puedes convertir esto a un data frame para manejar más fácilmente las coordenadas
coords2 <- st_coordinates(centros_grilla2)


# Convertir coordenadas a data frames
df_coords1 <- as.data.frame(coords)
colnames(df_coords1) <- c("longitude", "latitude")
df_coords1$longitude <- round(df_coords1$longitude,6)
df_coords1$latitude <- round(df_coords1$latitude,6)

df_coords2 <- as.data.frame(coords2)
colnames(df_coords2) <- c("longitude", "latitude")
df_coords2$longitude <- round(df_coords2$longitude,6)
df_coords2$latitude <- round(df_coords2$latitude,6)

# Agregar una columna de identificación a cada data frame
df_coords1$id <- "coords1"
df_coords2$id <- "coords2"

# Combinar ambos data frames
combined_coords <- rbind(df_coords1, df_coords2)

# Encontrar las filas únicas en coords2
unique_coords2 <- combined_coords[!duplicated(combined_coords[,c("longitude", "latitude")]),]
unique_in_coords2 <- unique_coords2[unique_coords2$id == "coords2",]

# Las coordenadas únicas de coords2 que no están en coords1
df_coords2 <- unique_in_coords2[, c("longitude", "latitude")]
df_coords2$study_id = "JdA"
df_coords2$site_id=(nrow(coords)+1):((nrow(coords)+nrow(df_coords2)))
df_coords2$refYear=2019

# Ver el data.frame
print(df_coords2)


# Crear el plot
ggplot(data = andalucia) +
  geom_sf() +
  geom_point(data = df_coords2, aes(x = longitude, y = latitude), color = "blue", size = 2) +
  theme_minimal()


# Definir la resolución del grid (por ejemplo, 0.1 grados)
res3 <- 0.03

# Obtener los límites de Andalucía
bounds <- st_bbox(andalucia)

# Crear una grilla regular que cubra los límites de Andalucía

grilla3 <- st_make_grid(andalucia, cellsize = c(res3, res3), square = TRUE)

# Convertir la grilla a un sf object
puntos_grilla3<- st_sf(geometry = st_sfc(grilla3, crs = crs_andalucia))

# Intersectar la grilla con el polígono de Andalucía para obtener las celdas que están al menos parcialmente dentro
celdas_intersectadas3 <- st_intersection(puntos_grilla3, andalucia)

# Calcular los centroides de las celdas intersectadas
centros_grilla3 <- st_centroid(celdas_intersectadas3)

# Asegúrate de eliminar geometrías vacías que podrían resultar de la intersección
centros_grilla3 <- centros_grilla3[!st_is_empty(centros_grilla3), ]

# Si es necesario, puedes convertir esto a un data frame para manejar más fácilmente las coordenadas
coords3 <- st_coordinates(centros_grilla3)

# Convertir las coordenadas a un data.frame
df_coords3 <- data.frame(study_id = "JdA",  site_id=1:nrow(coords3), longitude = coords3[, 'X'], latitude = coords3[, 'Y'], refYear=2019)

# Ver el data.frame
print(df_coords3)


# Crear el plot
ggplot(data = andalucia) +
  geom_sf() +
  geom_point(data = df_coords3, aes(x = longitude, y = latitude), color = "red", size = 2) +
  theme_minimal()


readr::write_csv(df_coords,"Results/sites_AAP2_refined_UNIFORM.csv")
readr::write_csv(df_coords2,"Results/sites_AAP2_refined_LIMITS.csv")
readr::write_csv(df_coords3,"Results/sites_AAP2_refined.csv")
