

df_percentage4codigo_inside_circle <- function(features_dentro_circulo,
                                               codigo,
                                               resolucion){

  ext <- raster::extent(sf::st_bbox(features_dentro_circulo))

  features_por_codigo <- features_dentro_circulo[features_dentro_circulo$Codigo == codigo, ]

  # Unir los polígonos por el código usando st_union
  features_union <- sf::st_union(features_por_codigo)


  #v <- terra::vect(features_por_codigo)
  v <- terra::vect(features_union)


  # Crear un objeto Raster para el área y resolución definidas
  raster_vacio <- raster::raster(ext, res = c(resolucion, resolucion))
  raster_cobertura <- raster::rasterize(features_por_codigo, raster_vacio, fun = "first")
  raster_vacio <- raster_cobertura
  r <- terra::rast(raster_vacio)

  #resultado <- terra::extract(r, v, na.rm = TRUE, cells = TRUE, xy = TRUE , weights = TRUE)
  resultado <- terra::extract(r, v, na.rm = TRUE, cells = TRUE, xy = TRUE , weights = TRUE, exact = TRUE)
  colnames(resultado) <- c("ID", "layer", "cell", "x", "y", "weight")
  resultado$codigo <- codigo

  #Sanity check
  any(duplicated(resultado$cell))

  return(resultado)


}


