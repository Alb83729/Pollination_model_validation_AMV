#
# CopyOffeatures_within_circle <- function(CORINE_validos2,
#                                     latitud,longitud,
#                                     radio_circulo){
#
#   # Crear un punto SF en WGS84
#   punto_sf <- st_sfc(st_point(c(longitud, latitud)), crs = 4326)
#
#   # Transformar el punto al CRS de CORINE_raster (ETRS89 / UTM zona 30N)
#   punto_sf_utm <- st_transform(punto_sf, st_crs(CORINE_validos2))
#
#   # Transformar el punto al CRS de ecosistemas2023
#   punto_sf_utm <- st_transform(punto_sf, st_crs(CORINE_validos2)$epsg)
#
#   # Crear un cuadrado alrededor del punto
#   circulo <- st_buffer(punto_sf_utm, dist = radio_circulo) # Esto crea un círculo
#
#   # Intersectar el circulo con CORINE_validos2
#   features_dentro_circulo <- st_intersection(CORINE_validos2, circulo)
#
#   return(features_dentro_circulo)
#
# }
#
#VERSIÓN PREVIA AL 19/04/2025:
#intento del 17/04/2025:
# CopyOffeatures_within_circle <- function(CORINE_raster,
#                                          latitud, longitud,
#                                          radio_circulo,
#                                          resolution_raster) {
#
#   # Crear punto en WGS84
#   punto_sf <- st_sfc(st_point(c(longitud, latitud)), crs = 4326)
#
#   # Transformar punto al CRS del ráster
#   punto_proj <- st_transform(punto_sf, crs(CORINE_raster))
#
#   # Crear círculo (buffer)
#   circulo <- st_buffer(punto_proj, dist = radio_circulo)
#
#   # Recortar y enmascarar el ráster original
#   raster_crop <- raster::crop(CORINE_raster, circulo)
#   raster_mask <- raster::mask(raster_crop, circulo)
#
#   # Crear un ráster de referencia con la resolución deseada
#   raster_ref <- raster::raster(extent(raster_mask), res = resolution_raster, crs = crs(CORINE_raster))
#
#   # Reescalar el ráster original a la nueva resolución (usando vecino más cercano)
#   raster_resampled <- raster::resample(raster_mask, raster_ref, method = "ngb")
#
#   # Convertir el ráster a dataframe
#   raster_df <- as.data.frame(raster_resampled, xy = TRUE, na.rm = TRUE)
#   colnames(raster_df) <- c("x", "y", "codigo")  # "codigo" corresponde al valor de categoría
#
#   # Añadir el número de celda
#   raster_df$cell <- cellFromXY(raster_resampled, raster_df[, c("x", "y")])
#
#   return(raster_df)
# }
#FIN VERSIÓN PREVIA AL 19/04/2025
library(sf)
library(raster)
library(dplyr) # Asegúrate de tener dplyr cargado si usas pipes fuera

CopyOffeatures_within_circle <- function(CORINE_raster,
                                         latitud, longitud,
                                         radio_circulo,
                                         resolution_raster) {

  # Crear punto en WGS84
  punto_sf <- st_sfc(st_point(c(longitud, latitud)), crs = 4326)

  # Verificar que el raster tenga CRS
  if (is.na(crs(CORINE_raster))) {
    stop("El ráster de entrada (CORINE_raster) no tiene un sistema de coordenadas definido (CRS).")
  }

  # Transformar punto al CRS del ráster
  punto_proj <- st_transform(punto_sf, crs(CORINE_raster))

  # Crear círculo (buffer) sf
  # Usamos un nombre más descriptivo para evitar confusión con la función 'raster::crop'
  circulo_sf <- st_buffer(punto_proj, dist = radio_circulo)

  # --- INICIO SOLUCIÓN ---
  # 1. Recortar usando la EXTENSIÓN del círculo sf
  #    raster::extent() puede extraer la extensión directamente de un objeto sf
  extent_circulo <- raster::extent(circulo_sf)
  #    Usamos tryCatch para manejar el caso donde el círculo no solapa con el raster
  raster_crop <- tryCatch({
    raster::crop(CORINE_raster, extent_circulo)
  }, error = function(e) {
    warning(paste("Error al recortar el raster con el círculo: el círculo podría no solapar con la extensión del raster.", e))
    return(NULL) # Devolvemos NULL si no se puede recortar
  })

  # Si el recorte falló o resultó en NULL, no podemos continuar
  if (is.null(raster_crop)) {
    warning("El recorte del raster resultó en NULL. No se encontraron celdas dentro de la extensión del círculo.")
    # Devolver un dataframe vacío con la estructura esperada
    return(data.frame(x=numeric(), y=numeric(), codigo=integer(), cell=integer()))
  }


  # 2. Enmascarar usando el círculo sf CONVERTIDO a Spatial
  #    Convertimos el objeto sf a Spatial, que raster::mask entiende bien
  circulo_sp <- as(circulo_sf, "Spatial")
  raster_mask <- raster::mask(raster_crop, circulo_sp)
  # --- FIN SOLUCIÓN ---

  # Crear un ráster de referencia con la resolución deseada
  # Importante: Usar la extensión del raster YA RECORTADO Y ENMASCARADO (raster_mask)
  raster_ref <- raster::raster(extent(raster_mask), res = resolution_raster, crs = crs(raster_mask)) # Usar crs de raster_mask por si acaso

  # Reescalar el ráster enmascarado a la nueva resolución (usando vecino más cercano)
  # Asegurarse de que raster_mask no sea NULL (aunque ya lo chequeamos antes con raster_crop)
  if(is.null(raster_mask) || all(is.na(values(raster_mask)))){
    warning("El raster enmascarado está vacío o es NULL antes de remuestrear.")
    return(data.frame(x=numeric(), y=numeric(), codigo=integer(), cell=integer()))
  }
  raster_resampled <- raster::resample(raster_mask, raster_ref, method = "ngb")

  # Convertir el ráster a dataframe
  raster_df <- as.data.frame(raster_resampled, xy = TRUE, na.rm = TRUE)

  # Comprobar si el dataframe está vacío después de quitar NAs
  if (nrow(raster_df) == 0) {
    warning("No quedaron celdas válidas en el raster dentro del círculo después de remuestrear y quitar NAs.")
    return(data.frame(x=numeric(), y=numeric(), codigo=integer(), cell=integer()))
  }

  colnames(raster_df) <- c("x", "y", "codigo")  # "codigo" corresponde al valor de categoría

  # Añadir el número de celda DEL RASTER REMUESTREADO
  # Es crucial usar 'raster_resampled' aquí
  raster_df$cell <- cellFromXY(raster_resampled, raster_df[, c("x", "y")])

  # Comprobación final de que 'cell' no contiene NAs (no debería si cellFromXY funcionó)
  if(any(is.na(raster_df$cell))){
    warning("Se produjeron NAs en la columna 'cell'. Verifique las coordenadas y el raster remuestreado.")
    # Podrías decidir eliminar esas filas o detenerte
    raster_df <- raster_df[!is.na(raster_df$cell), ]
    if (nrow(raster_df) == 0) {
      return(data.frame(x=numeric(), y=numeric(), codigo=integer(), cell=integer()))
    }
  }


  return(raster_df)
}
