# Asegúrate que sf y raster estén cargados
library(sf)
library(raster)

CopyOffeatures_within_circle <- function(CORINE_raster,
                                         latitud, longitud,
                                         radio_circulo,
                                         resolution_raster) {

  # --- 1. Crear y validar punto ---
  # (Código de validación previo aquí...)
  if (is.na(latitud) || is.na(longitud) || !is.finite(latitud) || !is.finite(longitud)) {
    warning(paste("Coordenadas inválidas (NA o no finitas) para Lon:", longitud, "Lat:", latitud))
    return(data.frame(x=numeric(), y=numeric(), codigo=integer(), cell=integer()))
  }
  punto_sf <- tryCatch({
    st_sfc(st_point(c(longitud, latitud)), crs = 4326)
  }, error = function(e) {
    warning(paste("Error al crear punto sf para Lon:", longitud, "Lat:", latitud, "-", e$message))
    return(NULL)
  })
  if (is.null(punto_sf)) {
    return(data.frame(x=numeric(), y=numeric(), codigo=integer(), cell=integer()))
  }

  # --- 2. Verificar CRS del ráster y transformar punto ---
  # (Código de validación previo aquí...)
  raster_crs <- tryCatch(crs(CORINE_raster), error = function(e) NULL)
  if (is.null(raster_crs) || is.na(raster_crs)) {
    stop("El ráster de entrada (CORINE_raster) no tiene un sistema de coordenadas definido (CRS) o no se pudo obtener.")
  }
  punto_proj <- tryCatch({
    st_transform(punto_sf, raster_crs)
  }, error = function(e) {
    warning(paste("Error al transformar punto a CRS del ráster para Lon:", longitud, "Lat:", latitud, "-", e$message))
    return(NULL)
  })
  if (is.null(punto_proj)) {
    return(data.frame(x=numeric(), y=numeric(), codigo=integer(), cell=integer()))
  }

  # --- 3. Crear y validar buffer (círculo) ---
  # (Código de validación previo aquí...)
  if (is.na(radio_circulo) || radio_circulo <= 0) {
    warning(paste("Radio del círculo inválido (<= 0 o NA):", radio_circulo, "para Lon:", longitud, "Lat:", latitud))
    return(data.frame(x=numeric(), y=numeric(), codigo=integer(), cell=integer()))
  }
  circulo_sf <- tryCatch({
    st_buffer(punto_proj, dist = radio_circulo)
  }, error = function(e) {
    warning(paste("Error en st_buffer para Lon:", longitud, "Lat:", latitud, "-", e$message))
    return(NULL)
  })
  if (is.null(circulo_sf) || length(circulo_sf) == 0 || !inherits(circulo_sf, "sfc")) {
    warning(paste("Creación del buffer falló o resultó en objeto inválido/vacío para Lon:", longitud, "Lat:", latitud))
    return(data.frame(x=numeric(), y=numeric(), codigo=integer(), cell=integer()))
  }


  # --- 4. Recortar (Crop) y Enmascarar (Mask) ---
  # --- INICIO CAMBIO ---
  # Usar st_bbox() para obtener la caja delimitadora del buffer sf
  bbox_circulo <- tryCatch({
    sf::st_bbox(circulo_sf) # Devuelve un objeto 'bbox'
  }, error = function(e) {
    # Este error ahora sería dentro de st_bbox
    warning(paste("Error al obtener el bbox del buffer sf (st_bbox) para Lon:", longitud, "Lat:", latitud, "-", e$message))
    return(NULL)
  })

  # Si no se pudo obtener el bbox, no podemos continuar
  if (is.null(bbox_circulo) || !inherits(bbox_circulo, "bbox")) {
    warning(paste("El bbox del buffer no es válido para Lon:", longitud, "Lat:", latitud))
    return(data.frame(x=numeric(), y=numeric(), codigo=integer(), cell=integer()))
  }
  # --- FIN CAMBIO ---

  # Usamos tryCatch para manejar el caso donde el círculo no solapa con el raster
  # raster::crop puede usar directamente el objeto 'bbox' de sf
  raster_crop <- tryCatch({
    raster::crop(CORINE_raster, bbox_circulo) # Pasar el objeto bbox directamente
  }, error = function(e) {
    message(paste("Nota: El círculo (bbox) para Lon:", longitud, "Lat:", latitud, "podría no solapar con el raster o hubo error en crop:", e$message))
    return(NULL)
  })

  # Si el recorte falló o resultó en NULL, no podemos continuar
  if (is.null(raster_crop)) {
    warning(paste("El recorte del raster resultó en NULL para Lon:", longitud, "Lat:", latitud))
    return(data.frame(x=numeric(), y=numeric(), codigo=integer(), cell=integer()))
  }

  # Convertir círculo a Spatial ANTES de enmascarar
  # Esto debería funcionar bien con el 'circulo_sf' (sfc) original
  circulo_sp <- tryCatch({
    as(circulo_sf, "Spatial")
  }, error = function(e) {
    warning(paste("Error al convertir buffer sf a Spatial para Lon:", longitud, "Lat:", latitud, "-", e$message))
    return(NULL)
  })

  if(is.null(circulo_sp)) {
    return(data.frame(x=numeric(), y=numeric(), codigo=integer(), cell=integer()))
  }

  # Enmascarar
  raster_mask <- tryCatch({
    raster::mask(raster_crop, circulo_sp)
  }, error = function(e) {
    warning(paste("Error durante raster::mask para Lon:", longitud, "Lat:", latitud, "-", e$message))
    return(NULL)
  })

  if(is.null(raster_mask)){
    warning(paste("El enmascaramiento del raster resultó en NULL para Lon:", longitud, "Lat:", latitud))
    return(data.frame(x=numeric(), y=numeric(), codigo=integer(), cell=integer()))
  }

  # --- 5. Remuestrear (Resample) ---
  # (Código sin cambios aquí...)
  mask_extent <- tryCatch(extent(raster_mask), error = function(e) NULL)
  if(is.null(mask_extent)){
    warning(paste("No se pudo obtener la extensión del raster enmascarado para Lon:", longitud, "Lat:", latitud))
    return(data.frame(x=numeric(), y=numeric(), codigo=integer(), cell=integer()))
  }
  raster_ref <- raster::raster(mask_extent, res = resolution_raster, crs = crs(raster_mask))
  if(all(is.na(values(raster_mask)))){
    warning(paste("El raster enmascarado está completamente vacío (todo NA) antes de remuestrear para Lon:", longitud, "Lat:", latitud))
    return(data.frame(x=numeric(), y=numeric(), codigo=integer(), cell=integer()))
  }
  raster_resampled <- raster::resample(raster_mask, raster_ref, method = "ngb")

  # --- 6. Convertir a DataFrame y añadir número de celda ---
  # (Código sin cambios aquí...)
  raster_df <- as.data.frame(raster_resampled, xy = TRUE, na.rm = TRUE)
  if (nrow(raster_df) == 0) {
    warning(paste("No quedaron celdas válidas en el raster dentro del círculo después de remuestrear para Lon:", longitud, "Lat:", latitud))
    return(data.frame(x=numeric(), y=numeric(), codigo=integer(), cell=integer()))
  }
  colnames(raster_df) <- c("x", "y", "codigo")
  cells_from_xy <- tryCatch({
    cellFromXY(raster_resampled, raster_df[, c("x", "y")])
  }, error = function(e){
    warning(paste("Error en cellFromXY para Lon:", longitud, "Lat:", latitud, "-", e$message))
    return(NULL)
  })
  if(is.null(cells_from_xy) || length(cells_from_xy) != nrow(raster_df)){
    warning(paste("Fallo al obtener números de celda para Lon:", longitud, "Lat:", latitud))
    return(data.frame(x=numeric(), y=numeric(), codigo=integer(), cell=integer()))
  }
  raster_df$cell <- cells_from_xy
  if(any(is.na(raster_df$cell))){
    warning(paste("Se produjeron NAs en la columna 'cell' para Lon:", longitud, "Lat:", latitud))
    raster_df <- raster_df[!is.na(raster_df$cell), ]
    if (nrow(raster_df) == 0) {
      return(data.frame(x=numeric(), y=numeric(), codigo=integer(), cell=integer()))
    }
  }

  return(raster_df)
}
