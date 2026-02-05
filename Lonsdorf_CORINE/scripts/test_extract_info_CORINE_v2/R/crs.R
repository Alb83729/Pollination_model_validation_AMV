ensure_raster_crs <- function(r, fallback_epsg) {
  if (is.na(raster::crs(r))) {
    warning(
      "Raster sin CRS. Asignando fallback EPSG:", fallback_epsg,
      " (solo válido si está verificado para el dataset que use).",
      call. = FALSE
    )
    raster::crs(r) <- sf::st_crs(fallback_epsg)$wkt
  }
  r
}

get_sf_crs_from_raster <- function(r) {
  crs_txt <- raster::crs(r)
  if (is.na(crs_txt)) stop("Raster sigue sin CRS.", call. = FALSE)
  sf::st_crs(crs_txt)
}
