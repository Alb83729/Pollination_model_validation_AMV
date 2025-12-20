
features_within_circle <- function(ecosistemas2023_validos2,
                                    latitud,longitud,
                                    radio_circulo){

  # Crear un punto SF en WGS84
  punto_sf <- st_sfc(st_point(c(longitud, latitud)), crs = 4326)

  # Transformar el punto al CRS de ecosistemas2023 (ETRS89 / UTM zona 30N)
  punto_sf_utm <- st_transform(punto_sf, st_crs(ecosistemas2023_validos2))

  # Transformar el punto al CRS de ecosistemas2023
  punto_sf_utm <- st_transform(punto_sf, st_crs(ecosistemas2023_validos2)$epsg)

  # Crear un cuadrado alrededor del punto
  circulo <- st_buffer(punto_sf_utm, dist = radio_circulo) # Esto crea un círculo

  # Intersectar el circulo con ecosistemas2023_validos2
  features_dentro_circulo <- st_intersection(ecosistemas2023_validos2, circulo)

  return(features_dentro_circulo)

}

