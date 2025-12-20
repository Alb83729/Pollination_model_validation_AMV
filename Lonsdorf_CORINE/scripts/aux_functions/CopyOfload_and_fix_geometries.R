
CopyOfload_and_fix_geometries <- function(raster_path, legend_pah){
#NO USAR CON EL RASTER DE CORINE (02/03/2025)
  layers <- st_layers(raster_path)
  print(layers)

  Leyenda <- st_read(raster_path, layer = "Leyenda")

  CORINE <- st_read(raster_path, layer = legend_pah)

  # Verificar geometrías válidas
  valid_geometries <- st_is_valid(CORINE, NA_on_error = TRUE, reason = FALSE)
  # # reason_valid_geometries <- st_is_valid(ecosistemas2023, NA_on_error = TRUE, reason = TRUE)
  # # unique(reason_valid_geometries)
  # # Hay autoinserción
  #
  # sum(is.na(valid_geometries))
  # sum(valid_geometries[!is.na(valid_geometries)])
  # sum(!valid_geometries[!is.na(valid_geometries)])

  CORINE_validos <- CORINE[!is.na(valid_geometries) & valid_geometries, ]
  CORINE_invalidos <- CORINE[!is.na(valid_geometries) & !valid_geometries, ]
  CORINE_corregidos <- st_make_valid(CORINE_invalidos)

  valid_geometries_corregidos <- st_is_valid(CORINE_corregidos, NA_on_error = TRUE, reason = FALSE)
  # sum(is.na(valid_geometries_corregidos))
  # sum(valid_geometries_corregidos[!is.na(valid_geometries_corregidos)])
  # sum(!valid_geometries_corregidos[!is.na(valid_geometries_corregidos)])


  CORINE[!is.na(valid_geometries) & !valid_geometries, ] <- CORINE_corregidos

  valid_geometries2 <- st_is_valid(CORINE, NA_on_error = TRUE, reason = FALSE)
  # sum(is.na(valid_geometries2))
  # sum(valid_geometries2[!is.na(valid_geometries2)])
  # sum(!valid_geometries2[!is.na(valid_geometries2)])

  return(CORINE[!is.na(valid_geometries2) & valid_geometries2, ])

}
