
load_and_fix_geometries <- function(gdb_path, layer2read){

  layers <- st_layers(gdb_path)
  print(layers)

  Leyenda <- st_read(gdb_path, layer = "Leyenda")

  ecosistemas2023 <- st_read(gdb_path, layer = layer2read)

  # Verificar geometrías válidas
  valid_geometries <- st_is_valid(ecosistemas2023, NA_on_error = TRUE, reason = FALSE)
  # # reason_valid_geometries <- st_is_valid(ecosistemas2023, NA_on_error = TRUE, reason = TRUE)
  # # unique(reason_valid_geometries)
  # # Hay autoinserción
  #
  # sum(is.na(valid_geometries))
  # sum(valid_geometries[!is.na(valid_geometries)])
  # sum(!valid_geometries[!is.na(valid_geometries)])

  ecosistemas2023_validos <- ecosistemas2023[!is.na(valid_geometries) & valid_geometries, ]
  ecosistemas2023_invalidos <- ecosistemas2023[!is.na(valid_geometries) & !valid_geometries, ]
  ecosistemas2023_corregidos <- st_make_valid(ecosistemas2023_invalidos)

  valid_geometries_corregidos <- st_is_valid(ecosistemas2023_corregidos, NA_on_error = TRUE, reason = FALSE)
  # sum(is.na(valid_geometries_corregidos))
  # sum(valid_geometries_corregidos[!is.na(valid_geometries_corregidos)])
  # sum(!valid_geometries_corregidos[!is.na(valid_geometries_corregidos)])


  ecosistemas2023[!is.na(valid_geometries) & !valid_geometries, ] <- ecosistemas2023_corregidos

  valid_geometries2 <- st_is_valid(ecosistemas2023, NA_on_error = TRUE, reason = FALSE)
  # sum(is.na(valid_geometries2))
  # sum(valid_geometries2[!is.na(valid_geometries2)])
  # sum(!valid_geometries2[!is.na(valid_geometries2)])

  return(ecosistemas2023[!is.na(valid_geometries2) & valid_geometries2, ])

}
