compute_lonsdorf_for_site <- function(
    CORINE_raster, legend_df,
    NS_big, NS_small, HF_big, HF_small,
    lat, lon,
    cfg,
    crs_raster_sf
) {
  # Validar coords
  if (is.na(lat) || is.na(lon) || !is.finite(lat) || !is.finite(lon)) {
    return(list(big = NA_real_, small = NA_real_))
  }

  # Centro proyectado real del sitio
  point_ll <- sf::st_sfc(sf::st_point(c(lon, lat)), crs = 4326)
  point_proj <- tryCatch(sf::st_transform(point_ll, crs = crs_raster_sf), error = function(e) NULL)
  if (is.null(point_proj)) return(list(big = NA_real_, small = NA_real_))

  xy <- sf::st_coordinates(point_proj)
  x_center <- xy[1, "X"]; y_center <- xy[1, "Y"]

  # Extraer features dentro del círculo
  circle_radius <- cfg$radius_factor * cfg$typical_foraging_dist_big
  features <- CopyOffeatures_within_circle(CORINE_raster, lat, lon, circle_radius, cfg$resolution_raster)

  if (nrow(features) == 0) return(list(big = NA_real_, small = NA_real_))

  df <- CopyOfcreate_df_features_within_circle_sf(features) |>
    dplyr::mutate(
      latitud = lat, longitud = lon,
      x_centro_site = x_center, y_centro_site = y_center,
      distancia_centro = sqrt((x - x_centro_site)^2 + (y - y_centro_site)^2)
    )

  # BIG base
  coords_big <- df |>
    dplyr::filter(distancia_centro <= cfg$radius_factor * cfg$typical_foraging_dist_big) |>
    dplyr::mutate(exp_decay = exp(-distancia_centro / cfg$typical_foraging_dist_big))

  sum_big <- coords_big |>
    dplyr::distinct(cell, .keep_all = TRUE) |>
    dplyr::summarise(s = sum(exp_decay, na.rm = TRUE)) |>
    dplyr::pull(s)

  coords_big <- coords_big |>
    dplyr::mutate(relative_exp_decay = if (sum_big > 0) exp_decay / sum_big else 0)

  # SMALL base
  coords_small <- df |>
    dplyr::filter(distancia_centro <= cfg$radius_factor * cfg$typical_foraging_dist_small) |>
    dplyr::mutate(exp_decay = exp(-distancia_centro / cfg$typical_foraging_dist_small))

  sum_small <- coords_small |>
    dplyr::distinct(cell, .keep_all = TRUE) |>
    dplyr::summarise(s = sum(exp_decay, na.rm = TRUE)) |>
    dplyr::pull(s)

  coords_small <- coords_small |>
    dplyr::mutate(relative_exp_decay = if (sum_small > 0) exp_decay / sum_small else 0)

  # HN
  HN_big <- coords_big |>
    dplyr::left_join(NS_big |> dplyr::select(codigo, species, nesting_suitability), by = "codigo") |>
    dplyr::filter(!is.na(nesting_suitability)) |>
    dplyr::mutate(HN_LC_pixel = weight * nesting_suitability) |>
    dplyr::group_by(cell, x, y, latitud, longitud, species) |>
    dplyr::summarise(HN_pixel = sum(HN_LC_pixel, na.rm = TRUE), .groups = "drop")

  HN_small <- coords_small |>
    dplyr::left_join(NS_small |> dplyr::select(codigo, species, nesting_suitability), by = "codigo") |>
    dplyr::filter(!is.na(nesting_suitability)) |>
    dplyr::mutate(HN_LC_pixel = weight * nesting_suitability) |>
    dplyr::group_by(cell, x, y, latitud, longitud, species) |>
    dplyr::summarise(HN_pixel = sum(HN_LC_pixel, na.rm = TRUE), .groups = "drop")

  # HF big (paralelo)
  HF_cells_big <- coords_big |>
    dplyr::select(cell, x, y, relative_exp_decay) |>
    dplyr::distinct(cell, .keep_all = TRUE)

  if (nrow(HF_cells_big) > 0) {
    HF_vals_big <- foreach::foreach(i = 1:nrow(HF_cells_big), .packages = "dplyr", .combine = "c") %dopar% {
      x_n <- HF_cells_big$x[i]; y_n <- HF_cells_big$y[i]

      resources <- df |>
        dplyr::mutate(dist_from_nest = sqrt((x - x_n)^2 + (y - y_n)^2)) |>
        dplyr::filter(dist_from_nest <= cfg$radius_factor * cfg$typical_foraging_dist_big) |>
        dplyr::mutate(exp_decay_from_nest = exp(-dist_from_nest / cfg$typical_foraging_dist_big))

      s <- sum(resources$exp_decay_from_nest, na.rm = TRUE)
      resources <- resources |>
        dplyr::mutate(relative_exp_decay_from_nest = if (s > 0) exp_decay_from_nest / s else 0)

      HF_value <- resources |>
        dplyr::left_join(HF_big, by = "codigo") |>
        dplyr::filter(!is.na(foraging_suitability)) |>
        dplyr::mutate(product_pixel = foraging_suitability * weight * relative_exp_decay_from_nest) |>
        dplyr::group_by(species, season, fl_resource_weight) |>
        dplyr::summarise(sum_product_pixel = sum(product_pixel, na.rm = TRUE), .groups = "drop") |>
        dplyr::mutate(product_season_i = sum_product_pixel * fl_resource_weight) |>
        dplyr::group_by(species) |>
        dplyr::summarise(sum_product_species = sum(product_season_i, na.rm = TRUE), .groups = "drop") |>
        dplyr::summarise(HF_pixel_final = sum(sum_product_species, na.rm = TRUE)) |>
        dplyr::pull(HF_pixel_final)

      if (length(HF_value) == 0 || is.na(HF_value)) 0 else HF_value
    }
    HF_cells_big$HF_pixel <- HF_vals_big
  } else {
    HF_cells_big$HF_pixel <- numeric(0)
  }

  # HF small (paralelo)
  HF_cells_small <- coords_small |>
    dplyr::select(cell, x, y, relative_exp_decay) |>
    dplyr::distinct(cell, .keep_all = TRUE)

  if (nrow(HF_cells_small) > 0) {
    HF_vals_small <- foreach::foreach(i = 1:nrow(HF_cells_small), .packages = "dplyr", .combine = "c") %dopar% {
      x_n <- HF_cells_small$x[i]; y_n <- HF_cells_small$y[i]

      resources <- df |>
        dplyr::mutate(dist_from_nest = sqrt((x - x_n)^2 + (y - y_n)^2)) |>
        dplyr::filter(dist_from_nest <= cfg$radius_factor * cfg$typical_foraging_dist_small) |>
        dplyr::mutate(exp_decay_from_nest = exp(-dist_from_nest / cfg$typical_foraging_dist_small))

      s <- sum(resources$exp_decay_from_nest, na.rm = TRUE)
      resources <- resources |>
        dplyr::mutate(relative_exp_decay_from_nest = if (s > 0) exp_decay_from_nest / s else 0)

      HF_value <- resources |>
        dplyr::left_join(HF_small, by = "codigo") |>
        dplyr::filter(!is.na(foraging_suitability)) |>
        dplyr::mutate(product_pixel = foraging_suitability * weight * relative_exp_decay_from_nest) |>
        dplyr::group_by(species, season, fl_resource_weight) |>
        dplyr::summarise(sum_product_pixel = sum(product_pixel, na.rm = TRUE), .groups = "drop") |>
        dplyr::mutate(product_season_i = sum_product_pixel * fl_resource_weight) |>
        dplyr::group_by(species) |>
        dplyr::summarise(sum_product_species = sum(product_season_i, na.rm = TRUE), .groups = "drop") |>
        dplyr::summarise(HF_pixel_final = sum(sum_product_species, na.rm = TRUE)) |>
        dplyr::pull(HF_pixel_final)

      if (length(HF_value) == 0 || is.na(HF_value)) 0 else HF_value
    }
    HF_cells_small$HF_pixel <- HF_vals_small
  } else {
    HF_cells_small$HF_pixel <- numeric(0)
  }

  # POS
  POS_big <- HN_big |>
    dplyr::left_join(HF_cells_big |> dplyr::select(cell, relative_exp_decay, HF_pixel), by = "cell") |>
    dplyr::filter(!is.na(HF_pixel) & !is.na(relative_exp_decay) & !is.na(HN_pixel)) |>
    dplyr::mutate(val = HN_pixel * HF_pixel * relative_exp_decay) |>
    dplyr::summarise(total = sum(val, na.rm = TRUE)) |>
    dplyr::pull(total)

  POS_small <- HN_small |>
    dplyr::left_join(HF_cells_small |> dplyr::select(cell, relative_exp_decay, HF_pixel), by = "cell") |>
    dplyr::filter(!is.na(HF_pixel) & !is.na(relative_exp_decay) & !is.na(HN_pixel)) |>
    dplyr::mutate(val = HN_pixel * HF_pixel * relative_exp_decay) |>
    dplyr::summarise(total = sum(val, na.rm = TRUE)) |>
    dplyr::pull(total)

  list(big = POS_big, small = POS_small)
}
