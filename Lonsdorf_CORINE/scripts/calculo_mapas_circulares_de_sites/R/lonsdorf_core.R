# R/lonsdorf_core.R ------------------------------------------------------

prepare_features_for_site <- function(CORINE_raster, lat, lon, circle_radius, resolution_raster) {
  CopyOffeatures_within_circle(
    CORINE_raster,
    lat, lon,
    circle_radius,
    resolution_raster
  )
}

features_to_weighted_df <- function(features_sf) {
  CopyOfcreate_df_features_within_circle_sf(features_sf)
}

add_center_and_distances <- function(df, x_center, y_center, lat, lon) {
  df$latitud <- lat
  df$longitud <- lon
  df$x_centro <- x_center
  df$y_centro <- y_center
  df$distancia_centro <- sqrt((df$x - x_center)^2 + (df$y - y_center)^2)
  df
}

compute_coords_decay <- function(df, typical_foraging_dist, max_dist = NULL) {
  d <- df
  if (!is.null(max_dist)) d <- dplyr::filter(d, distancia_centro <= max_dist)

  d <- d |>
    unique() |>
    dplyr::mutate(exp_decay = exp(-distancia_centro / typical_foraging_dist))

  sum_exp <- d |>
    dplyr::select(x, y, exp_decay) |>
    unique() |>
    dplyr::summarise(sum_exp = sum(exp_decay)) |>
    dplyr::pull(sum_exp)

  d$relative_exp_decay <- d$exp_decay / sum_exp

  list(df = d, sum_exp = sum_exp)
}

compute_HN_pixel <- function(coords_df, NS_table) {
  coords_df |>
    dplyr::left_join(NS_table, by = c("codigo")) |>
    dplyr::select(latitud, longitud, x, y, species, weight, nesting_suitability) |>
    dplyr::mutate(HN_LC_pixel = weight * nesting_suitability) |>
    dplyr::group_by(latitud, longitud, x, y, species) |>
    dplyr::count(wt = HN_LC_pixel) |>
    dplyr::rename(HN_pixel = n) |>
    dplyr::ungroup()
}

compute_HF_for_pixels_big <- function(
    df_features_within_circle_sf,
    HF_table_big,
    typical_foraging_dist_big,
    sum_exp_distances_big,
    n_cores
) {
  HF_pixel_big <- unique(df_features_within_circle_sf[, c("x", "y", "relative_exp_decay")])
  # cluster already registered outside
  results_big <- foreach::foreach(cell_i = 1:nrow(HF_pixel_big), .packages = c("dplyr")) %dopar% {
    x_c <- HF_pixel_big$x[cell_i]
    y_c <- HF_pixel_big$y[cell_i]

    df_temp <- df_features_within_circle_sf
    df_temp$distancia_centro <- sqrt((df_temp$x - x_c)^2 + (df_temp$y - y_c)^2)

    coords_cell <- df_temp |>
      dplyr::filter(distancia_centro <= 2 * typical_foraging_dist_big) |>
      unique()

    coords_cell$exp_decay <- exp(-coords_cell$distancia_centro / typical_foraging_dist_big)
    coords_cell$relative_exp_decay <- coords_cell$exp_decay / sum_exp_distances_big

    HF_val <- coords_cell |>
      dplyr::left_join(HF_table_big, by = c("codigo")) |>
      dplyr::mutate(product_pixel = foraging_suitability * weight * relative_exp_decay) |>
      dplyr::select(species, season, fl_resource_weight, product_pixel) |>
      dplyr::group_by(species, season, fl_resource_weight) |>
      dplyr::count(wt = product_pixel) |>
      dplyr::rename(sum_product_pixel = n) |>
      dplyr::ungroup() |>
      dplyr::mutate(product_season_i = sum_product_pixel * fl_resource_weight) |>
      dplyr::group_by(species) |>
      dplyr::count(wt = product_season_i) |>
      dplyr::rename(sum_product_season_i = n) |>
      dplyr::ungroup() |>
      dplyr::select(-species) |>
      dplyr::pull()

    HF_val
  }

  HF_pixel_big$HF_pixel <- unlist(results_big)
  HF_pixel_big
}
