# main.R -----------------------------------------------------------------

source("scripts/calculo_mapas_circulares_de_sites/config.R")

source("scripts/calculo_mapas_circulares_de_sites/R/deps.R")
source("scripts/calculo_mapas_circulares_de_sites/R/io.R")
source("scripts/calculo_mapas_circulares_de_sites/R/spatial_prep.R")
source("scripts/calculo_mapas_circulares_de_sites/R/lonsdorf_core.R")
source("scripts/calculo_mapas_circulares_de_sites/R/plots.R")
source("scripts/calculo_mapas_circulares_de_sites/R/checks.R")

load_pkgs()
source_aux()

# ---- Load inputs
corine <- load_corine(cfg$raster_path_tif, cfg$legend_path_dbf)
CORINE_raster <- ensure_raster_crs(corine$raster)
CORINE_legend <- corine$legend

validation_data <- load_validation_points(cfg$validation_csv)
selected_sites_coordinates <- load_selected_sites(cfg)

expert <- load_expert_tables(cfg$expert_table_csv)

# ---- Site selection
site_i <- cfg$site_i
lat_i <- as.numeric(selected_sites_coordinates$latitude[site_i])
lon_i <- as.numeric(selected_sites_coordinates$longitude[site_i])

# ---- Spatial prep: buffer view (for visibility / optional export)
pt_wgs84 <- make_point_wgs84(lon_i, lat_i)
pt_proj  <- project_point_to_raster(pt_wgs84, CORINE_raster)
buffer   <- make_buffer_m(pt_proj, cfg$buffer_radius_m)

prep <- crop_mask_resample(CORINE_raster, buffer, cfg$resolution_raster_m)

if (isTRUE(cfg$do_plots)) {
  plot(CORINE_raster)
  plot(prep$disk_resampled)
}

# ---- CORINE df (optional)
CORINE_df <- raster_to_corine_df(prep$disk_resampled, CORINE_legend)

if (isTRUE(cfg$export_corine_df)) {
  write.csv(CORINE_df, "CORINE_transformed_dataframe.csv", row.names = FALSE)
}

if (isTRUE(cfg$do_plots)) {
  print(plot_corine_df(CORINE_df))
}

# ---- Parallel
doParallel::registerDoParallel(cores = cfg$n_cores)

# ---- Lonsdorf computation for one site ("Gráficos para un site")
circle_radius <- 2 * 2 * cfg$typical_foraging_dist_big
resolution_raster <- 150

features_sf <- prepare_features_for_site(CORINE_raster, lat_i, lon_i, circle_radius, resolution_raster)

legend_df <- CORINE_legend %>%
  dplyr::select(Value, LABEL3) %>%
  dplyr::rename(codigo = Value)

if (isTRUE(cfg$do_plots)) {
  print(plot_features_tiles(features_sf, legend_df))
}

df_features <- features_to_weighted_df(features_sf)

# centro por bbox (método)
x_center <- (max(features_sf$x) + min(features_sf$x)) / 2
y_center <- (max(features_sf$y) + min(features_sf$y)) / 2

df_features <- add_center_and_distances(df_features, x_center, y_center, lat_i, lon_i)

# ---- Big coords decay (sin filtrar, como en big)
big <- compute_coords_decay(
  df_features,
  typical_foraging_dist = cfg$typical_foraging_dist_big,
  max_dist = NULL
)
coordinates4big <- big$df
sum_exp_distances_big <- big$sum_exp

# ---- Big coords decay
small <- compute_coords_decay(
  df_features,
  typical_foraging_dist = cfg$typical_foraging_dist_big,
  max_dist = 2 * cfg$typical_foraging_dist_small
)
coordinates4small <- small$df
sum_exp_distances_small <- small$sum_exp

# ---- HN
HN_pixel_big <- compute_HN_pixel(coordinates4big, expert$NS_big)
HN_pixel_small <- compute_HN_pixel(coordinates4small, expert$NS_small)

if (isTRUE(cfg$do_plots)) {
  print(
    ggplot(HN_pixel_big) +
      geom_tile(aes(x = x, y = y, fill = HN_pixel)) +
      theme_bw() + coord_equal()
  )
}

# ---- Sanity checks (ejemplo)
check_no_duplicate_cells(df_features, cols = c("x","y","codigo"))

# ---- HF big (foreach)
HF_pixel_big <- compute_HF_for_pixels_big(
  df_features_within_circle_sf = coordinates4big, # aquí ya contiene relative_exp_decay, etc.
  HF_table_big = expert$HF_big,
  typical_foraging_dist_big = cfg$typical_foraging_dist_big,
  sum_exp_distances_big = sum_exp_distances_big,
  n_cores = cfg$n_cores
)

if (isTRUE(cfg$do_plots)) {
  print(
    ggplot(HF_pixel_big) +
      geom_tile(aes(x = x, y = y, fill = HF_pixel)) +
      theme_bw() + coord_equal()
  )
}
