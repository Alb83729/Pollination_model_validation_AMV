# R/spatial_prep.R -------------------------------------------------------

ensure_raster_crs <- function(r) {
  if (is.na(raster::crs(r))) {
    raster::crs(r) <- "+proj=utm +zone=30 +datum=WGS84"
  }
  r
}

make_point_wgs84 <- function(lon, lat) {
  sf::st_point(c(lon, lat)) |>
    sf::st_sfc(crs = 4326)
}

project_point_to_raster <- function(point_wgs84, r) {
  sf::st_transform(point_wgs84, crs = raster::crs(r))
}

make_buffer_m <- function(point_proj, radius_m) {
  # CRS proyectado en metros;
  # Evita conversiones aproximadas de metros a grados
  sf::st_buffer(point_proj, dist = radius_m)
}

crop_mask_resample <- function(r, buffer_sf, resolution_m) {
  buffer_sp <- methods::as(buffer_sf, "Spatial")

  r_crop    <- raster::crop(r, raster::extent(buffer_sp))
  r_visible <- raster::mask(r_crop, buffer_sp)

  r_trim    <- raster::trim(r_visible)
  r_disk    <- raster::mask(r_trim, buffer_sp)

  ref <- raster::raster(
    raster::extent(r_crop),
    res = resolution_m,
    crs = raster::crs(r_crop)
  )

  r_crop_resampled    <- raster::resample(r_crop, ref, method = "ngb")
  r_visible_resampled <- raster::mask(r_crop_resampled, buffer_sp)

  r_disk_resampled <- raster::mask(raster::trim(r_visible_resampled), buffer_sp, updatevalue = NA)

  list(
    buffer_sp = buffer_sp,
    crop = r_crop,
    visible = r_visible,
    disk = r_disk,
    disk_resampled = r_disk_resampled
  )
}

raster_to_corine_df <- function(r_disk_resampled, legend_sf) {
  df <- as.data.frame(r_disk_resampled, xy = TRUE, na.rm = TRUE)
  colnames(df) <- c("x", "y", "value")

  df |>
    dplyr::left_join(legend_sf, by = c("value" = "Value"))
}
