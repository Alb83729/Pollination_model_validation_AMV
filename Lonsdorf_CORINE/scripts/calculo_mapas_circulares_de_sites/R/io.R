# R/io.R -----------------------------------------------------------------

assert_file_exists <- function(path, label = "archivo") {
  p <- normalizePath(path, winslash = "/", mustWork = FALSE)
  if (!file.exists(p)) stop("No existe ", label, " en: ", p, call. = FALSE)
  invisible(p)
}

load_corine <- function(raster_path_tif, legend_path_dbf) {
  assert_file_exists(raster_path_tif, "raster TIFF")
  assert_file_exists(legend_path_dbf, "leyenda DBF")

  r <- raster::raster(raster_path_tif)
  legend <- sf::st_read(legend_path_dbf, options = "ENCODING=WINDOWS-1252", quiet = TRUE)

  list(raster = r, legend = legend)
}

load_validation_points <- function(validation_csv) {
  assert_file_exists(validation_csv, "CSV validación JdA")
  readr::read_csv(validation_csv, show_col_types = FALSE)
}

load_selected_sites <- function(cfg) {
  selected <- NULL

  if (isTRUE(cfg$compute_validation_points)) {
    assert_file_exists(cfg$validation_sites, "CSV sitios validación")
    validation_sites <- readr::read_csv(cfg$validation_sites, show_col_types = FALSE) |>
      dplyr::select(Sitio, Ano, Latitud, Longitud) |>
      dplyr::rename(site_id = Sitio, refYear = Ano, latitude = Latitud, longitude = Longitud)

    selected <- validation_sites
  } else if (isTRUE(cfg$compute_other_points)) {
    stop("compute_other_points=TRUE: añade aquí tu CSV de otros puntos.", call. = FALSE)
  } else {
    stop("No hay puntos seleccionados: activa compute_validation_points o compute_other_points.", call. = FALSE)
  }

  selected
}

load_expert_tables <- function(expert_table_csv) {
  assert_file_exists(expert_table_csv, "CSV tablas expertas")
  list_expert_tables <- CopyOfload_expert_tables(expert_table_csv)

  list(
    NS_big   = list_expert_tables[[1]],
    NS_small = list_expert_tables[[2]],
    HF_big   = list_expert_tables[[3]],
    HF_small = list_expert_tables[[4]]
  )
}
