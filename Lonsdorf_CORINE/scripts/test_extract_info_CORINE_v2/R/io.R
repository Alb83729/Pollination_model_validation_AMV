assert_file <- function(path, label) {
  p <- normalizePath(path, winslash = "/", mustWork = FALSE)
  if (!file.exists(p)) stop(label, " no existe: ", p, call. = FALSE)
  p
}

load_corine <- function(cfg) {
  tif <- assert_file(cfg$raster_path_tif, "Raster TIFF")
  dbf <- assert_file(cfg$legend_path_dbf, "Leyenda DBF")

  r <- raster::raster(tif)
  legend <- foreign::read.dbf(dbf, as.is = TRUE)

  # Normaliza columna Value
  if (!"Value" %in% names(legend) && "VALUE" %in% names(legend)) {
    legend <- dplyr::rename(legend, Value = VALUE)
  } else if (!"Value" %in% names(legend)) {
    num_cols <- vapply(legend, is.numeric, logical(1))
    if (any(num_cols)) {
      first_num <- names(legend)[which(num_cols)[1]]
      warning("Columna 'Value' no encontrada, usando: ", first_num, call. = FALSE)
      legend <- dplyr::rename(legend, Value = !!rlang::sym(first_num))
    } else {
      stop("Leyenda sin columna 'Value' ni columna numérica.", call. = FALSE)
    }
  }

  list(raster = r, legend = legend)
}

load_expert <- function(cfg) {
  p <- assert_file(cfg$expert_table_csv, "Tablas expertas")
  tabs <- CopyOfload_expert_tables(p)

  # Importante: si la función puede devolver lista posicional o nombrada.
  # Si devuelve posicional, adaptar aquí.
  if (is.null(names(tabs)) || any(names(tabs) == "")) {
    # fallback posicional (como el script original antiguo)
    list(
      NS_big   = tabs[[1]],
      NS_small = tabs[[2]],
      HF_big   = tabs[[3]],
      HF_small = tabs[[4]]
    )
  } else {
    list(
      NS_big   = tabs[["NS_big"]],
      NS_small = tabs[["NS_small"]],
      HF_big   = tabs[["HF_big"]],
      HF_small = tabs[["HF_small"]]
    )
  }
}

load_sites <- function(cfg) {
  if (!dir.exists(cfg$output_dir)) dir.create(cfg$output_dir, recursive = TRUE)

  if (isTRUE(cfg$compute_validation_points)) {
    p <- assert_file(cfg$validation_sites, "CSV sitios validación")
    sites <- readr::read_csv(p, show_col_types = FALSE) |>
      dplyr::select(dplyr::any_of(c("Sitio","Ano","Latitud","Longitud"))) |>
      dplyr::rename(site_id = Sitio, refYear = Ano, latitude = Latitud, longitude = Longitud) |>
      dplyr::mutate(latitude = as.numeric(latitude), longitude = as.numeric(longitude))
    return(sites)
  }

  if (isTRUE(cfg$compute_other_points)) {
    stop("compute_other_points=TRUE: añade aquí tu CSV y mapeo de columnas.", call. = FALSE)
  }

  stop("No hay fuentes de sitios activas.", call. = FALSE)
}
