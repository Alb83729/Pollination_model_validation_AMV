source("scripts/test_extract_info_CORINE_v2/config.R")
source("scripts/test_extract_info_CORINE_v2/R/deps.R")
source("scripts/test_extract_info_CORINE_v2/R/io.R")
source("scripts/test_extract_info_CORINE_v2/R/crs.R")
source("scripts/test_extract_info_CORINE_v2/R/parallel.R")
source("scripts/test_extract_info_CORINE_v2/R/lonsdorf_site.R")

load_pkgs()
source_aux()

# IO
corine <- load_corine(cfg)
CORINE_raster <- ensure_raster_crs(corine$raster, cfg$fallback_epsg)
CORINE_legend <- corine$legend

expert <- load_expert(cfg)
sites  <- load_sites(cfg)

# CRS para sf transform
crs_raster_sf <- get_sf_crs_from_raster(CORINE_raster)

# Output filename
ts <- format(Sys.time(), "%Y-%m-%d_%H-%M-%S")
results_filename <- file.path(cfg$output_dir, paste0("Lonsdorf_results_", ts, ".csv"))

# Paralelo (se cierra aunque falle)
setup_parallel(cfg$n_cores)

# columnas salida
sites$Lonsdorf_big   <- NA_real_
sites$Lonsdorf_small <- NA_real_

message("Iniciando cálculo para ", nrow(sites), " sitios…")
tictoc::tic("Procesamiento Total Lonsdorf")

for (i in seq_len(nrow(sites))) {
  tictoc::tic(paste("Sitio", i))

  lat <- as.numeric(sites$latitude[i])
  lon <- as.numeric(sites$longitude[i])

  out <- compute_lonsdorf_for_site(
    CORINE_raster = CORINE_raster,
    legend_df = CORINE_legend,
    NS_big   = expert$NS_big,
    NS_small = expert$NS_small,
    HF_big   = expert$HF_big,
    HF_small = expert$HF_small,
    lat = lat, lon = lon,
    cfg = cfg,
    crs_raster_sf = crs_raster_sf
  )

  sites$Lonsdorf_big[i]   <- out$big
  sites$Lonsdorf_small[i] <- out$small

  # guardado incremental
  readr::write_csv(sites, results_filename)

  tictoc::toc()
}

tictoc::toc()
message("Resultados guardados en: ", results_filename)
print(head(sites))
