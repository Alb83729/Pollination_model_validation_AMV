############################################################
# Script: 03_mapas_lonsdorf.R
# Objetivo: genera tres mapas (land-cover, nesting, floral)
# Aut.: <tu nombre> – TFG 2025
############################################################

# -------- 0) Librerías --------
if (!requireNamespace("terra"))      install.packages("terra")
if (!requireNamespace("tidyterra"))  install.packages("tidyterra")
if (!requireNamespace("ggplot2"))    install.packages("ggplot2")
if (!requireNamespace("viridis"))    install.packages("viridis")
if (!requireNamespace("dplyr"))      install.packages("dplyr")
if (!requireNamespace("readr"))      install.packages("readr")

library(terra)      # ráster rápido
library(tidyterra)  # terra + ggplot2
library(ggplot2)    # mapas bonitos
library(viridis)    # paletas
library(dplyr)
library(readr)

# -------- 1) Rutas de entrada (ajusta si cambian) --------
ras_path   <- "../raster_CORINE/U2018_CLC2018_V2020_20u1.tif"
nest_csv   <- "../Results/expert_table_JdA_V1.csv"  # misma tabla que ya usas
# (La función CopyOfload_expert_tables entrega una lista pero aquí
#  solo necesitamos las columnas "codigo", "nesting_suitability",
#  "foraging_suitability".)

# -------- 2) Cargar ráster de land-cover --------
lc <- rast(ras_path)

# -------- 3) Leer tablas expertas --------
expert_tbl <- read_csv(nest_csv, show_col_types = FALSE)

# Asegúrate de que existen:
# codigo | nesting_suitability | foraging_suitability
needed_cols <- c("codigo", "nesting_suitability", "foraging_suitability")
stopifnot(all(needed_cols %in% names(expert_tbl)))

# Construir matrices de reclasificación (from, to, becomes)
rc_nest <- cbind(from    = expert_tbl$codigo,
                 to      = expert_tbl$codigo,
                 becomes = expert_tbl$nesting_suitability)

rc_flor <- cbind(from    = expert_tbl$codigo,
                 to      = expert_tbl$codigo,
                 becomes = expert_tbl$foraging_suitability)

# -------- 4) Reclasificar --------
nesting  <- classify(lc, rc_nest, right = FALSE)
floral   <- classify(lc, rc_flor, right = FALSE)

# -------- 5) Función genérica para plot + guardar --------
save_map <- function(r, titulo, paleta = "viridis", archivo_png){
  g <- ggplot() +
    geom_spatraster(data = r) +
    scale_fill_viridis_c(option = paleta, na.value = "transparent") +
    labs(title = titulo, fill = NULL) +
    coord_equal() +
    theme_minimal() +
    theme(axis.text  = element_blank(),
          axis.title = element_blank(),
          axis.ticks = element_blank())
  ggsave(archivo_png, g, width = 7, height = 7, dpi = 300)
  message("Mapa guardado en ", archivo_png)
}

# -------- 6) Generar mapas --------
save_map(lc,      "Land-cover (CORINE 2018)",  "turbo",  "mapa_landcover.png")
save_map(nesting, "Índice de anidación (Nesting)", "viridis", "mapa_nesting.png")
save_map(floral,  "Índice de recursos florales",   "magma",  "mapa_floral.png")

message("✅ Todos los mapas creados con éxito.")
