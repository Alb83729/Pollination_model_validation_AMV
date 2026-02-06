# main.R -----------------------------------------------------------------

source("Scripts/prueba_plotear_correlacion_Pearson_con_pValor_v3 por tipo de terreno_v4/config.R")
source("Scripts/prueba_plotear_correlacion_Pearson_con_pValor_v3 por tipo de terreno_v4/R/deps.R")
source("Scripts/prueba_plotear_correlacion_Pearson_con_pValor_v3 por tipo de terreno_v4/R/utils.R")
source("Scripts/prueba_plotear_correlacion_Pearson_con_pValor_v3 por tipo de terreno_v4/R/io.R")
source("Scripts/prueba_plotear_correlacion_Pearson_con_pValor_v3 por tipo de terreno_v4/R/normalize.R")
source("Scripts/prueba_plotear_correlacion_Pearson_con_pValor_v3 por tipo de terreno_v4/R/join.R")
source("Scripts/prueba_plotear_correlacion_Pearson_con_pValor_v3 por tipo de terreno_v4/R/analysis.R")
source("Scripts/prueba_plotear_correlacion_Pearson_con_pValor_v3 por tipo de terreno_v4/R/plots.R")

load_pkgs()

# 1) Cargar inputs
inputs <- read_inputs(cfg)

# 2) Normalización flexible
data_corine          <- preparar_tabla(inputs$corine)
data_junta_results   <- preparar_tabla(inputs$junta)
data_alfonso_habitats<- preparar_tabla(inputs$habitats)

# 3) Limpiar habitats (swap lat/lon si lat<0, dedup)
habitats_clean <- clean_habitats(data_alfonso_habitats)

# 4) Joins
data_final <- build_final_dataset(
  data_corine, data_junta_results, habitats_clean,
  join_keys = cfg$join_keys
)

# 5) Determinar columna de hábitat
hab_col <- get_habitat_colname(data_final)
if (is.null(hab_col)) stop("No se encontró columna de hábitat (habitat_oficial / habitat_oficial.x).", call. = FALSE)

# 6) Análisis SMALL
msg_section("RESULTADOS: POLINIZADORES PEQUEÑOS (lonsdorf_small)")
pearson_small <- pearson_by_habitat(data_final, hab_col, cfg$small_corine, cfg$small_junta)
print(pearson_small)

if (isTRUE(cfg$do_plots)) {
  p_small <- facet_scatter(data_final, hab_col, cfg$small_corine, cfg$small_junta, smooth_color = "blue")
  print(p_small)
}

# 7) Análisis BIG
msg_section("RESULTADOS: POLINIZADORES GRANDES (lonsdorf_big)")
pearson_big <- pearson_by_habitat(data_final, hab_col, cfg$big_corine, cfg$big_junta)
print(pearson_big)

if (isTRUE(cfg$do_plots)) {
  p_big <- facet_scatter(data_final, hab_col, cfg$big_corine, cfg$big_junta, smooth_color = "darkgreen")
  print(p_big)
}

# 8) Guardar plots (opcional)
if (isTRUE(cfg$save_plots) && isTRUE(cfg$do_plots)) {
  ensure_dir(cfg$out_dir)
  ggplot2::ggsave(file.path(cfg$out_dir, "facet_small.png"), p_small, width = 10, height = 7)
  ggplot2::ggsave(file.path(cfg$out_dir, "facet_big.png"),   p_big,   width = 10, height = 7)
  message("Plots guardados en: ", cfg$out_dir)
}
