# main.R -----------------------------------------------------------------

source("Scripts/prueba_plotear_correlacion_Pearson_con_pValor_v3 o big o small/config.R")
source("Scripts/prueba_plotear_correlacion_Pearson_con_pValor_v3 o big o small/R/deps.R")
source("Scripts/prueba_plotear_correlacion_Pearson_con_pValor_v3 o big o small/R/utils.R")
source("Scripts/prueba_plotear_correlacion_Pearson_con_pValor_v3 o big o small/R/io.R")
source("Scripts/prueba_plotear_correlacion_Pearson_con_pValor_v3 o big o small/R/align.R")
source("Scripts/prueba_plotear_correlacion_Pearson_con_pValor_v3 o big o small/R/analysis.R")
source("Scripts/prueba_plotear_correlacion_Pearson_con_pValor_v3 o big o small/R/plots.R")

load_pkgs()

# ---- Load
low_path  <- assert_file(cfg$lowres_csv)
high_path <- assert_file(cfg$highres_csv)

inputs <- read_input_csvs(low_path, high_path)
aligned <- align_datasets(inputs$low, inputs$high, cfg$id_candidates)

message("Modo de alineación: ", aligned$mode)
df_merged <- aligned$data

# columnas (tras alineación siempre terminan con _low/_high)
small_low  <- paste0(cfg$col_small, "_low")
small_high <- paste0(cfg$col_small, "_high")
big_low    <- paste0(cfg$col_big,   "_low")
big_high   <- paste0(cfg$col_big,   "_high")

# ---- SMALL
res_small <- analyze_pair(
  df_merged, small_low, small_high,
  method = cfg$cor_method,
  alternative = cfg$cor_alternative
)

p_small <- make_scatter_plot(
  res_small$data,
  title   = "Comparación de Lonsdorf – Polinizadores pequeños",
  x_label = "Lonsdorf_small (CORINE)",
  y_label = "Lonsdorf_small (JdA)",
  add_regression = TRUE
)

print(p_small)
print(res_small$cor_test)
print(paste("Pendiente (small):", res_small$slope))

# ---- BIG
res_big <- analyze_pair(
  df_merged, big_low, big_high,
  method = cfg$cor_method,
  alternative = cfg$cor_alternative
)

p_big <- make_scatter_plot(
  res_big$data,
  title   = "Comparación de Lonsdorf – Polinizadores grandes",
  x_label = "Lonsdorf_big (CORINE)",
  y_label = "Lonsdorf_big (JdA)",
  add_regression = TRUE
)

print(p_big)
print(res_big$cor_test)
print(paste("Pendiente (big):", res_big$slope))

# ---- Save plots (optional)
if (isTRUE(cfg$save_plots)) {
  ensure_dir(cfg$out_dir)
  ggplot2::ggsave(file.path(cfg$out_dir, "scatter_small.png"), p_small, width = 7, height = 5)
  ggplot2::ggsave(file.path(cfg$out_dir, "scatter_big.png"),   p_big,   width = 7, height = 5)
  message("Plots guardados en: ", cfg$out_dir)
}
