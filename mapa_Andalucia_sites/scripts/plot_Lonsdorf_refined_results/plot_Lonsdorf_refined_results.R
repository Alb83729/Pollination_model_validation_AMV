# main.R -----------------------------------------------------------------

source("scripts/plot_Lonsdorf_refined_results/config.R")
source("scripts/plot_Lonsdorf_refined_results/R/deps.R")
source("scripts/plot_Lonsdorf_refined_results/R/utils.R")
source("scripts/plot_Lonsdorf_refined_results/R/io.R")
source("scripts/plot_Lonsdorf_refined_results/R/shapes.R")
source("scripts/plot_Lonsdorf_refined_results/R/plots.R")

load_pkgs()

csv_path <- assert_file(cfg$results_csv)
results  <- read_results(csv_path)

andalucia <- get_ccaa_shape(cfg$ccaa_code)

p <- make_lonsdorf_map(
  results_df = results,
  region_sf  = andalucia,
  value_col  = cfg$value_col,
  title = cfg$title,
  xlab  = cfg$xlab,
  ylab  = cfg$ylab,
  point_size  = cfg$point_size,
  point_alpha = cfg$point_alpha
)

print(p)
