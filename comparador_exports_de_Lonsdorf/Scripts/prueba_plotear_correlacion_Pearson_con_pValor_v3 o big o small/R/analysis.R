# R/analysis.R -----------------------------------------------------------

prepare_xy <- function(df, x_col, y_col) {
  d <- df |>
    dplyr::transmute(
      x = as.numeric(.data[[x_col]]),
      y = as.numeric(.data[[y_col]])
    ) |>
    dplyr::filter(is.finite(x), is.finite(y))
  
  if (nrow(d) < 3) {
    stop("No hay suficientes pares válidos (>=3) tras filtrar NA/Inf.", call. = FALSE)
  }
  d
}

run_correlation <- function(d, method, alternative) {
  cor.test(d$x, d$y, method = method, alternative = alternative)
}

run_regression <- function(d) {
  lm(y ~ x, data = d)
}

get_slope <- function(lm_model) {
  unname(coef(lm_model)[2])
}

analyze_pair <- function(df_merged, x_col, y_col, method, alternative) {
  d <- prepare_xy(df_merged, x_col, y_col)
  cor_res <- run_correlation(d, method, alternative)
  lm_res  <- run_regression(d)
  slope   <- get_slope(lm_res)
  
  list(data = d, cor_test = cor_res, lm = lm_res, slope = slope)
}
