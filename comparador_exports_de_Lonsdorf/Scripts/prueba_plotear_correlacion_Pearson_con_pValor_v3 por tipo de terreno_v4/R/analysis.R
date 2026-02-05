# R/analysis.R -----------------------------------------------------------

pearson_by_habitat <- function(df, habitat_col, x_col, y_col) {
  h <- rlang::sym(habitat_col)
  x <- rlang::sym(x_col)
  y <- rlang::sym(y_col)
  
  df %>%
    dplyr::group_by(!!h) %>%
    dplyr::summarise(
      n_puntos  = dplyr::n(),
      r_pearson = suppressWarnings(stats::cor(!!x, !!y, use = "complete.obs")),
      p_valor   = dplyr::if_else(
        dplyr::n() > 2,
        suppressWarnings(stats::cor.test(!!x, !!y)$p.value),
        NA_real_
      ),
      .groups = "drop"
    )
}
