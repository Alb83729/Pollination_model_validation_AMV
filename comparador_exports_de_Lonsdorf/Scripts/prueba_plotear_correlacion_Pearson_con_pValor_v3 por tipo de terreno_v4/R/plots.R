# R/plots.R --------------------------------------------------------------

facet_scatter <- function(df, habitat_col, x_col, y_col, smooth_color) {
  h <- rlang::sym(habitat_col)
  x <- rlang::sym(x_col)
  y <- rlang::sym(y_col)
  
  ggplot2::ggplot(df, ggplot2::aes(x = !!x, y = !!y)) +
    ggplot2::geom_point(ggplot2::aes(color = !!h), alpha = 0.6) +
    ggplot2::geom_smooth(method = "lm", se = FALSE, color = smooth_color) +
    ggplot2::facet_wrap(ggplot2::vars(!!h), scales = "free") +
    ggplot2::theme_minimal()
}
