# R/plots.R --------------------------------------------------------------

make_scatter_plot <- function(d, title, x_label, y_label, add_regression = TRUE) {
  p <- ggplot2::ggplot(d, ggplot2::aes(x = x, y = y)) +
    ggplot2::geom_point() +
    ggplot2::labs(title = title, x = x_label, y = y_label) +
    ggplot2::theme_minimal()
  
  if (isTRUE(add_regression)) {
    p <- p + ggplot2::geom_smooth(method = "lm", se = FALSE)
  }
  p
}
