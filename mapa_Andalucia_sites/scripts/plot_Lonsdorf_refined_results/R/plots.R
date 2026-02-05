# R/plots.R --------------------------------------------------------------

make_lonsdorf_map <- function(results_df, region_sf, value_col,
                              title, xlab, ylab,
                              point_size = 2, point_alpha = 0.5) {

  if (!value_col %in% names(results_df)) {
    stop("La columna '", value_col, "' no existe en el dataframe de resultados.", call. = FALSE)
  }

  ggplot2::ggplot() +
    ggplot2::geom_point(
      data = results_df,
      ggplot2::aes(x = longitude, y = latitude, color = .data[[value_col]]),
      size = point_size,
      alpha = point_alpha
    ) +
    ggplot2::geom_sf(data = region_sf, fill = NA, linewidth = 1) +
    ggplot2::scale_color_gradientn(
      colors = rev(wesanderson::wes_palette("Zissou1", type = "continuous"))
    ) +
    ggplot2::theme_bw() +
    ggplot2::labs(
      title = title,
      x = xlab,
      y = ylab,
      color = "Score"
    )
}
