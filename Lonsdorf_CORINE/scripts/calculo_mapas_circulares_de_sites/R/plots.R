# R/plots.R --------------------------------------------------------------

plot_corine_df <- function(corine_df) {
  ggplot(corine_df, aes(x = x, y = y, fill = as.factor(LABEL3))) +
    geom_tile() +
    labs(fill = "Category") +
    theme_minimal()
}

plot_features_tiles <- function(features_sf, legend_df) {
  ggplot(features_sf %>% left_join(legend_df, by = "codigo")) +
    geom_tile(aes(x = x, y = y, fill = LABEL3)) +
    theme_bw() +
    theme(legend.position = "bottom") +
    coord_equal()
}
