
create_df_features_within_circle_sf <- function(features_dentro_circulo_sf,resolution_raster){

  # df_features_within_circle_sf <- NULL
  #
  # codigos_unicos <- unique(features_dentro_circulo_sf$Codigo)
  #
  # for(codigo in codigos_unicos) {
  #
  #   df_percentage4codigo_point <- df_percentage4codigo_inside_circle(features_dentro_circulo_sf,
  #                                                                    codigo,
  #                                                                    resolution_raster)
  #
  #   df_features_within_circle_sf <- dplyr::bind_rows(df_features_within_circle_sf,
  #                                                    df_percentage4codigo_point)
  #
  #
  # }

  # Parallelization

  codigos_unicos <- unique(features_dentro_circulo_sf$Codigo)

  # Ejecutar en paralelo
  df_list <- foreach(codigo = codigos_unicos, .combine = 'rbind', .export = 'df_percentage4codigo_inside_circle') %dopar% {
    df_percentage4codigo_point <- df_percentage4codigo_inside_circle(features_dentro_circulo_sf,
                                                                     codigo,
                                                                     resolution_raster)
    return(df_percentage4codigo_point)
  }

  # Combinar los resultados
  df_features_within_circle_sf <- do.call(bind_rows, df_list)


  # # Sanity check
  #
  # check_weights <- df_features_within_circle_sf %>% group_by(x,y) %>%
  #   count(wt = weight) %>% filter(n>1)
  #
  # df_features_within_circle_sf %>%
  #   filter((x == check_weights$x[1]) & (y %in% check_weights$y[1])) %>%
  #   arrange(x,y)

  # Sum of weights is not normalized properly

  df_features_within_circle_sf_norm <- df_features_within_circle_sf %>%
    group_by(x, y) %>%
    mutate(total_weight = sum(weight)) %>%
    mutate(weight = ifelse(total_weight > 1, weight / total_weight, weight)) %>%
    dplyr::select(-total_weight) %>%
    ungroup()

  # # Sanity check
  # df_features_within_circle_sf_norm %>% group_by(x,y) %>%
  #   count(wt = weight) %>% mutate(n=round(n,2)) %>% filter(n>1)

  return(df_features_within_circle_sf_norm)

}
