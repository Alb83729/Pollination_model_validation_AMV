# R/align.R --------------------------------------------------------------

find_id_col <- function(df, candidates) {
  hit <- candidates[candidates %in% names(df)]
  if (length(hit) == 0) return(NULL)
  hit[1]
}

align_datasets <- function(low, high, id_candidates) {
  id_low  <- find_id_col(low,  id_candidates)
  id_high <- find_id_col(high, id_candidates)
  
  if (!is.null(id_low) && !is.null(id_high)) {
    low2  <- low  |> dplyr::rename(site_id = !!rlang::sym(id_low))
    high2 <- high |> dplyr::rename(site_id = !!rlang::sym(id_high))
    
    merged <- low2 |>
      dplyr::select(site_id, dplyr::everything()) |>
      dplyr::inner_join(
        high2 |> dplyr::select(site_id, dplyr::everything()),
        by = "site_id",
        suffix = c("_low", "_high")
      )
    
    return(list(data = merged, mode = "join_by_site_id"))
  }
  
  if (nrow(low) != nrow(high)) {
    stop(
      "No hay columna ID común para alinear y los datasets no tienen el mismo número de filas.\n",
      "LowRes: ", nrow(low), " filas; HighRes: ", nrow(high), " filas.",
      call. = FALSE
    )
  }
  
  merged <- dplyr::bind_cols(
    low  |> dplyr::rename_with(~ paste0(.x, "_low")),
    high |> dplyr::rename_with(~ paste0(.x, "_high"))
  )
  
  list(data = merged, mode = "row_order")
}
