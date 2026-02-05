# R/io.R -----------------------------------------------------------------

read_results <- function(csv_path) {
  df <- readr::read_csv(csv_path, show_col_types = FALSE)

  # Chequeos
  required <- c("longitude", "latitude")
  missing <- required[!required %in% names(df)]
  if (length(missing) > 0) {
    stop("Faltan columnas en results CSV: ", paste(missing, collapse = ", "), call. = FALSE)
  }

  df
}
