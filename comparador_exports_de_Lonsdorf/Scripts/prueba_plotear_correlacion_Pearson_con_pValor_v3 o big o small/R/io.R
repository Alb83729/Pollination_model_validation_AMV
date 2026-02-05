# R/io.R -----------------------------------------------------------------

read_input_csvs <- function(low_path, high_path) {
  low  <- readr::read_csv(low_path,  show_col_types = FALSE)
  high <- readr::read_csv(high_path, show_col_types = FALSE)
  list(low = low, high = high)
}
