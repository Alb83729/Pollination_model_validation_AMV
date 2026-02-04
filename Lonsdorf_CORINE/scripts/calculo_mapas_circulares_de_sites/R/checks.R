# R/checks.R -------------------------------------------------------------

check_no_duplicate_cells <- function(df, cols = c("x","y","codigo")) {
  dup <- any(duplicated(df[, cols]))
  if (dup) warning("Hay duplicados en (x,y,codigo). Revisa el pipeline.", call. = FALSE)
  invisible(!dup)
}

check_weights_sum <- function(df, exp_col = "exp_decay", weight_col = "weight", sum_exp) {
  lhs <- sum(df[[weight_col]] * df[[exp_col]])
  if (!isTRUE(all.equal(lhs, sum_exp))) {
    warning("Sanity check falló: sum(weight*exp_decay) != sum_exp.", call. = FALSE)
  }
  invisible(TRUE)
}
