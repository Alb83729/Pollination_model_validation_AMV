setup_parallel <- function(n_cores) {
  foreach::registerDoSEQ()
  doParallel::registerDoParallel(cores = n_cores)
  message("Registrando ", n_cores, " núcleos.")
  on.exit(doParallel::stopImplicitCluster(), add = TRUE)
  invisible(TRUE)
}
