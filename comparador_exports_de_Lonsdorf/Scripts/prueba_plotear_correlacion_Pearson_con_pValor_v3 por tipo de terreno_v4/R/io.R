# R/io.R -----------------------------------------------------------------

read_inputs <- function(cfg) {
  corine_path <- assert_file(cfg$corine_csv)
  junta_path  <- assert_file(cfg$junta_results_csv)
  hab_path    <- assert_file(cfg$habitats_csv)
  
  list(
    corine   = read.csv(corine_path, header = TRUE),
    junta    = read.csv(junta_path,  header = TRUE),
    habitats = read.csv(hab_path,    header = TRUE)
  )
}
