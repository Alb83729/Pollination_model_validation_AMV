# R/shapes.R -------------------------------------------------------------

get_ccaa_shape <- function(ccaa_code) {
  # mapSpain devuelve objetos sf
  mapSpain::esp_get_ccaa(ccaa_code)
}
