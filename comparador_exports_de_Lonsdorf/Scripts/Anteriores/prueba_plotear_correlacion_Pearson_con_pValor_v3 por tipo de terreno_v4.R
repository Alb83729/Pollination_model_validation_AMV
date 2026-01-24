library(dplyr)
library(ggplot2)

# ======================================================================
# 1. CARGA DE ARCHIVOS
# ======================================================================

data_corine <- read.csv('Data/datosDel09062025/Lonsdorf_results_2025-06-09_20-24-40_rast125.csv', header = TRUE)
data_junta_results <- read.csv('Data/DeCorreoAlfon11062025/Junta_validation_data_outputs.csv', header = TRUE)
data_alfonso_habitats <- read.csv('Data/Junta_validation_data.csv', header = TRUE)

# ======================================================================
# 2. NORMALIZACIÓN "FLEXIBLE"
# ======================================================================

preparar_tabla <- function(df) {
  df <- df %>% rename_with(tolower)
  if ("latitude" %in% names(df)) df <- df %>% rename(lat = latitude)
  else if ("latitud" %in% names(df)) df <- df %>% rename(lat = latitud)
  if ("longitude" %in% names(df)) df <- df %>% rename(lon = longitude)
  else if ("longitud" %in% names(df)) df <- df %>% rename(lon = longitud)
  if (any(grepl("habit", names(df)))) {
    col_hab <- names(df)[grepl("habit", names(df))][1]
    df <- df %>% rename(habitat_oficial = !!sym(col_hab))
  }
  return(df)
}

data_corine <- preparar_tabla(data_corine)
data_junta_results <- preparar_tabla(data_junta_results)
data_alfonso_habitats <- preparar_tabla(data_alfonso_habitats)

habitats_clean <- data_alfonso_habitats %>%
  select(lat, lon, habitat_oficial) %>%
  mutate(
    lat_temp = lat,
    lat = ifelse(lat_temp < 0, lon, lat_temp),
    lon = ifelse(lat_temp < 0, lat_temp, lon)
  ) %>% 
  select(-lat_temp) %>%
  distinct(lat, lon, .keep_all = TRUE)

data_comparacion <- data_corine %>%
  inner_join(data_junta_results, by = c("lat", "lon"), suffix = c("_corine", "_junta"))

data_final <- data_comparacion %>%
  left_join(habitats_clean, by = c("lat", "lon"))

# ======================================================================
# 3. ANÁLISIS: POLINIZADORES PEQUEÑOS (lonsdorf_small)
# ======================================================================

cat("\n==============================================================\n")
cat(" RESULTADOS: POLINIZADORES PEQUEÑOS (lonsdorf_small) \n")
cat("==============================================================\n")

pearson_small <- data_final %>%
  group_by(habitat_oficial.x) %>%
  summarise(
    n_puntos  = n(),
    r_pearson = cor(lonsdorf_small_corine, lonsdorf_small_junta, use = "complete.obs"),
    # Cálculo del p-valor: solo si hay más de 2 puntos para evitar errores
    p_valor   = if(n() > 2) cor.test(lonsdorf_small_corine, lonsdorf_small_junta)$p.value else NA
  )

print(pearson_small)

ggplot(data_final, aes(x = lonsdorf_small_corine, y = lonsdorf_small_junta)) +
  geom_point(aes(color = habitat_oficial.x), alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  facet_wrap(~habitat_oficial.x, scales = "free") +
  theme_minimal()

# ======================================================================
# 4. ANÁLISIS: POLINIZADORES GRANDES (lonsdorf_big)
# ======================================================================

cat("\n==============================================================\n")
cat(" RESULTADOS: POLINIZADORES GRANDES (lonsdorf_big) \n")
cat("==============================================================\n")

pearson_big <- data_final %>%
  group_by(habitat_oficial.x) %>%
  summarise(
    n_puntos  = n(),
    r_pearson = cor(lonsdorf_big_corine, lonsdorf_big_junta, use = "complete.obs"),
    # Cálculo del p-valor: solo si hay más de 2 puntos para evitar errores
    p_valor   = if(n() > 2) cor.test(lonsdorf_big_corine, lonsdorf_big_junta)$p.value else NA
  )

print(pearson_big)

ggplot(data_final, aes(x = lonsdorf_big_corine, y = lonsdorf_big_junta)) +
  geom_point(aes(color = habitat_oficial.x), alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, color = "darkgreen") +
  facet_wrap(~habitat_oficial.x, scales = "free") +
  theme_minimal()
