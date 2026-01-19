library(dplyr)
library(ggplot2)

# 1. CARGA DE ARCHIVOS
# ----------------------------------------------------------------------
# Archivo 1: Resultados del modelo (CORINE 125m)
lonsdorf_results <- read.csv('Data/datosDel09062025/Lonsdorf_results_2025-06-09_20-24-40_rast125.csv', header = TRUE)

# Archivo 2: Datos de campo y Hábitat
tabla_campo <- read.csv('Data/Junta_validation_data.csv', header = TRUE)

# 2. PREPARACIÓN Y LIMPIEZA DE DATOS (Común para ambos análisis)
# ----------------------------------------------------------------------
emplazamientos <- tabla_campo %>%
  select(Latitud, Longitud, Habitat, Abundancia_x_min) %>%
  mutate(
    # Convertir abundancia: cambiar coma por punto y pasar a numérico
    Abundancia_x_min = as.numeric(gsub(",", ".", Abundancia_x_min))
  ) %>%
  # Corrección de coordenadas invertidas (específicamente detectadas en Olivar)
  mutate(
    Lat_temp = Latitud,
    Latitud = ifelse(Lat_temp < 0, Longitud, Lat_temp),
    Longitud = ifelse(Lat_temp < 0, Lat_temp, Longitud)
  ) %>% 
  select(-Lat_temp)

# Unión de los resultados del modelo con los datos de campo
lonsdorf_val_hab <- lonsdorf_results %>%
  rename(Latitud = latitude, Longitud = longitude) %>%
  left_join(emplazamientos, by = c("Latitud", "Longitud"))


# ======================================================================
# BLOQUE 1: POLINIZADORES PEQUEÑOS (Lonsdorf_small)
# ======================================================================

cat("\n==============================================================\n")
cat("ANALISIS PARA: POLINIZADORES PEQUEÑOS (Lonsdorf_small)")
cat("\n==============================================================\n")

# Calcular correlación por hábitat para Small
pearson_small_habitat <- lonsdorf_val_hab %>%
  group_by(Habitat) %>%
  summarise(
    n_puntos = n(),
    r_pearson = cor(Lonsdorf_small, Abundancia_x_min, method = "pearson", use = "complete.obs")
  )

print(pearson_small_habitat)

# Gráfico para Small
plot_small <- ggplot(lonsdorf_val_hab, aes(x = Lonsdorf_small, y = Abundancia_x_min)) +
  geom_point(aes(color = Habitat)) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  facet_wrap(~Habitat, scales = "free") +
  labs(
    title = "VALIDACIÓN: Polinizadores Pequeños (Small)",
    subtitle = "Correlación entre predicción Lonsdorf_small y abundancia observada",
    x = "Predicción Modelo (Lonsdorf_small)",
    y = "Abundancia en Campo (x min)"
  ) +
  theme_minimal()

print(plot_small)


# ======================================================================
# BLOQUE 2: POLINIZADORES GRANDES (Lonsdorf_big)
# ======================================================================

cat("\n==============================================================\n")
cat("ANALISIS PARA: POLINIZADORES GRANDES (Lonsdorf_big)")
cat("\n==============================================================\n")

# Calcular correlación por hábitat para Big
pearson_big_habitat <- lonsdorf_val_hab %>%
  group_by(Habitat) %>%
  summarise(
    n_puntos = n(),
    r_pearson = cor(Lonsdorf_big, Abundancia_x_min, method = "pearson", use = "complete.obs")
  )

print(pearson_big_habitat)

# Gráfico para Big
plot_big <- ggplot(lonsdorf_val_hab, aes(x = Lonsdorf_big, y = Abundancia_x_min)) +
  geom_point(aes(color = Habitat)) +
  geom_smooth(method = "lm", se = FALSE, color = "darkgreen") +
  facet_wrap(~Habitat, scales = "free") +
  labs(
    title = "VALIDACIÓN: Polinizadores Grandes (Big)",
    subtitle = "Correlación entre predicción Lonsdorf_big y abundancia observada",
    x = "Predicción Modelo (Lonsdorf_big)",
    y = "Abundancia en Campo (x min)"
  ) +
  theme_minimal()

print(plot_big)
