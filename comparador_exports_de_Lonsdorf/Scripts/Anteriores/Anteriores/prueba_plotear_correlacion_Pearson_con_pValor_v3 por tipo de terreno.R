library(dplyr)
library(ggplot2)

# 1. CARGAR LOS ARCHIVOS
# ----------------------------------------------------------------------
# Archivo 1: Resultados del modelo Lonsdorf (CORINE 125m)
lonsdorf_results <- read.csv('Data/datosDel09062025/Lonsdorf_results_2025-06-09_20-24-40_rast125.csv', header = TRUE)

# Archivo 2: Datos de campo, Hábitat y Validación
# (Suponiendo que se llama 'datos_campo.csv')
tabla_campo <- read.csv('Data/Junta_validation_data.csv', header = TRUE)

# 2. LIMPIEZA Y PREPARACIÓN DEL ARCHIVO DE CAMPO
# ----------------------------------------------------------------------
emplazamientos <- tabla_campo %>%
  # Seleccionamos Lat/Long, el Hábitat y la abundancia real observada para validar
  select(Latitud, Longitud, Habitat, Abundancia_x_min) %>%
  mutate(
    # Convertimos la abundancia a numérico (cambiando coma por punto si es necesario)
    Abundancia_x_min = as.numeric(gsub(",", ".", Abundancia_x_min))
  )

# Nota: Si el join falla en el "Olivar", es porque en ese archivo las 
# coordenadas están intercambiadas (Latitud < 0). Corregimos si es el caso:
emplazamientos <- emplazamientos %>%
  mutate(
    Lat_temp = Latitud,
    Latitud = ifelse(Lat_temp < 0, Longitud, Lat_temp),
    Longitud = ifelse(Lat_temp < 0, Lat_temp, Longitud)
  ) %>% select(-Lat_temp)

# 3. UNIÓN DE DATOS (Left Join)
# ----------------------------------------------------------------------
# Unimos los resultados del modelo con la información de hábitat
# Usamos las coordenadas como llave
lonsdorf_val_hab <- lonsdorf_results %>%
  rename(Latitud = latitude, Longitud = longitude) %>%
  left_join(emplazamientos, by = c("Latitud", "Longitud"))

# 4. CÁLCULO DE CORRELACIÓN POR HÁBITAT
# ----------------------------------------------------------------------
# Validamos el modelo (Lonsdorf_small) contra los datos reales (Abundancia_x_min)
pearson_por_habitat <- lonsdorf_val_hab %>%
  group_by(Habitat) %>%
  summarise(
    n_puntos = n(),
    r_pearson = cor(Lonsdorf_small, 
                    Abundancia_x_min, 
                    method = "pearson", 
                    use = "complete.obs")
  )

# Mostrar resultados
print("Coeficiente de Pearson: Modelo (Small) vs Observaciones de Campo")
print(pearson_por_habitat)

# 5. GRÁFICO DE VALIDACIÓN POR HÁBITAT
# ----------------------------------------------------------------------
ggplot(lonsdorf_val_hab, aes(x = Lonsdorf_small, y = Abundancia_x_min)) +
  geom_point(aes(color = Habitat)) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  facet_wrap(~Habitat, scales = "free") +
  labs(
    title = "Validación Lonsdorf Small por Tipo de Hábitat",
    x = "Predicción del Modelo (Lonsdorf)",
    y = "Abundancia Observada (Campo)"
  ) +
  theme_minimal()
