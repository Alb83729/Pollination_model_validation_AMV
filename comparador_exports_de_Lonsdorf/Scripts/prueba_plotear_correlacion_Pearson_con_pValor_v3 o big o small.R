# Cargar las librerías necesarias
library(ggplot2)
library(dplyr)
# Leer los archivos CSV 17/01/2026: Aquí da igual low res o high res, son los dos a 125, pero vamos a graficar por un lado la comparación entre los small, y por otro lado solo los big
data_LowRes <- read.csv('Data/datosDel09062025/Lonsdorf_results_2025-06-09_20-24-40_rast125.csv', header = TRUE)
data_HighRes <- read.csv('Data/DeCorreoAlfon11062025/Junta_validation_data_outputs.csv', header = TRUE)

# Convertir las columnas V5 y V6 a numérico
# data_LowRes$V5 <- as.numeric(as.character(data_LowRes$V5))
# data_LowRes$V6 <- as.numeric(as.character(data_LowRes$V6))
# data_HighRes$V5 <- as.numeric(as.character(data_HighRes$V5))
# data_HighRes$V6 <- as.numeric(as.character(data_HighRes$V6))

# P R I M E R O LOS SMALL POLLINATORS

# Crear las columnas Lonsdorf_Medio SOLO PARA COMPARAR LOS SMALL DE AMBOS ARCHIVOS
data_LowRes$Lonsdorf_Medio <- data_LowRes$Lonsdorf_small
data_HighRes$Lonsdorf_Medio <- data_HighRes$Lonsdorf_small

# Crear un dataframe para la nube de puntos
df_comparison <- data.frame(
  Lonsdorf_Medio_LowRes = data_LowRes$Lonsdorf_Medio,
  Lonsdorf_Medio_HighRes = data_HighRes$Lonsdorf_Medio
)

# Generar la nube de puntos con la recta de regresión
ggplot(df_comparison, aes(x = Lonsdorf_Medio_LowRes, y = Lonsdorf_Medio_HighRes)) +
  geom_point() +
  geom_smooth(method = 'lm', se = FALSE, color = 'blue') +
  labs(title = 'Comparación de Lonsdorf Polinizadores pequeños',
       x = 'Lonsdorf_Medio (Polinizadores pequeños resultados CORINE)',
       y = 'Lonsdorf_Medio (Polinizadores pequeños resultados JdA)') +
  theme_minimal()

# Calcular la correlación de Pearson y su p-valor
cor_test_result <- cor.test(df_comparison$Lonsdorf_Medio_LowRes, df_comparison$Lonsdorf_Medio_HighRes,method = "pearson",alternative = "greater")

# Imprimir el resultado de la prueba de correlación en consola
print(cor_test_result)

# Crear un gráfico adicional para mostrar la correlación de Pearson
# df_correlation <- data.frame(
#   Correlation = cor_test_result$estimate,
#   PValue = cor_test_result$p.value
# )
# 
# ggplot(df_correlation, aes(x = factor(1), y = Correlation)) +
#   geom_bar(stat = 'identity') +
#   geom_text(aes(label = sprintf("cor. de Pearson = %.15f\np-valor = %.18e", Correlation, PValue)), vjust = -0.5) +
#   labs(title = '                                   Correlación de Pearson entre Lonsdorf_Medio de ambos archivos',
#        x = '',
#        y = 'Correlación de Pearson') +
#   theme_minimal() +
#   ylim(-1, 2)

# Ajustar el modelo de regresión lineal
linear_model <- lm(Lonsdorf_Medio_HighRes ~ Lonsdorf_Medio_LowRes, data = df_comparison)

# Obtener la pendiente de la regresión
slope <- coef(linear_model)[2]

# Imprimir la pendiente en consola
print(paste("Pendiente de la recta de regresión:", slope))

# S E G U N D O LOS BIG POLLINATORS (bumblebees)

# Crear las columnas Lonsdorf_Medio SOLO PARA COMPARAR LOS SMALL DE AMBOS ARCHIVOS
data_LowRes$Lonsdorf_Medio_big <- data_LowRes$Lonsdorf_big
data_HighRes$Lonsdorf_Medio_big <- data_HighRes$Lonsdorf_big

# Crear un dataframe para la nube de puntos
df_comparison <- data.frame(
  Lonsdorf_Medio_LowRes_big = data_LowRes$Lonsdorf_Medio_big,
  Lonsdorf_Medio_HighRes_big = data_HighRes$Lonsdorf_Medio_big
)

# Generar la nube de puntos con la recta de regresión
ggplot(df_comparison, aes(x = Lonsdorf_Medio_LowRes_big, y = Lonsdorf_Medio_HighRes_big)) +
  geom_point() +
  geom_smooth(method = 'lm', se = FALSE, color = 'blue') +
  labs(title = 'Comparación de Lonsdorf Polinizadores grandes',
       x = 'Lonsdorf_Medio (Polinizadores grandes resultados CORINE)',
       y = 'Lonsdorf_Medio (Polinizadores pequeños resultados JdA)') +
  theme_minimal()

# Calcular la correlación de Pearson y su p-valor
cor_test_result <- cor.test(df_comparison$Lonsdorf_Medio_LowRes_big, df_comparison$Lonsdorf_Medio_HighRes_big,method = "pearson",alternative = "greater")

# Imprimir el resultado de la prueba de correlación en consola
print(cor_test_result)

# Crear un gráfico adicional para mostrar la correlación de Pearson
# df_correlation <- data.frame(
#   Correlation = cor_test_result$estimate,
#   PValue = cor_test_result$p.value
# )
# 
# ggplot(df_correlation, aes(x = factor(1), y = Correlation)) +
#   geom_bar(stat = 'identity') +
#   geom_text(aes(label = sprintf("cor. de Pearson = %.15f\np-valor = %.18e", Correlation, PValue)), vjust = -0.5) +
#   labs(title = '                                   Correlación de Pearson entre Lonsdorf_Medio de ambos archivos',
#        x = '',
#        y = 'Correlación de Pearson') +
#   theme_minimal() +
#   ylim(-1, 2)

# Ajustar el modelo de regresión lineal
linear_model <- lm(Lonsdorf_Medio_HighRes_big ~ Lonsdorf_Medio_LowRes_big, data = df_comparison)

# Obtener la pendiente de la regresión
slope <- coef(linear_model)[2]

# Imprimir la pendiente en consola
print(paste("Pendiente de la recta de regresión:", slope))
