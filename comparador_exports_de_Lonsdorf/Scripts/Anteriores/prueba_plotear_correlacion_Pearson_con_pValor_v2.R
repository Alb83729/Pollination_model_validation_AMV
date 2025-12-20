# Cargar las librerías necesarias
library(ggplot2)
library(dplyr)
# Leer los archivos CSV
data1 <- read.csv('Scripts/Lonsdorf_results_2024-07-09_17-15-47_62_con_5.csv', header = FALSE)
data2 <- read.csv('Scripts/Lonsdorf_results_2024-07-05_14-23-02_125.csv', header = FALSE)

# Convertir las columnas V5 y V6 a numérico
data1$V5 <- as.numeric(as.character(data1$V5))
data1$V6 <- as.numeric(as.character(data1$V6))
data2$V5 <- as.numeric(as.character(data2$V5))
data2$V6 <- as.numeric(as.character(data2$V6))

# Crear las columnas Lonsdorf_Medio
data1$Lonsdorf_Medio <- 0.5 * data1$V5 + 0.5 * data1$V6
data2$Lonsdorf_Medio <- 0.5 * data2$V5 + 0.5 * data2$V6

# Crear un dataframe para la nube de puntos
df_comparison <- data.frame(
  Lonsdorf_Medio1 = data1$Lonsdorf_Medio,
  Lonsdorf_Medio2 = data2$Lonsdorf_Medio
)

# Generar la nube de puntos
ggplot(df_comparison, aes(x = Lonsdorf_Medio1, y = Lonsdorf_Medio2)) +
  geom_point() +
  labs(title = 'Comparación de Lonsdorf_Medio',
       x = 'Lonsdorf_Medio (Archivo 1)',
       y = 'Lonsdorf_Medio (Archivo 2)') +
  theme_minimal()

# Calcular la correlación de Pearson y su p-valor
cor_test_result <- cor.test(df_comparison$Lonsdorf_Medio1, df_comparison$Lonsdorf_Medio2,method = "pearson",alternative = "greater")

# Imprimir el resultado de la prueba de correlación en consola
print(cor_test_result)

# Crear un gráfico adicional para mostrar la correlación de Pearson
df_correlation <- data.frame(
  Correlation = cor_test_result$estimate,
  PValue = cor_test_result$p.value
)

ggplot(df_correlation, aes(x = factor(1), y = Correlation)) +
  geom_bar(stat = 'identity') +
  geom_text(aes(label = sprintf("cor. de Pearson = %.15f\np-valor = %.18e", Correlation, PValue)), vjust = -0.5) +
  labs(title = '                                   Correlación de Pearson entre Lonsdorf_Medio de ambos archivos',
       x = '',
       y = 'Correlación de Pearson') +
  theme_minimal() +
  ylim(-1, 2)
