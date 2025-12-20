# Cargar las librerías necesarias
library(ggplot2)
library(dplyr)

# Definir los nombres de los archivos .csv
file1 <- "Scripts/Lonsdorf_results_2024-07-09_17-15-47_62_con_5.csv"
file2 <- "Scripts/Lonsdorf_results_2024-07-05_14-23-02_125.csv"

# Leer los archivos .csv
data1 <- read.csv(file1)
data2 <- read.csv(file2)

# Asegurarse de que los archivos tienen al menos 6 columnas y 164 filas
if(ncol(data1) < 6 | nrow(data1) < 164 | ncol(data2) < 6 | nrow(data2) < 164) {
  stop("Ambos archivos deben tener al menos 6 columnas y 164 filas.")
}

# Seleccionar las primeras 164 filas y las dos últimas columnas
data1_selected <- data1[1:164, (ncol(data1)-1):ncol(data1)]
data2_selected <- data2[1:164, (ncol(data2)-1):ncol(data2)]

# Renombrar las columnas para claridad
colnames(data1_selected) <- c("Lonsdorf_big", "Lonsdorf_small")
colnames(data2_selected) <- c("Lonsdorf_big", "Lonsdorf_small")

# Calcular Lonsdorf_Medio
data1_selected <- data1_selected %>%
  mutate(Lonsdorf_Medio = 0.5 * Lonsdorf_big + 0.5 * Lonsdorf_small)

data2_selected <- data2_selected %>%
  mutate(Lonsdorf_Medio = 0.5 * Lonsdorf_big + 0.5 * Lonsdorf_small)

# Crear un dataframe combinado para la comparación
comparison_data <- data.frame(
  Lonsdorf_Medio_Archivo1 = data1_selected$Lonsdorf_Medio,
  Lonsdorf_Medio_Archivo2 = data2_selected$Lonsdorf_Medio
)
# Calcular la correlación de Pearson
correlation <- cor(comparison_data$Lonsdorf_Medio_Archivo1, comparison_data$Lonsdorf_Medio_Archivo2, method = 'pearson')
print(correlation)

# Crear la nube de puntos
scatter_plot <- ggplot(comparison_data, aes(x = Lonsdorf_Medio_Archivo1, y = Lonsdorf_Medio_Archivo2)) +
  geom_point() +
  labs(title = "Comparación de Lonsdorf_Medio entre Archivos",
       x = "Lonsdorf_Medio Archivo 1",
       y = "Lonsdorf_Medio Archivo 2") +
  theme_minimal()

# Calcular la correlación de Pearson y su p-valor
cor_test_result <- cor.test(df_comparison$Lonsdorf_Medio1, df_comparison$Lonsdorf_Medio2)

# Imprimir el resultado de la prueba de correlación en consola
print(cor_test_result)

# Crear un gráfico adicional para mostrar la correlación de Pearson
df_correlation <- data.frame(
  Correlation = cor_test_result$estimate,
  PValue = cor_test_result$p.value
)

ggplot(df_correlation, aes(x = factor(1), y = Correlation)) +
  geom_bar(stat = 'identity') +
  geom_text(aes(label = sprintf("r = %.2f\np = %.3f", Correlation, PValue)), vjust = -0.5) +
  labs(title = 'Correlación de Pearson entre Lonsdorf_Medio de ambos archivos',
       x = '',
       y = 'Correlación de Pearson') +
  theme_minimal() +
  ylim(-1, 1)

