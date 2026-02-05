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

# Crear el gráfico de correlación
correlation_plot <- ggplot() +
  geom_text(aes(x = 1, y = 1, label = paste("Correlación de Pearson:", round(correlation, 2))), size = 6) +
  xlim(0.5, 1.5) + ylim(0.5, 1.5) +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(title = "Correlación de Pearson")

# Mostrar ambos gráficos
print(scatter_plot)
print(correlation_plot)
