# Cargar las librerías necesarias
library(ggplot2)
library(dplyr)

# Definir los nombres de los archivos .csv
file1 <- "Scripts/Lonsdorf_results_2024-07-05_12-00-11_por_defecto.csv"
file2 <- "Scripts/Lonsdorf_results_2024-07-05_14-23-02_125.csv"

# Leer los archivos .csv
data1 <- read.csv(file1)
data2 <- read.csv(file2)

# Asegurarse de que los archivos tienen al menos 6 columnas y 164 filas
if(ncol(data1) < 6 | nrow(data1) < 164 | ncol(data2) < 6 | nrow(data2) < 164) {
  stop("Ambos archivos deben tener al menos 6 columnas y 164 filas.")
}

# Seleccionar las dos últimas columnas de los primeros 164 filas
data1_selected <- data1[1:164, (ncol(data1)-1):ncol(data1)]
data2_selected <- data2[1:164, (ncol(data2)-1):ncol(data2)]

# Añadir una columna para identificar el origen de los datos
data1_selected$source <- "Archivo 1"
data2_selected$source <- "Archivo 2"

# Combinar los dos dataframes
combined_data <- rbind(data1_selected, data2_selected)

# Renombrar las columnas para facilidad de uso
colnames(combined_data) <- c("Var1", "Var2", "Source")

# Crear la nube de puntos
ggplot(combined_data, aes(x = Var1, y = Var2, color = Source)) +
  geom_point() +
  labs(title = "Nube de Puntos Comparativa",
       x = "Variable 1",
       y = "Variable 2") +
  theme_minimal()
