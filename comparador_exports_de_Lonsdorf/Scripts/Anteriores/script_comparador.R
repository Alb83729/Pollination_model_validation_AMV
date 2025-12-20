# Cargar las librerías necesarias
library(dplyr)

# Leer los archivos CSV (asegúrate de cambiar 'file1.csv' y 'file2.csv' por los nombres de tus archivos)
file1 <- "Scripts/Lonsdorf_results_2024-07-05_12-00-11_por_defecto.csv"
file2 <- "Scripts/Lonsdorf_results_2024-07-05_14-23-02_125.csv"

# Leer los datos de los archivos CSV, seleccionando solo las primeras 164 filas
data1 <- read.csv(file1, nrows = 164)
data2 <- read.csv(file2, nrows = 164)

# Seleccionar las dos últimas columnas de ambos archivos
columns_to_compare_file1 <- data1[, (ncol(data1)-1):ncol(data1)]
columns_to_compare_file2 <- data2[, (ncol(data2)-1):ncol(data2)]

# Comparar las dos últimas columnas
comparison <- columns_to_compare_file1 == columns_to_compare_file2

# Convertir a un data frame para una mejor visualización
comparison_df <- as.data.frame(comparison)

# Asignar nombres significativos a las columnas de comparación
colnames(comparison_df) <- c("Comparison_Column_5", "Comparison_Column_6")

# Imprimir el resultado de la comparación
print(comparison_df)
