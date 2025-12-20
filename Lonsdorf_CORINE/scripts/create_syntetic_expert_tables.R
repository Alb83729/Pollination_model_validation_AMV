
library(sf)
library(terra)
source("R-scripts/df_percentage4codigo_inside_circle.R")
source("R-scripts/features_within_circle.R")

# The input file geodatabase
gdb_path <- "Ecosistemas_2023.gdb"


layers <- st_layers(gdb_path)
print(layers)

Leyenda <- st_read(gdb_path, layer = "Leyenda")

ecosistemas2023 <- st_read(gdb_path, layer = "Ecosistemas2023")


###############################
###############################


# Identificar todos los códigos únicos y descripciones
codigos_tabla <- (ecosistemas2023$Codigo)
descripcion_tabla <- (ecosistemas2023$Descripcion)

unique(ecosistemas2023$Codigo)
unique(ecosistemas2023$Descripcion)

tabla_nesting_floral_empty <- tidyr::tibble(
  code = codigos_tabla,
  description = descripcion_tabla
) %>% unique() %>%
  mutate(nesting_suitability_small = NA,
         nesting_suitability_big = NA,
         foraging_suitability_small_season_1 = NA,
         foraging_suitability_big_season_1 = NA,
         fl_resource_weight_season_1= NA,
         foraging_suitability_small_season_2 = NA,
         foraging_suitability_big_season_2 = NA,
         fl_resource_weight_season_2= NA,
         foraging_suitability_small_season_3 = NA,
         foraging_suitability_big_season_3 = NA,
         fl_resource_weight_season_3= NA)

readr::write_csv(tabla_nesting_floral_empty,"Data/empty_expert_table_JdA.csv")


tabla_nesting_floral_syntetic <- tidyr::tibble(
  code = codigos_tabla,
  description = descripcion_tabla
) %>% unique() %>%
  mutate(nesting_suitability_small = runif(n(), 0, 1),
         nesting_suitability_big = runif(n(), 0, 1),
         foraging_suitability_small_season_1 = runif(n(), 0, 1),
         foraging_suitability_big_season_1 = runif(n(), 0, 1),
         fl_resource_weight_season_1= runif(n(), 0, 1),
         foraging_suitability_small_season_2 = runif(n(), 0, 1),
         foraging_suitability_big_season_2 = runif(n(), 0, 1),
         fl_resource_weight_season_2= runif(1, 0, 1 - fl_resource_weight_season_1),
         foraging_suitability_small_season_3 = runif(n(), 0, 1),
         foraging_suitability_big_season_3 = runif(n(), 0, 1),
         fl_resource_weight_season_3= 1 - (fl_resource_weight_season_1 + fl_resource_weight_season_2))

readr::write_csv(tabla_nesting_floral_syntetic,"Data/syntetic_expert_table_JdA.csv")
