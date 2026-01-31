library(wesanderson)
library(mapSpain)
library(raster)
library(ggplot2)
library(tidyverse)
# Los puntos que no aparecen en el mapa corresponden con localizaciones donde
# hay agua

country <- esp_get_country()
lines <- esp_get_can_box()

# Plot municipalities

andalucia <- esp_get_ccaa("01")

Results_Lonsdorf_validation <- readr::read_csv('resources/from_raster_CORINE/raster_125/Lonsdorf_results_2025-06-09_20-24-40.csv')


ggplot() +
  geom_point(data=Results_Lonsdorf_validation, aes(x = longitude, y = latitude, color = Lonsdorf_small),size=2, alpha = 0.5)+
  geom_sf(data=andalucia, fill=NA, linewidth = 1) +
  scale_color_gradientn(colors = rev(wes_palette("Zissou1", type = "continuous")))+
  theme_bw()+
  labs(title = "Lonsdorf's service map (small bees)", x = "Longitude", y = "Latitude",
       color = "Score")

