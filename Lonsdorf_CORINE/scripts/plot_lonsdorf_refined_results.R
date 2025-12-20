
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




Results_Lonsdorf_validation <- readr::read_csv("Results/Lonsdorf_results_2024-03-07_18-36-41.csv") %>%
  mutate(Lonsdorf_MEAN = 0.5*(Lonsdorf_small+Lonsdorf_big))




ggplot() +
  geom_point(data=Results_Lonsdorf_validation, aes(x = longitude, y = latitude, color = Lonsdorf_big),size=2, alpha = 0.5)+
  geom_sf(data=andalucia, fill=NA, linewidth = 1) +
  scale_color_gradientn(colors = rev(wes_palette("Zissou1", type = "continuous")))+
  theme_bw()+
  labs(title = "Lonsdorf's service map (bumblebees)", x = "Longitude", y = "Latitude",
       color = "Score")


ggplot() +
  geom_point(data=Results_Lonsdorf_validation, aes(x = longitude, y = latitude, color = Lonsdorf_small),size=2, alpha = 0.5)+
  geom_sf(data=andalucia, fill=NA, linewidth = 1) +
  scale_color_gradientn(colors = rev(wes_palette("Zissou1", type = "continuous")))+
  theme_bw()+
  labs(title = "Lonsdorf's service map (other wild bees)", x = "Longitude", y = "Latitude",
       color = "Score")


ggplot() +
  geom_point(data=Results_Lonsdorf_validation,
             aes(x = longitude, y = latitude,
                 color = 0.5*(Lonsdorf_small+Lonsdorf_big)),size=2, alpha = .8)+
  geom_sf(data=andalucia, fill=NA, linewidth = 1) +
  scale_color_gradientn(colors = rev(wes_palette("Zissou1", type = "continuous")))+
  theme_bw()+
  labs(title = "Lonsdorf's service map (average)", x = "Longitude", y = "Latitude",
       color = "Score")

ggplot() +
  geom_point(data=Results_Lonsdorf_validation, aes(x = Lonsdorf_big, y = Lonsdorf_small),size=2)+
  theme_bw()+
  labs(title = "Lonsdorf's service map", x = "Score (bumblebees)", y = "Score (other wild bees)",
       color = "Score")


library(tidyverse)
Results_Lonsdorf_all <- readr::read_csv("Results/Lonsdorf_results_2024-05-23_08-38-03.csv") %>%
  mutate(Lonsdorf_MEAN=0.5*(Lonsdorf_small+Lonsdorf_big)) %>% filter(!is.na(Lonsdorf_MEAN))

readr::write_csv(Results_Lonsdorf_all,"Results/Lonsdorf_MEAN_results_refined.csv")


ggplot() +
  geom_point(data=Results_Lonsdorf_all, aes(x = longitude, y = latitude, color = Lonsdorf_big),size=1)+
  geom_sf(data=andalucia, fill=NA, linewidth = 1) +
  scale_color_gradientn(colors = rev(wes_palette("Zissou1", type = "continuous")))+
  theme_bw()+
  labs(title = "Lonsdorf's service map (bumblebees)", x = "Longitude", y = "Latitude",
       color = "Score")


ggplot() +
  geom_point(data=Results_Lonsdorf_all %>% filter(site_id != 1), aes(x = longitude, y = latitude, color = Lonsdorf_small),size=1)+
  geom_sf(data=andalucia, fill=NA, linewidth = 1) +
  scale_color_gradientn(colors = rev(wes_palette("Zissou1", type = "continuous")))+
  theme_bw()+
  labs(title = "Lonsdorf's service map (other wild bees)", x = "Longitude", y = "Latitude",
       color = "Score")

ggplot() +
  geom_point(data=Results_Lonsdorf_all,
             aes(x = longitude, y = latitude, color = 0.5*(Lonsdorf_small+Lonsdorf_big)),size=1.25)+
  geom_sf(data=andalucia, fill=NA, linewidth = 1) +
  scale_color_gradientn(colors = rev(wes_palette("Zissou1", type = "continuous")))+
  theme_bw()+
  labs(title = "Lonsdorf's service map (average)", x = "Longitude", y = "Latitude",
       color = "Score")



ggplot() +
  geom_point(data=Results_Lonsdorf_all, aes(x = longitude, y = latitude, color = log(0.5*(Lonsdorf_small+Lonsdorf_big))),size=2)+
  geom_sf(data=andalucia, fill=NA, linewidth = 1) +
  scale_color_gradientn(colors = rev(wes_palette("Zissou1", type = "continuous")))+
  theme_bw()+
  labs(title = "Lonsdorf's service map (average)", x = "Longitude", y = "Latitude",
       color = "Ln(Score)")


ggplot(data=Results_Lonsdorf_all, aes(x = Lonsdorf_big, y = Lonsdorf_small),) +
  geom_point(size=2.5, alpha = 0.5)+
  geom_abline(slope = 1/3,intercept = 0, linetype ="dashed")+
  geom_smooth(method = "lm")+
  theme_bw()+
  labs(title = "Lonsdorf's service map", x = "Score (bumblebees)", y = "Score (other wild bees)",
       color = "Score")


library(raster)
df <- data.frame( x = Results_Lonsdorf_all$longitude,
                  y = Results_Lonsdorf_all$latitude,
                  l = Results_Lonsdorf_all$Lonsdorf_MEAN)

dfr <- rasterFromXYZ(df)  #Convert first two columns as lon-lat and third as value
plot(dfr)
dfr
