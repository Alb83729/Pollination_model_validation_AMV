
library(dplyr)
library(mapSpain)

path_file_validation_coordinates <- "Data/Junta_validation_data.csv"
validation_sites <- readr::read_csv(path_file_validation_coordinates)

fixed_validation_sites <- validation_sites

fixed_validation_sites$Latitud <- as.numeric(fixed_validation_sites$Latitud)
fixed_validation_sites$Longitud <- as.numeric(fixed_validation_sites$Longitud)

fixed_validation_sites$Longitud[fixed_validation_sites$Sitio=="ES1AZN01"] <-
  -6.168970

fixed_validation_sites$Latitud[fixed_validation_sites$Habitat=="Olivar"] <-
  as.numeric(validation_sites$Longitud[validation_sites$Habitat=="Olivar"])

fixed_validation_sites$Longitud[fixed_validation_sites$Habitat=="Olivar"] <-
  as.numeric(validation_sites$Latitud[validation_sites$Habitat=="Olivar"])

# Plot points
country <- esp_get_country()
lines <- esp_get_can_box()

# Plot municipalities

andalucia <- esp_get_ccaa("01")

ggplot() +
  geom_point(data=fixed_validation_sites, aes(x = Longitud, y = Latitud),size=2, alpha=0.3)+
  geom_sf(data=andalucia, fill=NA, linewidth = 1) +
  theme_bw()+
  labs(title = "Validation sites", x = "Longitude", y = "Latitude")

sites_validation_GEE <- dplyr::tibble(
  study_id = "validation_JdA",
  site_id = fixed_validation_sites$Sitio,
  latitude = fixed_validation_sites$Latitud,
  longitude = fixed_validation_sites$Longitud,
  startDate = paste0(fixed_validation_sites$Ano,"-01-01"),
  endDate = fixed_validation_sites$Ano,
  refYear = fixed_validation_sites$Ano

)


sites_validation_GEE_2019 <- sites_validation_GEE
sites_validation_GEE_2019$refYear[sites_validation_GEE_2019$refYear>2019] <- 2019


readr::write_csv(sites_validation_GEE, "Results/sites_validation_GEE.csv")
readr::write_csv(sites_validation_GEE_2019, "Results/sites_validation_GEE_2019.csv")
readr::write_csv(fixed_validation_sites, "Results/fixed_Junta_validation_data.csv")
