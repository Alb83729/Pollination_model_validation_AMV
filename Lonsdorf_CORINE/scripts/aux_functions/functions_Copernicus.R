# get UTM zone number from latitude and longitude
# Special zones for Svalbard and Norway

getZones <- function(lon, lat){
  if(lat >= 72.0 & lat < 84.0){
    if(lon >= 0.0 & lon < 9.0){return(31)}
    if(lon >= 9.0 & lon < 21.0){return(33)}
    if(lon >= 21.0 & lon < 33.0){return(35)}
    if(lon >= 33.0 & lon < 42.0){return(37)}
  }

  if((lat >= 56 & lat < 64.0) & (lon >= 3 & lon <= 12)){return(32)}

  return(floor((lon + 180) / 6) + 1)
}


findEPSG <- function(longitude, latitude){
  zone <-  getZones(longitude, latitude)
  epsg_code = 32600 + round(zone,0)
  if (latitude < 0){# South
    epsg_code = epsg_code + 100
  }

  return(epsg_code)
}

##########################################
##########################################

square_latlon <- function(data,i,radius){ # radius in meters

  data_aux <- data[i,]

  # Setting existing coordinate as lat-long system
  cord.dec = SpatialPoints(cbind(data_aux$longitude, data_aux$latitude), proj4string = CRS("+proj=longlat"))

  # Transforming coordinate to those of CORINE: EPSG:3035

  cord.UTM <- spTransform(cord.dec, CRS(paste0("+init=epsg:",findEPSG(data_aux$longitude[1],data_aux$latitude[1]))))

  # Define the square plot by using the radius (i.e., half side)
  yPlus <- cord.UTM@coords[,2]+radius
  xPlus <- cord.UTM@coords[,1]+radius
  yMinus <- cord.UTM@coords[,2]-radius
  xMinus <- cord.UTM@coords[,1]-radius

  # calculate polygon coordinates for each plot centroid.
  square <- cbind(xMinus,yPlus,  # NW corner
                  xPlus, yPlus,  # NE corner
                  xPlus,yMinus,  # SE corner
                  xMinus,yMinus, # SW corner
                  xMinus,yPlus)  # NW corner again to close ploygon

  # Extract the plot ID information
  ID <- data_aux$site_id[1]
  a <- vector('list', length(2))
  # loop through each centroid value and create a polygon

  # this is where we match the ID to the new plot coordinates
  for (i in 1:nrow(data_aux)) {  # for each for in object data
    a[[i]]<-Polygons(list(Polygon(matrix(square[i, ], ncol=2, byrow=TRUE))), ID[i])
    # make it an Polygon object with the Plot_ID from object ID
  }

  # convert a to SpatialPolygon and assign CRS
  sq.polys <- SpatialPolygons(a,proj4string = CRS(paste0("+init=epsg:",
                                                         findEPSG(data_aux$longitude[1],data_aux$latitude[1]))))

  # Create SpatialPolygonDataFrame

  sq.polys.df <- SpatialPolygonsDataFrame(sq.polys, data.frame(id=ID, row.names=ID))
  sq.polys.df2 <-  st_as_sf(sq.polys.df,crs = 4326)
  sq.polys.df2 <- st_transform(sq.polys.df2, crs =  4326)

  return(list(sq.polys.df2[1],sq.polys.df[1]))

}

##########################################
##########################################

load_rasters_year <- function(year){

  if(year == 2015){
    #Forest_raster <- raster("COPERNICUS/2015/PROBAV_LC100_global_v3.0.1_2015-base_Forest-Type-layer_EPSG-4326.tif")
    Bare_raster <- raster("COPERNICUS/2015/PROBAV_LC100_global_v3.0.1_2015-base_Bare-CoverFraction-layer_EPSG-4326.tif")
    BuiltUp_raster <- raster("COPERNICUS/2015/PROBAV_LC100_global_v3.0.1_2015-base_BuiltUp-CoverFraction-layer_EPSG-4326.tif")
    Crops_raster <- raster("COPERNICUS/2015/PROBAV_LC100_global_v3.0.1_2015-base_Crops-CoverFraction-layer_EPSG-4326.tif")
    Grass_raster <- raster("COPERNICUS/2015/PROBAV_LC100_global_v3.0.1_2015-base_Grass-CoverFraction-layer_EPSG-4326.tif")
    MossLichen_raster <- raster("COPERNICUS/2015/PROBAV_LC100_global_v3.0.1_2015-base_MossLichen-CoverFraction-layer_EPSG-4326.tif")
    Shrub_raster <- raster("COPERNICUS/2015/PROBAV_LC100_global_v3.0.1_2015-base_Shrub-CoverFraction-layer_EPSG-4326.tif")
    Tree_raster <- raster("COPERNICUS/2015/PROBAV_LC100_global_v3.0.1_2015-base_Tree-CoverFraction-layer_EPSG-4326.tif")
    PermanentWater_raster <- raster("COPERNICUS/2015/PROBAV_LC100_global_v3.0.1_2015-base_PermanentWater-CoverFraction-layer_EPSG-4326.tif")
    SeasonalWater_raster <- raster("COPERNICUS/2015/PROBAV_LC100_global_v3.0.1_2015-base_SeasonalWater-CoverFraction-layer_EPSG-4326.tif")

    return(list(Bare_raster,BuiltUp_raster,Crops_raster,Grass_raster,MossLichen_raster,Shrub_raster,
                Tree_raster,PermanentWater_raster,SeasonalWater_raster))

  }else if(year == 2016){
    #Forest_raster <- raster("COPERNICUS/2016/PROBAV_LC100_global_v3.0.1_2016-conso_Forest-Type-layer_EPSG-4326.tif")
    Bare_raster <- raster("COPERNICUS/2016/PROBAV_LC100_global_v3.0.1_2016-conso_Bare-CoverFraction-layer_EPSG-4326.tif")
    BuiltUp_raster <- raster("COPERNICUS/2016/PROBAV_LC100_global_v3.0.1_2016-conso_BuiltUp-CoverFraction-layer_EPSG-4326.tif")
    Crops_raster <- raster("COPERNICUS/2016/PROBAV_LC100_global_v3.0.1_2016-conso_Crops-CoverFraction-layer_EPSG-4326.tif")
    Grass_raster <- raster("COPERNICUS/2016/PROBAV_LC100_global_v3.0.1_2016-conso_Grass-CoverFraction-layer_EPSG-4326.tif")
    MossLichen_raster <- raster("COPERNICUS/2016/PROBAV_LC100_global_v3.0.1_2016-conso_MossLichen-CoverFraction-layer_EPSG-4326.tif")
    Shrub_raster <- raster("COPERNICUS/2016/PROBAV_LC100_global_v3.0.1_2016-conso_Shrub-CoverFraction-layer_EPSG-4326.tif")
    Tree_raster <- raster("COPERNICUS/2016/PROBAV_LC100_global_v3.0.1_2016-conso_Tree-CoverFraction-layer_EPSG-4326.tif")
    PermanentWater_raster <- raster("COPERNICUS/2016/PROBAV_LC100_global_v3.0.1_2016-conso_PermanentWater-CoverFraction-layer_EPSG-4326.tif")
    SeasonalWater_raster <- raster("COPERNICUS/2016/PROBAV_LC100_global_v3.0.1_2016-conso_SeasonalWater-CoverFraction-layer_EPSG-4326.tif")

    return(list(Bare_raster,BuiltUp_raster,Crops_raster,Grass_raster,MossLichen_raster,Shrub_raster,
                Tree_raster,PermanentWater_raster,SeasonalWater_raster))

  }else if(year == 2017){
    #Forest_raster <- raster("COPERNICUS/2017/PROBAV_LC100_global_v3.0.1_2017-conso_Forest-Type-layer_EPSG-4326.tif")
    Bare_raster <- raster("COPERNICUS/2017/PROBAV_LC100_global_v3.0.1_2017-conso_Bare-CoverFraction-layer_EPSG-4326.tif")
    BuiltUp_raster <- raster("COPERNICUS/2017/PROBAV_LC100_global_v3.0.1_2017-conso_BuiltUp-CoverFraction-layer_EPSG-4326.tif")
    Crops_raster <- raster("COPERNICUS/2017/PROBAV_LC100_global_v3.0.1_2017-conso_Crops-CoverFraction-layer_EPSG-4326.tif")
    Grass_raster <- raster("COPERNICUS/2017/PROBAV_LC100_global_v3.0.1_2017-conso_Grass-CoverFraction-layer_EPSG-4326.tif")
    MossLichen_raster <- raster("COPERNICUS/2017/PROBAV_LC100_global_v3.0.1_2017-conso_MossLichen-CoverFraction-layer_EPSG-4326.tif")
    Shrub_raster <- raster("COPERNICUS/2017/PROBAV_LC100_global_v3.0.1_2017-conso_Shrub-CoverFraction-layer_EPSG-4326.tif")
    Tree_raster <- raster("COPERNICUS/2017/PROBAV_LC100_global_v3.0.1_2017-conso_Tree-CoverFraction-layer_EPSG-4326.tif")
    PermanentWater_raster <- raster("COPERNICUS/2017/PROBAV_LC100_global_v3.0.1_2017-conso_PermanentWater-CoverFraction-layer_EPSG-4326.tif")
    SeasonalWater_raster <- raster("COPERNICUS/2017/PROBAV_LC100_global_v3.0.1_2017-conso_SeasonalWater-CoverFraction-layer_EPSG-4326.tif")

    return(list(Bare_raster,BuiltUp_raster,Crops_raster,Grass_raster,MossLichen_raster,Shrub_raster,
                Tree_raster,PermanentWater_raster,SeasonalWater_raster))

  }else if(year == 2018){
    #Forest_raster <- raster("COPERNICUS/2018/PROBAV_LC100_global_v3.0.1_2018-conso_Forest-Type-layer_EPSG-4326.tif")
    Bare_raster <- raster("COPERNICUS/2018/PROBAV_LC100_global_v3.0.1_2018-conso_Bare-CoverFraction-layer_EPSG-4326.tif")
    BuiltUp_raster <- raster("COPERNICUS/2018/PROBAV_LC100_global_v3.0.1_2018-conso_BuiltUp-CoverFraction-layer_EPSG-4326.tif")
    Crops_raster <- raster("COPERNICUS/2018/PROBAV_LC100_global_v3.0.1_2018-conso_Crops-CoverFraction-layer_EPSG-4326.tif")
    Grass_raster <- raster("COPERNICUS/2018/PROBAV_LC100_global_v3.0.1_2018-conso_Grass-CoverFraction-layer_EPSG-4326.tif")
    MossLichen_raster <- raster("COPERNICUS/2018/PROBAV_LC100_global_v3.0.1_2018-conso_MossLichen-CoverFraction-layer_EPSG-4326.tif")
    Shrub_raster <- raster("COPERNICUS/2018/PROBAV_LC100_global_v3.0.1_2018-conso_Shrub-CoverFraction-layer_EPSG-4326.tif")
    Tree_raster <- raster("COPERNICUS/2018/PROBAV_LC100_global_v3.0.1_2018-conso_Tree-CoverFraction-layer_EPSG-4326.tif")
    PermanentWater_raster <- raster("COPERNICUS/2018/PROBAV_LC100_global_v3.0.1_2018-conso_PermanentWater-CoverFraction-layer_EPSG-4326.tif")
    SeasonalWater_raster <- raster("COPERNICUS/2018/PROBAV_LC100_global_v3.0.1_2018-conso_SeasonalWater-CoverFraction-layer_EPSG-4326.tif")

    return(list(Bare_raster,BuiltUp_raster,Crops_raster,Grass_raster,MossLichen_raster,Shrub_raster,
                Tree_raster,PermanentWater_raster,SeasonalWater_raster))

  }else{
    #Forest_raster <- raster("COPERNICUS/2019/PROBAV_LC100_global_v3.0.1_2019-nrt_Forest-Type-layer_EPSG-4326.tif")
    Bare_raster <- raster("COPERNICUS/2019/PROBAV_LC100_global_v3.0.1_2019-nrt_Bare-CoverFraction-layer_EPSG-4326.tif")
    BuiltUp_raster <- raster("COPERNICUS/2019/PROBAV_LC100_global_v3.0.1_2019-nrt_BuiltUp-CoverFraction-layer_EPSG-4326.tif")
    Crops_raster <- raster("COPERNICUS/2019/PROBAV_LC100_global_v3.0.1_2019-nrt_Crops-CoverFraction-layer_EPSG-4326.tif")
    Grass_raster <- raster("COPERNICUS/2019/PROBAV_LC100_global_v3.0.1_2019-nrt_Grass-CoverFraction-layer_EPSG-4326.tif")
    MossLichen_raster <- raster("COPERNICUS/2019/PROBAV_LC100_global_v3.0.1_2019-nrt_MossLichen-CoverFraction-layer_EPSG-4326.tif")
    Shrub_raster <- raster("COPERNICUS/2019/PROBAV_LC100_global_v3.0.1_2019-nrt_Shrub-CoverFraction-layer_EPSG-4326.tif")
    Tree_raster <- raster("COPERNICUS/2019/PROBAV_LC100_global_v3.0.1_2019-nrt_Tree-CoverFraction-layer_EPSG-4326.tif")
    PermanentWater_raster <- raster("COPERNICUS/2019/PROBAV_LC100_global_v3.0.1_2019-nrt_PermanentWater-CoverFraction-layer_EPSG-4326.tif")
    SeasonalWater_raster <- raster("COPERNICUS/2019/PROBAV_LC100_global_v3.0.1_2019-nrt_SeasonalWater-CoverFraction-layer_EPSG-4326.tif")

    return(list(Bare_raster,BuiltUp_raster,Crops_raster,Grass_raster,MossLichen_raster,Shrub_raster,
                Tree_raster,PermanentWater_raster,SeasonalWater_raster))

  }

}

##########################################
##########################################

distance_species_s_parcel_m <- function(col_m,row_m,rr.df, side_x, side_y){

  x_dis <- side_x/ncol(rr.df)
  y_dis <- side_y/nrow(rr.df)

  v_dist <- matrix(rep(1:nrow(rr.df),ncol(rr.df)),ncol=ncol(rr.df))
  h_dist <- t(matrix(rep(1:ncol(rr.df),nrow(rr.df)),ncol=nrow(rr.df)))

  v_dist <- abs(v_dist - row_m)*y_dis
  h_dist <- abs(h_dist - col_m)*x_dis

  dist <- sqrt(v_dist*v_dist + h_dist*h_dist)

  dist

}

##########################################
##########################################

centre <- function(rr.df){
  if(nrow(rr.df) %% 2 !=0 ){ #if the number of rows is odd

    i <- (nrow(rr.df)+1)/2

  }else{
    i <- nrow(rr.df)/2
  }

  if(ncol(rr.df) %% 2 !=0 ){ #if the number of rows is odd

    j <- (ncol(rr.df)+1)/2

  }else{
    j <- ncol(rr.df)/2
  }

  list(i,j)
}



######################
# LONSDORF FUNCTIONS
######################


lonsdorf_GA_COPERNICUS <- function(Bare_nest_list,BuiltUp_nest_list,Crops_nest_list,
                                   Grass_nest_list,MossLichen_nest_list,Shrub_nest_list,Tree_nest_list,
                                   PermanentWater_nest_list,SeasonalWater_nest_list,
                                   Bare_floral_list_500,BuiltUp_floral_list_500,Crops_floral_list_500,
                                   Grass_floral_list_500,MossLichen_floral_list_500,
                                   Shrub_floral_list_500,Tree_floral_list_500,Bare_floral_list_3000,
                                   BuiltUp_floral_list_3000,Crops_floral_list_3000,
                                   Grass_floral_list_3000,MossLichen_floral_list_3000,
                                   Shrub_floral_list_3000,Tree_floral_list_3000,smoothing_list_500,
                                   smoothing_list_3000,FloralNest_tab,parameters){

  # Create tables

  FloralNest_tab$nesting_max <- 0
  FloralNest_tab$floral <- 0

  # FloralNest_tab$nesting_max[1:7] <- parameters[1:7] # nesting for wild_bees
  # FloralNest_tab$floral[10:16] <- parameters[8:14] # nesting for bombus
  # FloralNest_tab$floral[19:nrow(FloralNest_tab)] <- parameters[15:23] # nesting for syrphids
  #
  # FloralNest_tab$floral[1:7] <- parameters[24:30] # nesting for wild_bees
  # FloralNest_tab$floral[10:16] <- parameters[31:37] # nesting for bombus
  # FloralNest_tab$floral[19:25] <- parameters[38:44] # nesting for syrphids

  FloralNest_tab$nesting_max[c(1:7,10:16,19:nrow(FloralNest_tab))] <- parameters[c(1:23)]
  FloralNest_tab$floral[c(1:7,10:16,19:25)] <- parameters[c(24:44)]

  FloralNest_tab <- FloralNest_tab %>% as.data.frame()

  # Estimate nesting suit by guild

  FloralNest_tab_wild_bees <- FloralNest_tab %>% filter(guild == "wild_bees") %>% dplyr::select(-guild)
  nesting_suit_raster_wild_bees <- nesting_suit_GA_COPERNICUS(Bare_nest_list,BuiltUp_nest_list,Crops_nest_list,
                                                              Grass_nest_list,MossLichen_nest_list,
                                                              Shrub_nest_list,Tree_nest_list,
                                                              PermanentWater_nest_list,SeasonalWater_nest_list,
                                                              FloralNest_tab_wild_bees,guild = "wild_bees")

  FloralNest_tab_bombus <- FloralNest_tab %>% filter(guild == "bombus") %>% dplyr::select(-guild)
  nesting_suit_raster_bombus <- nesting_suit_GA_COPERNICUS(Bare_nest_list,BuiltUp_nest_list,Crops_nest_list,
                                                           Grass_nest_list,MossLichen_nest_list,
                                                           Shrub_nest_list,Tree_nest_list,
                                                           PermanentWater_nest_list,SeasonalWater_nest_list,
                                                           FloralNest_tab_bombus,guild = "bombus")

  FloralNest_tab_syrphids <- FloralNest_tab %>% filter(guild == "syrphids") %>% dplyr::select(-guild)
  nesting_suit_raster_syrphids <- nesting_suit_GA_COPERNICUS(Bare_nest_list,BuiltUp_nest_list,Crops_nest_list,
                                                             Grass_nest_list,MossLichen_nest_list,
                                                             Shrub_nest_list,Tree_nest_list,
                                                             PermanentWater_nest_list,SeasonalWater_nest_list,
                                                             FloralNest_tab_syrphids,guild = "syrphids")

  # Estimate floral resources by guild

  floral_resource_raster_wild_bees <- floral_resource_GA_COPERNICUS(Bare_floral_list_500,
                                                                    BuiltUp_floral_list_500,
                                                                    Crops_floral_list_500,
                                                                    Grass_floral_list_500,
                                                                    MossLichen_floral_list_500,
                                                                    Shrub_floral_list_500,
                                                                    Tree_floral_list_500,
                                                                    FloralNest_tab_wild_bees)

  floral_resource_raster_bombus <- floral_resource_GA_COPERNICUS(Bare_floral_list_3000,
                                                                 BuiltUp_floral_list_3000,
                                                                 Crops_floral_list_3000,
                                                                 Grass_floral_list_3000,
                                                                 MossLichen_floral_list_3000,
                                                                 Shrub_floral_list_3000,
                                                                 Tree_floral_list_3000,
                                                                 FloralNest_tab_bombus)

  floral_resource_raster_syrphids <- floral_resource_GA_COPERNICUS(Bare_floral_list_500,
                                                                   BuiltUp_floral_list_500,
                                                                   Crops_floral_list_500,
                                                                   Grass_floral_list_500,
                                                                   MossLichen_floral_list_500,
                                                                   Shrub_floral_list_500,
                                                                   Tree_floral_list_500,
                                                                   FloralNest_tab_syrphids)



  # Pollinator score in the cent of the square (OBserv point)
  score_center_wild_bees <- pollinator_score_center_GA_COPERNICUS(nesting_suit_raster_wild_bees,
                                                                  floral_resource_raster_wild_bees,
                                                                  smoothing_list_500)

  score_center_bombus <- pollinator_score_center_GA_COPERNICUS(nesting_suit_raster_bombus,
                                                               floral_resource_raster_bombus,
                                                               smoothing_list_3000)

  score_center_syrphids <- pollinator_score_center_GA_COPERNICUS(nesting_suit_raster_syrphids,
                                                                 floral_resource_raster_syrphids,
                                                                 smoothing_list_500)


  score_center_list <- list(score_center_wild_bees, score_center_bombus, score_center_syrphids)

  return(score_center_list)
}

##########################################
##########################################

nesting_suit_GA_COPERNICUS <- function(Bare_list,BuiltUp_list,Crops_list,Grass_list,
                                       MossLichen_list,Shrub_list,Tree_list,
                                       PermanentWater_nest_list,SeasonalWater_nest_list,
                                       FloralNest_tab_guild,guild){

  nesting_suit_raster <- NULL

  if(guild != "syrphids"){

    for(i in 1:length(Bare_list)){
      nesting_suit_raster[[i]] <- Bare_list[[i]]*FloralNest_tab_guild[1,2]+
        BuiltUp_list[[i]]*FloralNest_tab_guild[2,2]+ Crops_list[[i]]*FloralNest_tab_guild[3,2] +
        Grass_list[[i]]*FloralNest_tab_guild[4,2] + MossLichen_list[[i]]*FloralNest_tab_guild[5,2] +
        Shrub_list[[i]]*FloralNest_tab_guild[6,2] + Tree_list[[i]]*FloralNest_tab_guild[7,2]
    }

  }else{

    for(i in 1:length(Bare_list)){
      nesting_suit_raster[[i]] <- Bare_list[[i]]*FloralNest_tab_guild[1,2]+
        BuiltUp_list[[i]]*FloralNest_tab_guild[2,2]+ Crops_list[[i]]*FloralNest_tab_guild[3,2] +
        Grass_list[[i]]*FloralNest_tab_guild[4,2] + MossLichen_list[[i]]*FloralNest_tab_guild[5,2] +
        Shrub_list[[i]]*FloralNest_tab_guild[6,2] + Tree_list[[i]]*FloralNest_tab_guild[7,2]+
        PermanentWater_list[[i]]*FloralNest_tab_guild[8,2] + SeasonalWater_list[[i]]*FloralNest_tab_guild[9,2]
    }

  }



  nesting_suit_raster

}

##########################################
##########################################

floral_resource_GA_COPERNICUS <- function(Bare_list,BuiltUp_list,Crops_list,Grass_list,
                                          MossLichen_list,Shrub_list,Tree_list,FloralNest_tab_guild){

  Floral_res_smooth <- NULL

  for(i in 1:length(Bare_list)){
    Floral_res_smooth[[i]] <- Bare_list[[i]]*FloralNest_tab_guild[1,3]+
      BuiltUp_list[[i]]*FloralNest_tab_guild[2,3]+ Crops_list[[i]]*FloralNest_tab_guild[3,3] +
      Grass_list[[i]]*FloralNest_tab_guild[4,3] + MossLichen_list[[i]]*FloralNest_tab_guild[5,3] +
      Shrub_list[[i]]*FloralNest_tab_guild[6,3] + Tree_list[[i]]*FloralNest_tab_guild[7,3]
  }

  Floral_res_smooth

}

##########################################
##########################################

pollinator_score_center_GA_COPERNICUS <- function(nesting_suit_raster,floral_resource_raster,smoothing_list){

  score_center <- NULL

  for(i in 1:length(nesting_suit_raster)){
    score_center[[i]] <- sum(nesting_suit_raster[[i]]*floral_resource_raster[[i]]*smoothing_list[[i]])
  }

  score_center

}

###########################################
###########################################

table_GA_COPERNICUS <- function(parameters,FloralNest_tab,biome){

  FloralNest_tab$nesting_max <- 0
  FloralNest_tab$floral <- 0

  FloralNest_tab$nesting_max[1:7] <- parameters[1:7] # nesting for wild_bees
  FloralNest_tab$floral[10:16] <- parameters[8:14] # nesting for bombus
  FloralNest_tab$floral[19:nrow(FloralNest_tab)] <- parameters[15:23] # nesting for syrphids

  FloralNest_tab$floral[1:7] <- parameters[24:30] # nesting for wild_bees
  FloralNest_tab$floral[10:16] <- parameters[31:37] # nesting for bombus
  FloralNest_tab$floral[19:25] <- parameters[38:44] # nesting for syrphids

  FloralNest_tab$biome <- biome

  FloralNest_tab <- FloralNest_tab %>% as.data.frame()
}

# ##########################################
# ##########################################
#
# centre <- function(rr.df){
#   if(nrow(rr.df) %% 2 !=0 ){ #if the number of rows is odd
#
#     i <- (nrow(rr.df)+1)/2
#
#   }else{
#     i <- nrow(rr.df)/2
#   }
#
#   if(ncol(rr.df) %% 2 !=0 ){ #if the number of rows is odd
#
#     j <- (ncol(rr.df)+1)/2
#
#   }else{
#     j <- ncol(rr.df)/2
#   }
#
#   list(i,j)
# }

