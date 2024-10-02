---
title: "Landscape crops and Shapes"
---
  
#Library--------------------------------------
library(sf)
library(rnaturalearth)
library(raster)
library(dplyr)

#---------------Working Directory and objects-------------------------


#Home computer
setwd("")


#-------------Breeding Range creation and cropping------------------

#Create shape for the study region
usa_states <- ne_states(country = "united states of america", returnclass = "sf") %>%
  dplyr::filter((name %in%  c("North Dakota", "South Dakota", "Nebraska", "Kansas", 
                              "Missouri", "Iowa", "Minnesota", 
                              "Wisconsin", "Illinois", "Kentucky", "Indiana", "Michigan",
                              "Ohio", "Virginia", "West Virginia",
                              "Maryland", "Delaware", "New Jersey", "Pennsylvania",
                              "New York", "Connecticut", "Massachusetts", "New Hampshire", "Vermont", "Maine",
                              "Rhode Island"))) %>% sf::as_Spatial()

canada <- ne_states(country = "canada", returnclass = "sf") %>%
  dplyr::filter((name %in%  c("Ontario", "Québec"))) %>% sf::as_Spatial()



range <- bind(usa_states, canada)


original_extent <- extent(range)

new_extent <- extent(original_extent@xmin, original_extent@xmax-5, original_extent@ymin, 47.5)

cropped_breeding <- crop(range, new_extent)

breeding_sf <- st_as_sf(cropped_breeding)

# Dissolve state borders by using the union operation
dissolved_breeding_sf <- breeding_sf %>%
  summarise(geometry = st_union(geometry))

plot(dissolved_breeding_sf)

#Save object
st_write(dissolved_breeding_sf, dsn = "Shapes/StartingRangeFull.shp")
saveRDS(dissolved_breeding_sf, "Shapes/StartingRangeFull.rds")


#----------------------Full study region-------------------

#Create shape to crop the raster to the study region
usa_states <- ne_states(country = "united states of america", returnclass = "sf") %>%
  dplyr::filter(!(name %in%  c("Alaska", "Hawaii"))) %>%
  sf::as_Spatial()

canada <- ne_states(country = "canada", returnclass = "sf") %>%
  dplyr::filter((name %in%  c("British Columbia", "Alberta", "Québec", 
                              "Saskatchewan", "Manitoba", "Ontario"))) %>%
  sf::as_Spatial()

mexico <- ne_states(country = "mexico", returnclass = "sf") %>% 
  sf::as_Spatial()

#Full area
study_region <- bind(usa_states, canada, mexico)

#Cropping extent
original_extent <- extent(study_region)
new_extent <-  extent(original_extent@xmin, original_extent@xmax-5, original_extent@ymin, 50)

#Cropped region
cropped_study_region <- crop(study_region, new_extent)


region_sf <- st_as_sf(cropped_study_region)

# Dissolve state borders by using the union operation
dissolved_region_sf <- region_sf %>%
  summarise(geometry = st_union(geometry))

plot(dissolved_region_sf)

#Save object
st_write(dissolved_region_sf, dsn = "Shapes/FullStudyRegion.shp")
saveRDS(dissolved_region_sf, "Shapes/FullStudyRegion.rds")


#----------------Crop and convert Altitude raster to probabilities---------

# Import raster
landscape <- raster("Covariates/AltitudeRaster.tif")

#Import Study region for crop
range <- read_sf("Shapes/FullStudyRegion.shp")

# Crop and edit raster to be in the new extent
r2 <- crop(landscape, extent(range))
r3 <- mask(r2, range)

plot(r3)




#------------------Overwintering Location------------------------------

ow_coords <- data.frame(lon = -100.30878851854285, lat=19.590505669916)
ow_sf <- st_as_sf(ow_coords, coords = c("lon", "lat"), crs=4326)


#Create thirty kilometer buffer radius
ow_buffer  <- st_as_sf(st_buffer(ow_sf$geometry, dist = 30000))
#Sixty Kilometer radius
ow_buffer  <- st_as_sf(st_buffer(ow_sf$geometry, dist = 60000))
ow_buffer  <- st_as_sf(st_buffer(ow_sf$geometry, dist = 120000))
ow_buffer  <- st_as_sf(st_buffer(ow_sf$geometry, dist = 240000))
#Graph
ggplot()+
  gg(ow_buffer, alpha =0)+
  #gg(StartingRange)+
  gg(ow_sf)+
  gg(EndRange, alpha =0)
    

#Save
st_write(ow_sf, "Shapes/OverwinterPoint.shp")
st_write(ow_buffer, "Shapes/OverwinterBuffer240kilo.shp")

#---------------Cropping Covariates for SDMs---------------------
#habitat---------------------------

#Imporrt
raster_files <- list.files(path = "Covariates/Landcover Sections", pattern = "\\.tif$", full.names = TRUE)

first <- raster(raster_files[1])
second <- raster(raster_files[5])
third <- raster(raster_files[6])
fourth <- raster(raster_files[7])
fifth <- raster(raster_files[8])
sixth <- raster(raster_files[9])
seventh<- raster(raster_files[10])
eighth<- raster(raster_files[11])
ninth<- raster(raster_files[12])
tenth<- raster(raster_files[2])
eleventh<- raster(raster_files[3])
twelth<- raster(raster_files[4])

#Merge
row1 <- merge(first, second)
row1 <- merge(row1, third)
row1 <- merge(row1, fourth)
row1 <- merge(row1, fifth)
row2 <- merge(sixth, seventh)
row2 <- merge(row2, eighth)
row2 <- merge(row2, ninth)
row3 <- merge(tenth, eleventh)
row3 <- merge(row3, twelth)
merge_test2 <- merge(row1, row2)
merge_test2 <- merge(merge_test2, row3)

#Save full Raster
writeRaster(merge_test2, "Covariates/habitat_full.tif", "GTiff")

#Import full raster
merge_test2 <- raster("Covariates/habitat_full.tif")

#Cropping Box (provide buffer to decrease the number of NAs)
coords_crop <- mesh.monarch$loc
min_x <- min(coords_crop[, 1])-0.5
max_x <- max(coords_crop[, 1])+0.5
min_y <- min(coords_crop[, 2])-0.5
max_y <- max(coords_crop[, 2])+0.5
bbox_inla <- c(min_x, max_x, min_y, max_y)

#Crop
cropped_habitat <- raster::intersect(merge_test2, extent(bbox_inla))


#Save cropped raster
writeRaster(cropped_habitat, "Covariates/habitat_cropped.tif", "GTiff")


#Decrease resolution
habitat_low_res<- raster::aggregate(cropped_habitat, fact = 9, fun=modal, na.rm =F)

#REPLACE NA with value for ocean/water
habitat_low_res[is.na(habitat_low_res)] <- 200

#Save low res and cropped raster
writeRaster(habitat_low_res, "Covariates/habitat_cropped_low_res.tif", "GTiff")

#Convert in sp object
habitat.sp <- as(habitat_low_res, "SpatialPixelsDataFrame")

#save sp
saveRDS(habitat.sp, "Covariates/habitat_cropped_lowres.rds")



#Altitude-------------------------
altitude <-  raster("Covariates/AltitudeRaster.tif")

#Cropping Box
coords_crop <- mesh.monarch$loc
min_x <- min(coords_crop[, 1])-0.5
max_x <- max(coords_crop[, 1])+0.5
min_y <- min(coords_crop[, 2])-0.5
max_y <- max(coords_crop[, 2])+0.5
bbox_inla <- c(min_x, max_x, min_y, max_y)
#Crop
cropped_altitude <- raster::intersect(altitude, extent(bbox_inla))

#Fill in ocean with 0 altitude
cropped_altitude[is.na(cropped_altitude)] <- 0

#Save Cropped Raster
writeRaster(cropped_altitude, "Covariates/altitude_cropped.tif", "GTiff", overwrite =T)

#Convert to sp object
altitude.sp <- as(cropped_altitude, "SpatialPixelsDataFrame")

#Sav sp object
saveRDS(altitude.sp, "Covariates/altitude_croped.rds")


#Human population density-----------------------------
pop_density <-  raster("Covariates/gpw_v4_population_density_adjusted_to_2015_unwpp_country_totals_rev11_2020_30_sec.tif")

#Cropping Box
coords_crop <- mesh.monarch$loc
min_x <- min(coords_crop[, 1])-0.5
max_x <- max(coords_crop[, 1])+0.5
min_y <- min(coords_crop[, 2])-0.5
max_y <- max(coords_crop[, 2])+0.5
bbox_inla <- c(min_x, max_x, min_y, max_y)

#Crop
cropped_pop <- raster::intersect(pop_density, extent(bbox_inla))

#Fill in NA information and ocean with zero people
cropped_pop[is.na(cropped_pop)] <- 0

#Save Cropped Raster
writeRaster(cropped_pop, "Covariates/humandensity_cropped.tif", "GTiff", overwrite =T)

#Convert to sp object
human_density.sp <- as(cropped_pop, "SpatialPixelsDataFrame")

#Sav sp object
saveRDS(human_density.sp, "Covariates/humandensity_croped.rds")


# Cropping and editing Covariates for spde----------------------------------------------
#Full, original, uncropped Covariates
human_density.cov <- raster("Covariates/gpw_v4_population_density_adjusted_to_2015_unwpp_country_totals_rev11_2020_30_sec.tif")
Altitude.cov <- raster("Covariates/AltitudeRaster.tif")
Habitat.cov <- raster("Covariates/habitat_full.tif")
Habitat.cov_lowres <- raster("Covariates/Lowres_Habitat_full.tif")


#Cropping Box
coords_crop <- mesh.monarch$loc
min_x <- min(coords_crop[, 1])-0.5
max_x <- max(coords_crop[, 1])+0.5
min_y <- min(coords_crop[, 2])-0.5
max_y <- max(coords_crop[, 2])+0.5
bbox_inla <- c(min_x, max_x, min_y, max_y)


#Cropping the rasters
habitat <- crop(Habitat.cov, extent(bbox_inla))
altitude <- crop(Altitude.cov, extent(bbox_inla))
human_density <- crop(human_density.cov, extent(bbox_inla))

#Decrease Habitat resolution
habitat<- raster::aggregate(habitat, fact = 9, fun=modal, na.rm =F)

#Fill in NAs
habitat[is.na(habitat)] <- 200 #Fills in NA with ocean value 200
altitude[is.na(altitude)] <- 0 #Fills in NA with ocean altitude of 0
human_density[is.na(human_density)] <- 0 #Fills in NA with 0 population

#Save cropped covariate versions with no NAs
writeRaster(habitat, "Covariates/habitat_lowres_cropped.tif", "GTiff")
writeRaster(altitude, "Covariates/altitude_cropped.tif", "GTiff")
writeRaster(human_density, "Covariates/human_density_cropped.tif", "GTiff")



#Converting Rasters into SpatialPixelDataFrames for ease of using---------------
human_density.sp <- as(human_density, "SpatialPixelsDataFrame")
altitude.sp <- as(altitude, "SpatialPixelsDataFrame")
habitat.sp <- as(habitat, "SpatialPixelsDataFrame")



#Save SpatialPIxelDataFrames to Savetime creating them
saveRDS(human_density.sp, file = "Covariates/human_density.sp.rds")
saveRDS(altitude.sp, file = "Covariates/altitude.sp.rds")
saveRDS(habitat.sp, file = "Covariates/habitat.sp.rds")

