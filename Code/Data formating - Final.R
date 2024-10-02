####Library####
library(inlabru)
library(INLA)
library(RColorBrewer)
library(ggpolypath)
library(sp)
library(fmesher)
library(tidyverse)
library(sf)
library(raster)
library(maptools)
library(spatstat)
library(tmap)
library(leaflet)
library(dplyr)
library(rnaturalearth)
library(sftime)
library(beepr)
library(terra)

#### Set Working Directory for Data input ####

#Home computer
setwd("")




#-----------------------------Import Simulation Data--------------------------
# --------------------------------Full Paths----------------------------

#Single Vector (global landscape probs)
gv_obs <- read.csv("Data/1000Agents_eastern_global.csv") # 30 km beacon

#Single Vector (local Landscape Probs)
lv_obs <- read.csv("Data/1000Agents_eastern_local.csv") # 30km beacon
lv_obs60 <- read.csv("Data/1000Agents_eastern_local_vector_60km.csv") # 60km beacon
lv_obs120 <- read.csv("Data/1000Agents_eastern_local_vector_120km.csv") # 120km beacon
lv_obs240 <- read.csv("Data/1000Agents_eastern_local_vector_240km.csv") # 240km beacon

#Multi-vector (local landscape probs)
lmv_obs <- read.csv("Data/1000Agents_eastern_local_multivector.csv") # 30km beacon
lmv_obs60 <- read.csv("Data/1000Agents_eastern_local_multivector_60km.csv") # 60km beacon
lmv_obs120 <- read.csv("Data/1000Agents_eastern_local_multivector_120km.csv") # 120km beacon
lmv_obs240 <- read.csv("Data/1000Agents_eastern_local_multivector_240km.csv") # 240km beacon
lmv_obs_k0.61 
#True Navigator (local landscape probs) 
lt_obs <- read.csv("Data/1000Agents_eastern_local_true.csv") # Inf beacon range


#------------------------------ Final Locations -----------------
#Single Vector (global landscape probs)
gv_ends <- read.csv("Data/Final_location_1000Agents_eastern_global.csv")

#Single Vector (local Landscape Probs)
lv_ends <- read.csv("Data/Final_location_1000Agents_eastern_local.csv")
lv_ends60 <- read.csv("Data/Final_location_1000Agents_eastern_local_vector_60km.csv")
lv_ends120 <- read.csv("Data/Final_location_1000Agents_eastern_local_vector_120km.csv")
lv_ends240 <- read.csv("Data/Final_location_1000Agents_eastern_local_vector_240km.csv")
lv_ends_k0.61 <- read.csv("Data/Final_location_1000Agents_eastern_local_vector_30km_k0.61.csv")
#Multi-vector (local landscape probs)
lmv_ends <- read.csv("Data/Final_location_1000Agents_eastern_local_multivector.csv")
lmv_ends60 <- read.csv("Data/Final_location_1000Agents_eastern_local_multivector_60km.csv")
lmv_ends120 <- read.csv("Data/Final_location_1000Agents_eastern_local_multivector_120km.csv")
lmv_ends240 <- read.csv("Data/Final_location_1000Agents_eastern_local_multivector_240km.csv")
lmv_ends_k0.61 <- read.csv("Data/Final_location_1000Agents_eastern_local_multivector_0.61k.csv")
#True Navigator (local landscape probs)
lt_ends <- read.csv("Data/Final_location_1000Agents_eastern_local_true.csv")
lt_ends <- read.csv("Data/Final_location_1000Agents_eastern_local_true_7500steps.csv")
lt_ends_k0.61 <- read.csv("Data/Final_location_1000Agents_eastern_true_k0.61.csv")

###Study region for graphing---
range <- read_sf("Shapes/FullStudyRegion.shp")
EndRange <- read_sf("Shapes/OverwinterBuffer.shp")


#----------------------Convert Paths into Lines for graphing------------------

####### Single Vector (global landscape probs)
gv_paths <- gv_obs[, c(6,5,7)]

colnames(gv_paths) <- c("Latitude", "Longitude", "Agent") 

gv_obs_sf <- st_as_sf(gv_paths, coords = c('Longitude', 'Latitude')) #Creeate sf object


lines_gv <- gv_obs_sf %>%
  group_by(Agent) %>%
  summarise(geometry = st_cast(st_combine(geometry), "LINESTRING")) %>%
  st_as_sf(crs = st_crs(EndRange))


st_write(lines_gv, "Data/vector_global_lines.shp")
saveRDS(gv_obs_sf, "Data/1000vector_global_sf.rds")




######## Single Vector (local Landscape Probs)
lv_paths <- lv_obs[, c(6,5,7)] # 30km beacon
lv_paths60 <- lv_obs60[, c(6,5,7)] 
lv_paths120 <- lv_obs120[, c(6,5,7)] 
lv_paths240 <- lv_obs240[, c(6,5,7)] 

colnames(lv_paths) <- c("Latitude", "Longitude", "Agent")
colnames(lv_paths60) <- c("Latitude", "Longitude", "Agent")
colnames(lv_paths120) <- c("Latitude", "Longitude", "Agent")
colnames(lv_paths240) <- c("Latitude", "Longitude", "Agent")

# Sf objects
lv_obs_sf <- st_as_sf(lv_paths, coords = c('Longitude', 'Latitude'))
lv_obs_sf60 <- st_as_sf(lv_paths60, coords = c('Longitude', 'Latitude'))
lv_obs_sf120 <- st_as_sf(lv_paths120, coords = c('Longitude', 'Latitude'))
lv_obs_sf240 <- st_as_sf(lv_paths240, coords = c('Longitude', 'Latitude'))

#Save sf object
saveRDS(lv_obs_sf, "Data/1000vector_local_sf.rds")
saveRDS(lv_obs_sf60, "Data/1000vector_local_sf60.rds")
saveRDS(lv_obs_sf120, "Data/1000vector_local_sf120.rds")
saveRDS(lv_obs_sf240, "Data/1000vector_local_sf240.rds")

#Lines objects
lines_lv <- lv_obs_sf %>%
  dplyr::group_by(Agent) %>%
  dplyr::summarise(geometry = st_cast(st_combine(geometry), "LINESTRING")) %>%
  st_as_sf(crs = st_crs(EndRange))

lines_lv60 <- lv_obs_sf60 %>%
  dplyr::group_by(Agent) %>%
  dplyr::summarise(geometry = st_cast(st_combine(geometry), "LINESTRING")) %>%
  st_as_sf(crs = st_crs(EndRange))

lines_lv120 <- lv_obs_sf120 %>%
  dplyr::group_by(Agent) %>%
  dplyr::summarise(geometry = st_cast(st_combine(geometry), "LINESTRING")) %>%
  st_as_sf(crs = st_crs(EndRange))

lines_lv240 <- lv_obs_sf240 %>%
  dplyr::group_by(Agent) %>%
  dplyr::summarise(geometry = st_cast(st_combine(geometry), "LINESTRING")) %>%
  st_as_sf(crs = st_crs(EndRange))

#Save line object
st_write(lines_lv, "Data/vector_local_lines.shp")
st_write(lines_lv60, "Data/vector_local_lines60.shp")
st_write(lines_lv120, "Data/vector_local_lines120.shp")
st_write(lines_lv240, "Data/vector_local_lines240.shp")




########### Multi-vector (local landscape probs)
lmv_paths <- lmv_obs[, c(6,5,7)] # 30km beacon
lmv_paths60 <- lmv_obs60[, c(6,5,7)] # 60km beacon
lmv_paths120 <- lmv_obs120[, c(6,5,7)] # 120km beacon
lmv_paths240 <- lmv_obs240[, c(6,5,7)] # 240km beacon

colnames(lmv_paths) <- c("Latitude", "Longitude", "Agent")
colnames(lmv_paths60) <- c("Latitude", "Longitude", "Agent")
colnames(lmv_paths120) <- c("Latitude", "Longitude", "Agent")
colnames(lmv_paths240) <- c("Latitude", "Longitude", "Agent")

# Sf objects
lmv_obs_sf <- st_as_sf(lmv_paths, coords = c('Longitude', 'Latitude'))
lmv_obs_sf60 <- st_as_sf(lmv_paths60, coords = c('Longitude', 'Latitude'))
lmv_obs_sf120 <- st_as_sf(lmv_paths120, coords = c('Longitude', 'Latitude'))
lmv_obs_sf240 <- st_as_sf(lmv_paths240, coords = c('Longitude', 'Latitude'))

saveRDS(lmv_obs_sf, "Data/1000multivector_local_sf.rds")
saveRDS(lmv_obs_sf60, "Data/1000multivector_local_sf60.rds")
saveRDS(lmv_obs_sf, "Data/1000multivector_local_sf.rds")
saveRDS(lmv_obs_sf, "Data/1000multivector_local_sf.rds")

#Line objects
lines_lmv <- lmv_obs_sf %>% # 30km beacon
  dplyr::group_by(Agent) %>%
  dplyr::summarise(geometry = st_cast(st_combine(geometry), "LINESTRING")) %>%
  st_as_sf(crs = st_crs(EndRange))


lines_lmv60 <- lmv_obs_sf60 %>% # 60km beacon
  dplyr::group_by(Agent) %>%
  dplyr::summarise(geometry = st_cast(st_combine(geometry), "LINESTRING")) %>%
  st_as_sf(crs = st_crs(EndRange))


lines_lmv120 <- lmv_obs_sf120 %>% # 120km beacon
  dplyr::group_by(Agent) %>%
  dplyr::summarise(geometry = st_cast(st_combine(geometry), "LINESTRING")) %>%
  st_as_sf(crs = st_crs(EndRange))


lines_lmv240 <- lmv_obs_sf240 %>% # 240km beacon
  dplyr::group_by(Agent) %>%
  dplyr::summarise(geometry = st_cast(st_combine(geometry), "LINESTRING")) %>%
  st_as_sf(crs = st_crs(EndRange))


st_write(lines_lmv, "Data/multivector_local_lines.shp")
st_write(lines_lmv60, "Data/multivector_local_lines60.shp")
st_write(lines_lmv120, "Data/multivector_local_lines120.shp")
st_write(lines_lmv240, "Data/multivector_local_lines240.shp")








################ True navigation (local landscape probs)
lt_paths <- lt_obs[, c(6,5,7)]

colnames(lt_paths) <- c("Latitude", "Longitude", "Agent")

lt_obs_sf <- st_as_sf(lt_paths, coords = c('Longitude', 'Latitude'))


lines_lt <- lt_obs_sf %>%
  group_by(Agent) %>%
  summarise(geometry = st_cast(st_combine(geometry), "LINESTRING")) %>%
  st_as_sf(crs = st_crs(EndRange))

st_write(lines_lt, "Data/true_local_lines.shp")
saveRDS(lt_obs_sf, "Data/1000true_local_sf.rds")






#-------------Convert End locations into sf objects---------------------------

############ Single vector (global probs)
gv_ends <- gv_ends %>% 
  dplyr::rename(Latitude = Y, Longitude = X)

gv_ends_sf <- st_as_sf(gv_ends, coords = c('Longitude', 'Latitude'), crs = st_crs(EndRange))

st_write(gv_ends_sf, "Data/vector_global_ends.shp")






########### Single Vector (local probs)

lv_ends <- lv_ends %>% 
  dplyr::rename(Latitude = Y, Longitude = X) #30 km beacon

lv_ends60 <- lv_ends60 %>% 
  dplyr::rename(Latitude = Y, Longitude = X) #60 Km Beacon

lv_ends120 <- lv_ends120 %>% 
  dplyr::rename(Latitude = Y, Longitude = X) #120 km beacon

lv_ends240 <- lv_ends240 %>% 
  dplyr::rename(Latitude = Y, Longitude = X) #240 Km beacon

lv_ends_k0.61 <- lv_ends_k0.61 %>% 
  dplyr::rename(Latitude = Y, Longitude = X)

# Convert to sf object
lv_ends_sf <- st_as_sf(lv_ends, coords = c('Longitude', 'Latitude'), crs = st_crs(EndRange))
lv_ends_sf60 <- st_as_sf(lv_ends60, coords = c('Longitude', 'Latitude'), crs = st_crs(EndRange))
lv_ends_sf120 <- st_as_sf(lv_ends120, coords = c('Longitude', 'Latitude'), crs = st_crs(EndRange))
lv_ends_sf240 <- st_as_sf(lv_ends240, coords = c('Longitude', 'Latitude'), crs = st_crs(EndRange))
lv_ends_sf_k0.61 <- st_as_sf(lv_ends_k0.61, coords = c('Longitude', 'Latitude'), crs = st_crs(EndRange))

st_write(lv_ends_sf, "Data/vector_local_ends.shp")
st_write(lv_ends_sf60, "Data/vector_local_ends60.shp")
st_write(lv_ends_sf120, "Data/vector_local_ends120.shp")
st_write(lv_ends_sf240, "Data/vector_local_ends240.shp")
st_write(lv_ends_sf_k0.61, "Data/vector_local_ends_k0.61.shp")

lv_ends_sf <- read_sf("Data/vector_local_ends.shp")
lv_ends_sf60 <- read_sf("Data/vector_local_ends60.shp")
lv_ends_sf120 <- read_sf("Data/vector_local_ends120.shp")
lv_ends_sf240 <- read_sf("Data/vector_local_ends240.shp")
lv_ends_sf_k0.61 <- st_read("Data/vector_local_ends_k0.61.shp")

########### Multi-vector (local landscape probs)

lmv_ends <- lmv_ends %>% # 30km beacon
  dplyr::rename(Latitude = Y, Longitude = X)

lmv_ends60 <- lmv_ends60 %>% # 60km beacon
  dplyr::rename(Latitude = Y, Longitude = X)

lmv_ends120 <- lmv_ends120 %>% # 120km beacon
  dplyr::rename(Latitude = Y, Longitude = X)

lmv_ends240 <- lmv_ends240 %>% # 240km beacon
  dplyr::rename(Latitude = Y, Longitude = X)


lmv_ends_k0.61 <- lmv_ends_k0.61 %>% 
  dplyr::rename(Latitude = Y, Longitude = X)

# Sf objects of ends
lmv_ends_sf <- st_as_sf(lmv_ends, coords = c('Longitude', 'Latitude'), crs = st_crs(EndRange))
lmv_ends_sf60 <- st_as_sf(lmv_ends60, coords = c('Longitude', 'Latitude'), crs = st_crs(EndRange))
lmv_ends_sf120 <- st_as_sf(lmv_ends120, coords = c('Longitude', 'Latitude'), crs = st_crs(EndRange))
lmv_ends_sf240 <- st_as_sf(lmv_ends240, coords = c('Longitude', 'Latitude'), crs = st_crs(EndRange))
lmv_ends_sf_k0.61 <- st_as_sf(lmv_ends_k0.61, coords = c('Longitude', 'Latitude'), crs = st_crs(EndRange))


st_write(lmv_ends_sf, "Data/multivector_local_ends.shp")
st_write(lmv_ends_sf60, "Data/multivector_local_ends60.shp")
st_write(lmv_ends_sf120, "Data/multivector_local_ends120.shp")
st_write(lmv_ends_sf240, "Data/multivector_local_ends240.shp")
st_write(lmv_ends_sf_k0.61, "Data/multivector_local_ends_k0.61.shp")

lmv_ends_sf <- read_sf("Data/multivector_local_ends.shp")
lmv_ends_sf60 <- read_sf("Data/multivector_local_ends60.shp")
lmv_ends_sf120 <- read_sf("Data/multivector_local_ends120.shp")
lmv_ends_sf240 <- read_sf("Data/multivector_local_ends240.shp")
lmv_ends_sf_k0.61 <- st_read("Data/multivector_local_ends_k0.61.shp")

############## True navigation

lt_ends <- lt_ends %>% 
  dplyr::rename(Latitude = Y, Longitude = X)

lt_ends_k0.61 <- lt_ends_k0.61 %>% 
  dplyr::rename(Latitude = Y, Longitude = X)

#Create SF objects from coords
lt_ends_sf <- st_as_sf(lt_ends, coords = c('Longitude', 'Latitude'), crs = st_crs(EndRange))
lt_ends_sf_k0.61 <- st_as_sf(lt_ends_k0.61, coords = c('Longitude', 'Latitude'), crs = st_crs(EndRange))

#Save Ojects
st_write(lt_ends_sf, "Data/true_local_ends.shp")
st_write(lt_ends_sf_k0.61, "Data/true_local_ends_k0.61.shp")



#--------------------------Data Exploration

#Calculate how many agents reached the end location
nrow(st_intersection(gv_ends_sf, EndRange$geometry))
nrow(st_intersection(lv_ends_sf, EndRange$geometry))
nrow(st_intersection(lmv_ends_sf, EndRange$geometry))
nrow(st_intersection(lt_ends_sf, EndRange$geometry))


agent17 <- lv_ends %>%
  group_by(Agent) %>% 
  filter(Agent == 17)
  



a1 <- ggplot() +
  gg(range) +
  geom_sf(data = lines_gv, color = "yellow")+
  gg(gv_ends_sf)+
  gg(EndRange)

b1 <- ggplot() +
  gg(range) +
  geom_sf(data = lines_lv, color = "yellow")+
  gg(lv_ends_sf)+
  gg(EndRange)

c1 <- ggplot() +
  gg(range) +
  geom_sf(data = lines_lmv, color = "yellow")+
  gg(lmv_ends_sf)+
  gg(ow_buffer)

d1 <- ggplot() +
  gg(range) +
  geom_sf(data = lines_lt, color = "yellow")+
  gg(lt_ends_sf)+
  gg(EndRange)



multiplot(a1, b1, c1, d1, cols =1)


########################### Monarch observation#############################

dat <- data.table::fread("Data/Observations [formated].csv")



# Elminate Hawaii and keep years 2018 onward. 
nov <- dat[Longitude >-140 & year >= 2018 & month>=11]
feb <- dat[Longitude >-140  & year >= 2018 & month<=2]

winter_dat <- rbind(nov, feb)

monarch <- st_as_sf(winter_dat, coords = c("Longitude", "Latitude")) 

# WGS84: EPSG 4326. iNaturalist uses this CRS. 
st_crs(monarch) <- 4326


tmap_mode("view")
tm_basemap(leaflet::providers$OpenStreetMap) +
  tm_shape(monarch) + tm_dots()

#Crop based on east coast
#Create shape to crop the raster to the study region
usa_states <- ne_states(country = "united states of america", returnclass = "sf") %>%
  dplyr::filter(!(name %in%  c("Alaska", "Hawaii", "Washington",
                               "Oregon"))) %>%
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
new_extent <-  extent(original_extent@xmin+22, original_extent@xmax-5, original_extent@ymin, 50)

#Cropped region
cropped_study_region <- crop(study_region, new_extent)


region_sf <- st_as_sf(cropped_study_region)

# Dissolve state borders by using the union operation
dissolved_region_sf <- region_sf %>%
  summarise(geometry = st_union(geometry))

# Load the shapefile back into R
dissolved_region_sf <- st_read("Shapes/Prediction_region.shp")


monarch_points_within_us <- st_intersection(monarch, dissolved_region_sf)
gv_ends_sf <- st_intersection(gv_ends_sf, dissolved_region_sf)


lv_ends_sf <- st_intersection(lv_ends_sf, dissolved_region_sf)
lv_ends_sf60 <- st_intersection(lv_ends_sf60, dissolved_region_sf)
lv_ends_sf120 <- st_intersection(lv_ends_sf120, dissolved_region_sf)
lv_ends_sf240 <- st_intersection(lv_ends_sf240, dissolved_region_sf)
lv_ends_sf_k0.61 <- st_intersection(lv_ends_sf_k0.61, dissolved_region_sf)

lmv_ends_sf <- st_intersection(lmv_ends_sf, dissolved_region_sf)
lmv_ends_sf60 <- st_intersection(lmv_ends_sf60, dissolved_region_sf)
lmv_ends_sf120 <- st_intersection(lmv_ends_sf120, dissolved_region_sf)
lmv_ends_sf240 <- st_intersection(lmv_ends_sf240, dissolved_region_sf)
lmv_ends_sf_k0.61 <- st_intersection(lmv_ends_sf_k0.61, dissolved_region_sf)

lt_ends_sf <- st_intersection(lt_ends_sf, dissolved_region_sf)
lt_ends_sf_k0.61 <- st_intersection(lt_ends_sf_k0.61, dissolved_region_sf)

st_write(dissolved_region_sf, "Shapes/Prediction_region.shp")
st_write(monarch_points_within_us, "Data/monarch_eastern_winter.shp")
st_write(gv_ends_sf, "Data/surviving_gv_ends.shp")


st_write(lv_ends_sf, "Data/surviving_lv_ends.shp")
st_write(lv_ends_sf60, "Data/surviving_lv_ends60.shp")
st_write(lv_ends_sf120, "Data/surviving_lv_ends120.shp")
st_write(lv_ends_sf240, "Data/surviving_lv_ends240.shp")
st_write(lv_ends_sf_k0.61, "Data/surviving_lv_ends_k0.61.shp")


st_write(lmv_ends_sf, "Data/surviving_lmv_ends.shp")
st_write(lmv_ends_sf60, "Data/surviving_lmv_ends60.shp")
st_write(lmv_ends_sf120, "Data/surviving_lmv_ends120.shp")
st_write(lmv_ends_sf240, "Data/surviving_lmv_ends240.shp")
st_write(lmv_ends_sf_k0.61, "Data/surviving_lmv_ends_k0.61.shp")

st_write(lt_ends_sf, "Data/surviving_lt_ends.shp")
st_write(lt_ends_sf_k0.61, "Data/surviving_lt_ends_k0.61.shp")
