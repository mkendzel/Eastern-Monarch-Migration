####Library####
library(inlabru)
library(INLA)
library(sf)
library(raster)
library(spatstat)
library(dplyr)




#### Set Working Directory for Data input ####

setwd("")



#-----------------------------Import Data--------------------------

#Region to make predicitons from
StatesBoundary.shp <- read_sf("Shapes/Prediction_region.shp")
#StatesBoundary.shp <- as(StatesBoundary.shp, "Spatial") #Only needed if using sp objects

#Monarch Obs
monarchs <- read_sf("Data/monarch_eastern_winter.shp")

#Sim Obs
gv_ends_sf <- read_sf("Data/surviving_gv_ends.shp")


lv_ends_sf <-  read_sf("Data/surviving_lv_ends.shp")
lv_ends_sf60 <-  read_sf("Data/surviving_lv_ends60.shp")
lv_ends_sf120 <-  read_sf("Data/surviving_lv_ends120.shp")
lv_ends_sf240 <-  read_sf("Data/surviving_lv_ends240.shp")


lmv_ends_sf <-  read_sf("Data/surviving_lmv_ends.shp")
lmv_ends_sf60 <-  read_sf("Data/surviving_lmv_ends60.shp")
lmv_ends_sf120 <-  read_sf("Data/surviving_lmv_ends120.shp")
lmv_ends_sf240 <-  read_sf("Data/surviving_lmv_ends240.shp")


lt_ends_sf <- read_sf("Data/surviving_lt_ends.shp")

#Covariates
human_density.sp <- readRDS("Covariates/human_density.sp.rds")
altitude.sp <- readRDS("Covariates/altitude.sp.rds")
habitat.sp <- readRDS("Covariates/habitat_cropped_lowres.rds")

#Normalized Covariates
habitat.normalized.sf <- readRDS("Covariates/habitat_normalized_sf.rds")
altitude.normalized.sf <- readRDS("Covariates/altitude_normalized_sf.rds") 
human_density.normalized.sf <- readRDS("Covariates/HumanDensity_normalized_sf.rds") 

#Convert into Sf objects
human_density.sf <- rast(human_density.sp)
altitude.sf <- rast(altitude.sp)
habitat.sf <- rast(habitat.sp)

#######################SIMULATION SPDE##############################

#Mesh Creation---
#Max.edge for mesh
max.edge.gv <- diff(range(st_coordinates(gv_ends_sf)[,1]))/(3*5)

max.edge.lv <- diff(range(st_coordinates(lv_ends_sf)[,1]))/(3*5)
max.edge.lv60 <- diff(range(st_coordinates(lv_ends_sf60)[,1]))/(3*5)
max.edge.lv120 <- diff(range(st_coordinates(lv_ends_sf120)[,1]))/(3*5)
max.edge.lv240 <- diff(range(st_coordinates(lv_ends_sf240)[,1]))/(3*5)



max.edge.lmv <- diff(range(st_coordinates(lmv_ends_sf)[,1]))/(3*5)
max.edge.lmv60 <- diff(range(st_coordinates(lmv_ends_sf60)[,1]))/(3*5)
max.edge.lmv120 <- diff(range(st_coordinates(lmv_ends_sf120)[,1]))/(3*5)
max.edge.lmv240 <- diff(range(st_coordinates(lmv_ends_sf240)[,1]))/(3*5)


max.edge.lt <- diff(range(st_coordinates(lt_ends_sf)[,1]))/(3*5)

#Mesh creation
mesh.gv <- inla.mesh.2d(loc = gv_ends_sf$geometry, boundary = StatesBoundary.shp, 
                         max.edge=max.edge.gv, offset = c(0.5, 1), 
                         cutoff = 0.3)


# Vector Local
mesh.lv <- inla.mesh.2d(loc = lv_ends_sf$geometry, boundary = StatesBoundary.shp, 
                        max.edge=max.edge.lv, offset = c(0.5, 1), 
                        cutoff = 0.3)
mesh.lv60 <- inla.mesh.2d(loc = lv_ends_sf60$geometry, boundary = StatesBoundary.shp, 
                        max.edge=max.edge.lv60, offset = c(0.5, 1), 
                        cutoff = 0.3)
mesh.lv120 <- inla.mesh.2d(loc = lv_ends_sf120$geometry, boundary = StatesBoundary.shp, 
                          max.edge=max.edge.lv120, offset = c(0.5, 1), 
                          cutoff = 0.3)
mesh.lv240 <- inla.mesh.2d(loc = lv_ends_sf240$geometry, boundary = StatesBoundary.shp, 
                           max.edge=max.edge.lv240, offset = c(0.5, 1), 
                           cutoff = 0.3)


# MultiVector Local
mesh.lmv <- inla.mesh.2d(loc = lmv_ends_sf$geometry, boundary = StatesBoundary.shp, 
                        max.edge=max.edge.lmv, offset = c(0.5, 1), 
                        cutoff = 0.3)
mesh.lmv60 <- inla.mesh.2d(loc = lmv_ends_sf60$geometry, boundary = StatesBoundary.shp, 
                         max.edge=max.edge.lmv60, offset = c(0.5, 1), 
                         cutoff = 0.3)
mesh.lmv120 <- inla.mesh.2d(loc = lmv_ends_sf120$geometry, boundary = StatesBoundary.shp, 
                         max.edge=max.edge.lmv120, offset = c(0.5, 1), 
                         cutoff = 0.3)
mesh.lmv240 <- inla.mesh.2d(loc = lmv_ends_sf240$geometry, boundary = StatesBoundary.shp, 
                         max.edge=max.edge.lmv240, offset = c(0.5, 1), 
                         cutoff = 0.3)



#True navigator
mesh.lt <- inla.mesh.2d(loc = lt_ends_sf$geometry, boundary = StatesBoundary.shp, 
                         max.edge=max.edge.lt, offset = c(0.5, 1), 
                         cutoff = 0.3)



################ Monarch Analysis--------------------------------------------------------------
# Monarch Mesh---
#Mesh
max.edge.monarch <- diff(range(st_coordinates(monarchs)[,1]))/(3*5)



mesh.monarch <- inla.mesh.2d(loc = monarchs$geometry, boundary = StatesBoundary.shp, 
                             max.edge=max.edge.monarch, offset = c(0.5, 1), 
                             cutoff = 0.3)



ggplot()+
  #gg(habitat.sp)+
  gg(mesh.monarch)+
  gg(monarchs)




### Model 1: No covariates--------------------

spde.monarch <- inla.spde2.pcmatern(mesh.monarch,
                                    prior.sigma = c(1, 0.01),
                                    prior.range = c(0.2, 0.01))

cmp.formula <- geometry ~ mySmooth(geometry, model = spde.monarch) + Intercept(1)

#Fit model
fit.monarch <- lgcp(cmp.formula, monarchs, samplers = StatesBoundary.shp, 
                    domain = list(geometry = mesh.monarch),
                    options = list(control.inla = list(strategy = "simplified.laplace", int.strategy = "eb")))

fit.monarch <- fit_0
summary(fit.monarch)

pred.monarch <- predict(
  fit.monarch,
  fm_pixels(mesh.monarch, mask = StatesBoundary.shp, format = "sp"),
  ~ data.frame(
    lambda = exp(mySmooth + Intercept),
    loglambda = mySmooth + Intercept
  )
)

pl1 <- ggplot() +
  gg(data = pred.monarch$lambda, aes(color = NULL))+
  #gg(sp_winter_obs, color = "red")+
  geom_tile()+
  gg(StatesBoundary.shp) +
  scale_fill_viridis_c()+
  ggtitle("Observation Locations: Monarchs", subtitle = "(Response Scale)")

pl2 <- ggplot() +
  gg(pred.monarch$loglambda, aes(color = NULL)) +
  geom_tile()+
  gg(StatesBoundary.shp, alpha = 0) +
  scale_fill_viridis_c()+
  ggtitle("", subtitle = "(Log Transformed)")

multiplot(pl1, pl2, cols = 2)

#Full crop
saveRDS(fit.monarch, file = "Fit Results/monarch_spdeonly_fit.rds")
saveRDS(pred.monarch, file = "Fit Results/monarch_spdeonly_pred.rds")

###  Model 2: Habitat with SPDE----------------

#Select which covariate to use
habitat.sf <- habitat.normalized.sf

#Convert CRS for all objects needed
fm_crs(StatesBoundary.shp) <- fm_crs(habitat.sf)
mesh.monarch$crs <- fm_crs(habitat.sf)
fm_crs(monarchs) <- fm_crs(habitat.sf)


#empty prediction data frame
pred.df <- fm_pixels(mesh.monarch, mask = StatesBoundary.shp, format = "sp")


spde.monarch <- inla.spde2.pcmatern(mesh.monarch,
                                    prior.sigma = c(1, 0.01),
                                    prior.range = c(0.2, 0.01))


hcomp <- geometry ~ 
  -1 +
  hab(habitat.sf, model = "factor_full") +
  mySmooth(geometry, model = spde.monarch)


Hfit <- lgcp(hcomp, monarchs, samplers = StatesBoundary.shp, 
             domain = list(geometry = mesh.monarch),
             options = list(control.inla = list(strategy = "simplified.laplace", int.strategy = "eb")))

Hfit_0 <-Hfit


summary(Hfit_0)
summary(Hfit)


h.int_log <- predict(
  Hfit,
  fm_pixels(mesh.monarch, mask = StatesBoundary.shp, format = "sp"),
  ~ data.frame(
    lambda = exp(mySmooth + hab),
    loglambda = mySmooth + hab
  )
)



#Multi
pl1 <- ggplot() +
  gg(StatesBoundary.shp) +
  gg(data = h.int_log$lambda, aes(color = NULL))+
  #gg(sp_winter_obs, color = "red")+
  geom_tile()+
  scale_fill_viridis_c()+
  ggtitle("Observation Locations: Monarchs", subtitle = "(Response Scale)")

pl2 <- ggplot() +
  gg(h.int_log$loglambda, aes(color = NULL)) +
  geom_tile()+
  gg(StatesBoundary.shp, alpha = 0) +
  scale_fill_viridis_c()+
  ggtitle("", subtitle = "(Log Transformed)")

multiplot(pl1, pl2, cols = 2)



#Full save
saveRDS(Hfit, file = "Fit Results/monarch_habitat_normalized_fit.rds")
saveRDS(h.int_log, file = "Fit Results/monarch_habitat_normalized_pred.rds")


###  Model 2: Elevation with SPDE----------------------
#Convert to normalized
altitude.sf <- altitude.normalized.sf

spde.monarch <- inla.spde2.pcmatern(mesh.monarch,
                                    prior.sigma = c(1, 0.01),
                                    prior.range = c(0.2, 0.01))

#Standardized based on mean
elev <- altitude.sf - mean(terra::values(altitude.sf), na.rm = TRUE)




ecomp <- geometry ~ elev(altitude.sf , model = "linear") +
  mySmooth(geometry, model = spde.monarch) + Intercept(1)

efit <- lgcp(ecomp, monarchs, samplers = StatesBoundary.shp, domain = list(geometry = mesh.monarch),
             options = list(control.inla = list(strategy = "simplified.laplace", int.strategy = "eb")))


summary(efit)


e.int_log <- predict(
  efit,
  fm_pixels(mesh.monarch, mask = StatesBoundary.shp, format = "sp"),
  ~ data.frame(
    lambda = exp(mySmooth + elev + Intercept),
    loglambda = mySmooth + elev + Intercept
  )
)


#Multi
pl1 <- ggplot() +
  gg(data = e.int_log$lambda, aes(color = NULL))+
  #gg(sp_winter_obs, color = "red")+
  geom_tile()+
  gg(StatesBoundary.shp, alpha = 0) +
  scale_fill_viridis_c()+
  ggtitle("Observation Locations: Monarchs", subtitle = "(Response Scale)")

pl2 <- ggplot() +
  gg(e.int_log$loglambda, aes(color = NULL)) +
  geom_tile()+
  gg(StatesBoundary.shp, alpha = 0) +
  scale_fill_viridis_c()+
  ggtitle("", subtitle = "(Log Transformed)")

multiplot(pl1, pl2, cols = 2)

#Save full results
saveRDS(efit, file = "Fit Results/monarch_altitude_fit.rds")
saveRDS(e.int_log, file ="Fit Results/monarch_altitude_pred.rds")

###  Model 2: Human Density with SPDE--------------------------------------
fm_crs(StatesBoundary.shp) <- fm_crs(human_density.sf)
mesh.monarch$crs <- fm_crs(human_density.sf)
fm_crs(monarchs) <- fm_crs(human_density.sf)


spde.monarch <- inla.spde2.pcmatern(mesh.monarch,
                                    prior.sigma = c(1, 0.01),
                                    prior.range = c(0.2, 0.01))

#Human density
hum_density <- human_density.sf - mean(terra::values(human_density.sf), na.rm = TRUE)

hum.comp <- geometry ~ hum(human_density.sf , model = "linear") +
  mySmooth(geometry, model = spde.monarch) + Intercept(1)

hum.fit <- lgcp(hum.comp, monarchs, samplers = StatesBoundary.shp, 
                domain = list(geometry = mesh.monarch),
                options = list(control.inla = list(strategy = "simplified.laplace", int.strategy = "eb")))

summary(hum.fit)


hum.int_log <- predict(
  hum.fit,
  fm_pixels(mesh.monarch, mask = StatesBoundary.shp, format = "sp"),
  ~ data.frame(
    lambda = exp(mySmooth + hum + Intercept),
    loglambda = mySmooth + hum + Intercept
  )
)


#Multi
pl1 <- ggplot() +
  gg(data = hum.int_log$lambda, aes(color = NULL))+
  gg(monarchs, color = "red")+
  geom_tile()+
  gg(StatesBoundary.shp, alpha = 0) +
  scale_fill_viridis_c()+
  ggtitle("Observation Locations: Monarchs", subtitle = "(Response Scale)")

pl2 <- ggplot() +
  gg(hum.int_log$loglambda, aes(color = NULL)) +
  geom_tile()+
  gg(StatesBoundary.shp, alpha = 0) +
  scale_fill_viridis_c()+
  ggtitle("", subtitle = "(Log Transformed)")

multiplot(pl1, pl2, cols = 2)

#Save full results
saveRDS(hum.fit, file = "Fit Results/monarch_humandensity_fit.rds")
saveRDS(hum.int_log, file = "Fit Results/monarch_humandensity_pred.rds")



###  Model 3: Elevation and Habitat with SPDE----------------------------
elev <- altitude.sf - mean(terra::values(altitude.sf), na.rm = TRUE)

spde.monarch <- inla.spde2.pcmatern(mesh.monarch,
                                    prior.sigma = c(1, 0.01),
                                    prior.range = c(0.2, 0.01))




hab_elev_comp <- geometry ~ 
  hab(habitat.sf, model = "factor_contrast") + 
  elev(elev, model = "linear") +
  mySmooth(geometry, model = spde.monarch) + Intercept(1)

hab_elev_fit <- lgcp(hab_elev_comp, monarchs, samplers = StatesBoundary.shp, 
                     domain = list(geometry = mesh.monarch),
                     options = list(control.inla = list(strategy = "simplified.laplace", int.strategy = "eb")))


hab_elev_0 <- hab_elev_fit
hab_elev_fit <- bru_rerun(hab_elev_fit)

summary(hab_elev_fit)


hab_elev_log <- predict(
  hab_elev_fit,
  fm_pixels(mesh.monarch, mask = StatesBoundary.shp, format = "sp"),
  ~ data.frame(
    lambda = exp(mySmooth + hab + elev + Intercept),
    loglambda = mySmooth + hab + elev + Intercept
  )
)


#Multi
pl1 <- ggplot() +
  gg(StatesBoundary.shp) +
  gg(data = hab_elev_log$lambda, aes(color = NULL))+
  #gg(sp_winter_obs, color = "red")+
  geom_tile()+
  scale_fill_viridis_c()+
  ggtitle("Observation Locations: Monarchs", subtitle = "(Response Scale)")

pl2 <- ggplot() +
  gg(hab_elev_log$loglambda, aes(color = NULL)) +
  geom_tile()+
  gg(StatesBoundary.shp, alpha = 0) +
  scale_fill_viridis_c()+
  ggtitle("", subtitle = "(Log Transformed)")

multiplot(pl1, pl2, cols = 2)

#Save full resulkts
saveRDS(hab_elev_fit, file = "Fit Results/monarch_elevation_habitat_fit.rds")
saveRDS(hab_elev_log, file = "Fit Results/monarch_elevation_habitat_pred.rds")

### Model 3: Human Density and Habitat with SPDE----------------------------

spde.monarch <- inla.spde2.pcmatern(mesh.monarch,
                                    prior.sigma = c(1, 0.01),
                                    prior.range = c(0.2, 0.01))

#Human density
f.hum <- function(where) {
  # Extract the values
  v <- eval_spatial(human_density.sp, where, layer = "gpw_v4_population_density_adjusted_to_2015_unwpp_country_totals_rev11_2020_30_sec")
  # Fill in missing values
  if (any(is.na(v))) {
    v <- bru_fill_missing(human_density.sp, where, v)
  }
  return(v)
}




hab_hum_comp <- geometry ~ 
  hab(habitat.sf, model = "factor_contrast") + 
  hum(elev, model = "linear") +
  mySmooth(geometry, model = spde.monarch) + Intercept(1)

hab_hum_fit <- lgcp(hab_hum_comp, monarchs, samplers = StatesBoundary.shp, 
                    domain = list(geometry = mesh.monarch),
                    options = list(control.inla = list(strategy = "simplified.laplace", int.strategy = "eb")))

summary(hab_hum_fit)


hab_hum_log <- predict(
  hab_hum_fit,
  fm_pixels(mesh.monarch, mask = StatesBoundary.shp, format = "sp"),
  ~ data.frame(
    lambda = exp(mySmooth + hab + hum + Intercept),
    loglambda = mySmooth + hab + hum + Intercept
  )
)


#Multi
pl1 <- ggplot() +
  gg(data = hab_hum_log$lambda, aes(color = NULL))+
  #gg(sp_winter_obs, color = "red")+
  geom_tile()+
  gg(StatesBoundary.shp, alpha = 0) +
  scale_fill_viridis_c()+
  ggtitle("Observation Locations: Monarchs", subtitle = "(Response Scale)")

pl2 <- ggplot() +
  gg(hab_hum_log$loglambda, aes(color = NULL)) +
  geom_tile()+
  gg(StatesBoundary.shp, alpha = 0) +
  scale_fill_viridis_c()+
  ggtitle("", subtitle = "(Log Transformed)")

multiplot(pl1, pl2, cols = 2)

#Save full results
saveRDS(hab_hum_fit, file = "Fit Results/monarch_humandensity_habitat_fit.rds")
saveRDS(hab_hum_log, file = "Fit Results/monarch_humandensity_habitat_pred.rds")


### Model 3: Elevation and Human density with SPDE----------------------------

spde.monarch <- inla.spde2.pcmatern(mesh.monarch,
                                    prior.sigma = c(1, 0.01),
                                    prior.range = c(0.2, 0.01))




elev_hum_comp <- geometry ~ 
  elev(elev, model = "linear") +
  hum(human_density.sf, model = "linear") +
  mySmooth(geometry, model = spde.monarch) + Intercept(1)

elev_hum_fit <- lgcp(elev_hum_comp, monarchs, samplers = StatesBoundary.shp, 
                     domain = list(geometry = mesh.monarch),
                     options = list(control.inla = list(strategy = "simplified.laplace", int.strategy = "eb")))

summary(elev_hum_fit)


elev_hum_log <- predict(
  elev_hum_fit,
  fm_pixels(mesh.monarch, mask = StatesBoundary.shp, format = "sp"),
  ~ data.frame(
    lambda = exp(mySmooth + elev + hum + Intercept),
    loglambda = mySmooth + elev + hum + Intercept
  )
)


#Multi
pl1 <- ggplot() +
  gg(data = elev_hum_log$lambda, aes(color = NULL))+
  #gg(sp_winter_obs, color = "red")+
  geom_tile()+
  gg(StatesBoundary.shp) +
  scale_fill_viridis_c()+
  ggtitle("Observation Locations: Monarchs", subtitle = "(Response Scale)")

pl2 <- ggplot() +
  gg(elev_hum_log$loglambda, aes(color = NULL)) +
  geom_tile()+
  gg(StatesBoundary.shp, alpha = 0) +
  scale_fill_viridis_c()+
  ggtitle("", subtitle = "(Log Transformed)")

multiplot(pl1, pl2, cols = 2)

#Save full results

saveRDS(elev_hum_fit, file = "Fit Results/monarch_elevation_humandensity_fit.rds")
saveRDS(elev_hum_log, file = "Fit Results/monarch_elevation_humandensity_pred.rds")


###  Model 4: Elevation, habitat, and population density-----------------

spde.monarch <- inla.spde2.pcmatern(mesh.monarch,
                                    prior.sigma = c(1, 0.01),
                                    prior.range = c(0.2, 0.01))



full_comp <- geometry ~ 
  elev(elev, model = "linear") + 
  hum(human_density.sf, model = "linear") + 
  hab(habitat.sf,  model = "factor_contrast")+
  mySmooth(geometry, model = spde.monarch) + Intercept(1)

full_fit <- lgcp(full_comp, monarchs, samplers = StatesBoundary.shp, 
                 domain = list(geometry = mesh.monarch),
                 options = list(control.inla = list(strategy = "simplified.laplace", int.strategy = "eb")))


summary(full_fit)

full.int_log <- predict(
  full_fit,
  fm_pixels(mesh.monarch, mask = StatesBoundary.shp, format = "sp"),
  ~ data.frame(
    lambda = exp(mySmooth + elev + hum + hab + Intercept),
    loglambda = mySmooth + elev + hum + hab+ Intercept
  )
)


#Multi
pl1 <- ggplot() +
  gg(data = full.int_log$lambda, aes(color = NULL))+
  gg(monarchs, color = "red")+
  geom_tile()+
  gg(StatesBoundary.shp, alpha = 0) +
  scale_fill_viridis_c()+
  ggtitle("Observation Locations: Monarchs", subtitle = "(Response Scale)")

pl2 <- ggplot() +
  gg(full.int_log$loglambda, aes(color = NULL)) +
  geom_tile()+
  gg(StatesBoundary.shp, alpha = 0) +
  scale_fill_viridis_c()+
  ggtitle("", subtitle = "(Log Transformed)")

multiplot(pl1, pl2, cols = 2)

#save full results
saveRDS(full_fit, "Fit Results/monarchs_full_fit.rds")
saveRDS(full.int_log, "Fit Results/monarchs_full_pred.rds")


### model DIC comparisons ---------------------------

smooth_only <- readRDS(file = "Fit Results/monarch_spdeonly_fit.rds")
habitat <- readRDS(file = "Fit Results/monarch_habitat_fit.rds")
elevation <- readRDS(file = "Fit Results/monarch_altitude_fit.rds")
human_density <- readRDS(file = "Fit Results/monarch_humandensity_fit.rds")
habitat_elevation <- readRDS(file = "Fit Results/monarch_elevation_habitat_fit.rds")
habitat_HumanDensity <- readRDS(file = "Fit Results/monarch_humandensity_habitat_fit.rds")
elevation_HumanDensity <- readRDS(file = "Fit Results/monarch_elevation_humandensity_fit.rds")
all_cov <- readRDS("Fit Results/monarchs_full_fit.rds")

#Full DIC
knitr::kable(deltaIC(smooth_only, habitat, elevation, human_density, habitat_elevation, habitat_HumanDensity, elevation_HumanDensity, all_cov, criterion = c("DIC")))


saveRDS(DIC_TABLE, file = "DIC_TABLE_Cropped_Results.rds")

###############Relative Mean Square Error##############

###################Relative Ratio Simm vs Monarchs#####
#-------------Import Prediction surfaces------------
Vector_global_pred <- readRDS("Fit Results/Vector_global_pred.rds")
Vector_local_pred <- readRDS("Fit Results/Vector_local_pred.rds")
MultiVector_local_pred <- readRDS("Fit Results/MultiVector_local_pred.rds")
True_local_pred <- readRDS("Fit Results/True_local_pred.rds")
Monarch_pred <- readRDS("Fit Results/monarch_habitat_pred.rds")



#--------------Extract values-----------------------------
Vector_global.lambda <- as.data.frame(Vector_global_pred$loglambda$mean)
Vector_local.lambda <- as.data.frame(Vector_local_pred$loglambda$mean)
MultiVector_local.lambda <- as.data.frame(MultiVector_local_pred$loglambda$mean)
True_local.lambda <- as.data.frame(True_local_pred$loglambda$mean)
monarch.lambda <- as.data.frame(Monarch_pred$loglambda$mean)




#Stores coordinates from original prediction (should match for each data frame)
dataframe_for_cords <- as.data.frame(Vector_global_pred$lambda)

#Monarchs is to short so add an NA
#monarch.lambda[nrow(monarch.lambda) + 1, ] <- NA


#-------------------Calculate Difference-------------------------
#Original
log_rd_vg <-  monarch.lambda - Vector_global.lambda
log_rd_vl <- monarch.lambda - Vector_local.lambda
log_rd_mvl <- monarch.lambda - MultiVector_local.lambda
log_rd_tl <- monarch.lambda - True_local.lambda




##Link Comparisons to locations##
#Vector Global
rd_vg_df <- cbind(log_rd_vg, dataframe_for_cords$coords.x1, dataframe_for_cords$coords.x2)
names(rd_vg_df) <- c("log.difference", "coords.x1", "coords.x2")

#Vector Local
rd_vl_df <- cbind(log_rd_vl, dataframe_for_cords$coords.x1, dataframe_for_cords$coords.x2)
names(rd_vl_df) <- c("log.difference", "coords.x1", "coords.x2")

#MultiVector
rd_mvl_df <- cbind(log_rd_mvl, dataframe_for_cords$coords.x1, dataframe_for_cords$coords.x2)
names(rd_mvl_df) <- c("log.difference", "coords.x1", "coords.x2")

#True
rd_tl_df <- cbind(log_rd_tl, dataframe_for_cords$coords.x1, dataframe_for_cords$coords.x2)
names(rd_tl_df) <- c("log.difference", "coords.x1", "coords.x2")


##turn df to spatial points##

rd_vg_sp <- SpatialPointsDataFrame(
  coords = rd_vg_df[, 2:3],
  data = rd_vg_df
)


rd_vl_sp <- SpatialPointsDataFrame(
  coords = rd_vl_df[, 2:3],
  data = rd_vl_df
)

rd_mvl_sp <- SpatialPointsDataFrame(
  coords = rd_mvl_df[, 2:3],
  data = rd_mvl_df
)

rd_tl_sp <- SpatialPointsDataFrame(
  coords = rd_tl_df[, 2:3],
  data = rd_tl_df
)


# Convert to SpatialPixelsDataFrame

rd_vg_spdf <- as(rd_vg_sp, "SpatialPixelsDataFrame")
rd_vl_spdf <- as(rd_vl_sp, "SpatialPixelsDataFrame")
rd_mvl_spdf <- as(rd_mvl_sp, "SpatialPixelsDataFrame")
rd_tl_spdf <- as(rd_tl_sp, "SpatialPixelsDataFrame")

saveRDS(rd_vg_spdf, "Shapes/Vector_global_RD.rds")
saveRDS(rd_vl_spdf, "Shapes/Vector_local_RD.rds")
saveRDS(rd_mvl_spdf, "Shapes/MultiVector_local_RD.rds")
saveRDS(rd_tl_spdf, "Shapes/True_local_RD.rds")







################### Table 1: Number Reaching overwintering location#############

lv_ends_sf <-  read_sf("Data/surviving_lv_ends.shp")
lv_ends_sf60 <-  read_sf("Data/surviving_lv_ends60.shp")
lv_ends_sf120 <-  read_sf("Data/surviving_lv_ends120.shp")
lv_ends_sf240 <-  read_sf("Data/surviving_lv_ends240.shp")

lmv_ends_sf <-  read_sf("Data/surviving_lmv_ends.shp")
lmv_ends_sf60 <-  read_sf("Data/surviving_lmv_ends60.shp")
lmv_ends_sf120 <-  read_sf("Data/surviving_lmv_ends120.shp")
lmv_ends_sf240 <-  read_sf("Data/surviving_lmv_ends240.shp")
lt_ends_sf <- read_sf("Data/surviving_lt_ends.shp")


EndRange <- read_sf("Shapes/OverwinterBuffer.shp")
EndRange240 <- read_sf("Shapes/OverwinterBuffer240kilo.shp")
#At overwintering locations
st_intersection(lv_ends_sf, EndRange)
st_intersection(lv_ends_sf60, EndRange)
st_intersection(lv_ends_sf120, EndRange)
st_intersection(lv_ends_sf240, EndRange)

st_intersection(lmv_ends_sf, EndRange)
st_intersection(lmv_ends_sf60, EndRange)
st_intersection(lmv_ends_sf120, EndRange)
st_intersection(lmv_ends_sf240, EndRange)

st_intersection(lt_ends_sf, EndRange)



#Within 240 km
st_intersection(lv_ends_sf, EndRange240)
st_intersection(lv_ends_sf60, EndRange240)
st_intersection(lv_ends_sf120, EndRange240)
st_intersection(lv_ends_sf240, EndRange240)

st_intersection(lmv_ends_sf, EndRange240)
st_intersection(lmv_ends_sf60, EndRange240)
st_intersection(lmv_ends_sf120, EndRange240)
st_intersection(lmv_ends_sf240, EndRange240)

st_intersection(lt_ends_sf, EndRange240)
