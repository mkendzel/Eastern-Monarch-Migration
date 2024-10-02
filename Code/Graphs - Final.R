####Library####
library(sf)
library(ggplot2)
library(rnaturalearth)
library(mapdata)



#### Set Working Directory for Data input ####

#Home computer
setwd("")


#-----------------Data import----------------------------------

#Simulation final locations
gv_ends_sf <- read_sf("Data/vector_global_ends.shp")
lv_ends_sf <- read_sf("Data/vector_local_ends.shp")
lmv_ends_sf <- read_sf("Data/multivector_local_ends.shp")
lt_ends_sf <- read_sf("Data/true_local_ends.shp")

#Simulation paths
lines_gv <- read_sf("Data/vector_global_lines.shp")
lines_lv <- read_sf("Data/vector_local_lines.shp")
lines_lmv <- read_sf("Data/multivector_local_lines.shp")
lines_lt <- read_sf("Data/true_local_lines.shp")

#Monarch Observations
monarchs <- read_sf("Data/monarch_eastern_winter.shp")

#Prediction Surfaces
Vector_global_pred <- readRDS("Fit Results/Vector_global_pred.rds")
Vector_local_pred <- readRDS("Fit Results/Vector_local_pred.rds")
Vector_local_pred_k0.61 <- readRDS("Fit Results/Vector_local_pred_k0.61.rds")
MultiVector_local_pred <- readRDS("Fit Results/MultiVector_local_pred.rds")
MultiVector_local_pred_k0.61 <- readRDS("Fit Results/MultiVector_local_pred_k0.61.rds")
True_local_pred <- readRDS("Fit Results/True_local_pred.rds")
True_local_pred_k0.61 <- readRDS(file = "Fit Results/True_local_pred_k0.61.rds")
Monarch_pred <- readRDS("Fit Results/monarch_habitat_pred.rds")

#Relative Ratio
rd_vg_spdf <- readRDS("Shapes/Vector_global_RD.rds")
rd_vl_spdf <- readRDS("Shapes/Vector_local_RD.rds")
rd_mvl_spdf <- readRDS("Shapes/MultiVector_local_RD.rds")
rd_tl_spdf <- readRDS("Shapes/True_local_RD.rds")

#Shapes
range <- read_sf("Shapes/FullStudyRegion.shp")
EndRange <- read_sf("Shapes/OverwinterBuffer.shp")
StatesBoundary.shp <- read_sf("Shapes/Prediction_region.shp")
StatesBoundary.shp <- as(StatesBoundary.shp, "Spatial")


#-------------Formating Data----------------------------------
st_crs(lines_gv)<- st_crs(range)
st_crs(lines_lv)<- st_crs(range)

st_crs(lines_lt)<- st_crs(range)

############      FIGURE 1   ###################
usa <- map_data("state")
canada <- map_data("worldHires", "Canada")
mexico <- map_data("worldHires", "Mexico")

usa <- map_data("state")
canada <- map_data("worldHires", "Canada")
mexico <- map_data("worldHires", "Mexico")

NAmap <- ggplot() + geom_polygon(data = usa, 
                                 aes(x=long, y = lat, group = group), 
                                 fill = "white", 
                                 color="black") +
  geom_polygon(data = canada, aes(x=long, y = lat, group = group), 
               fill = "white", color="black") + 
  geom_polygon(data = mexico, aes(x=long, y = lat, group = group), 
               fill = "white", color="black") +
  #geom_point(data = sites, aes(x = Longitude, y = Latitude, fill = Group), size = 2, 
  #shape = 23)+
  theme(legend.position="none")+
  coord_fixed(xlim = c(-105, -70),  ylim = c(18, 49), ratio = 1.2)+
  theme(axis.text.x = element_text(size = 25),
        axis.text.y = element_text(size = 25))+
  xlab(NULL) + ylab(NULL)
  NAmap


#-----Column 1: Raw Data Locations and Pathways-------------------
st_crs(lines_lmv)<- st_crs(range)

a1 <- ggplot() +
  gg(range) +
  geom_sf(data = lines_gv, color = "yellow")+
  gg(gv_ends_sf, color = "red", size = 3)+
  gg(EndRange, color = "purple", fill = "purple")+
  theme_minimal()+
  scale_x_continuous(breaks = seq(-120, -80, by = 20)) +  
  scale_y_continuous(breaks = seq(15, 50, by = 10))+
  theme(axis.text.x = element_text(size = 25),
        axis.text.y = element_text(size = 25),
        panel.grid.major = element_line(size = 1.5))
  
  

b1 <- ggplot() +
  gg(range) +
  geom_sf(data = lines_lv, color = "yellow")+
  gg(lv_ends_sf, color = "red", size = 3)+
  gg(EndRange, color = "purple", fill = "purple")+
  theme_minimal()+
  scale_x_continuous(breaks = seq(-120, -80, by = 20)) +  
  scale_y_continuous(breaks = seq(15, 50, by = 10))+
  theme(axis.text.x = element_text(size = 25),
        axis.text.y = element_text(size = 25),
        panel.grid.major = element_line(size = 1.5))

c1 <- ggplot() +
  gg(range) +
  geom_sf(data = lines_lmv, color = "yellow")+
  gg(lmv_ends_sf, color = "red", size = 3)+
  gg(EndRange, color = "purple", fill = "purple")+
  theme_minimal()+
  scale_x_continuous(breaks = seq(-120, -80, by = 20)) +  
  scale_y_continuous(breaks = seq(15, 50, by = 10))+
  theme(axis.text.x = element_text(size = 25),
        axis.text.y = element_text(size = 25),
        panel.grid.major = element_line(size = 1.5))

d1 <- ggplot() +
  gg(range) +
  geom_sf(data = lines_lt, color = "yellow")+
  gg(lt_ends_sf, color = "red", size = 3)+
  gg(EndRange, color = "purple", fill = "purple")+
  theme_minimal()+
  scale_x_continuous(breaks = seq(-120, -80, by = 20)) +  
  scale_y_continuous(breaks = seq(15, 50, by = 10))+
  theme(axis.text.x = element_text(size = 25),
        axis.text.y = element_text(size = 25),
        panel.grid.major = element_line(size = 1.5))

e1 <- ggplot() +
  gg(range) +
  gg(monarchs, size = 3)+
  gg(EndRange, color = "purple", fill = "purple")+
  theme_minimal()+
  scale_x_continuous(breaks = seq(-120, -80, by = 20)) +  
  scale_y_continuous(breaks = seq(15, 50, by = 10))+
  theme(axis.text.x = element_text(size = 25),
        axis.text.y = element_text(size = 25),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.major = element_line(size = 1.5))

ggarrange(a1, b1, c1, d1, e1, ncol = 1)

#-----Column 2: Density surfaces------------

a2 <- ggplot() +
  gg(StatesBoundary.shp) +
  gg(data = Vector_global_pred$loglambda, aes(color = NULL))+
  geom_tile()+
  scale_fill_viridis_c(name = "log(mean)")+
  #gg(EndRange, color = "purple", fill = "purple")+
  theme_minimal()+
  scale_x_continuous(breaks = seq(-120, -80, by = 20)) +  
  scale_y_continuous(breaks = seq(15, 50, by = 10))+
  theme(axis.text.x = element_text(size = 25),
        axis.text.y = element_text(size = 25),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position = c(0.95, 0.05),  # Adjust x, y coordinates to position the legend
        legend.justification = c(1, 0),
        legend.key.size = unit(1, "cm"),
        legend.text = element_text(size = 20),
        legend.title = element_text(size = 20),
        panel.grid.major = element_line(size = 1.5))

b2 <- ggplot() +
  gg(StatesBoundary.shp) +
  gg(data = Vector_local_pred$loglambda, aes(color = NULL))+
  geom_tile()+
  scale_fill_viridis_c(name = "log(mean)")+
  #gg(EndRange, color = "purple", fill = "purple")+
  theme_minimal()+
  scale_x_continuous(breaks = seq(-120, -80, by = 20)) +  
  scale_y_continuous(breaks = seq(15, 50, by = 10))+
  theme(axis.text.x = element_text(size = 25),
        axis.text.y = element_text(size = 25),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position = c(0.95, 0.05),  # Adjust x, y coordinates to position the legend
        legend.justification = c(1, 0),
        legend.key.size = unit(1, "cm"),
        legend.text = element_text(size = 20),
        legend.title = element_text(size = 20),
        panel.grid.major = element_line(size = 1.5))

c2 <- ggplot() +
  gg(StatesBoundary.shp) +
  gg(data = MultiVector_local_pred$loglambda, aes(color = NULL))+
  geom_tile()+
  scale_fill_viridis_c(name = "log(mean)")+
  #gg(EndRange, color = "purple", fill = "purple")+
  theme_minimal()+
  scale_x_continuous(breaks = seq(-120, -80, by = 20)) +  
  scale_y_continuous(breaks = seq(15, 50, by = 10))+
  theme(axis.text.x = element_text(size = 25),
        axis.text.y = element_text(size = 25),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position = c(0.95, 0.05),  # Adjust x, y coordinates to position the legend
        legend.justification = c(1, 0),
        legend.key.size = unit(1, "cm"),
        legend.text = element_text(size = 20),
        legend.title = element_text(size = 20),
        panel.grid.major = element_line(size = 1.5))

d2 <- ggplot() +
  gg(StatesBoundary.shp) +
  gg(data = True_local_pred$loglambda, aes(color = NULL))+
  geom_tile()+
  scale_fill_viridis_c(name = "log(mean)")+
  #gg(EndRange, color = "purple", fill = "purple")+
  theme_minimal()+
  scale_x_continuous(breaks = seq(-120, -80, by = 20)) +  
  scale_y_continuous(breaks = seq(15, 50, by = 10))+
  theme(axis.text.x = element_text(size = 25),
        axis.text.y = element_text(size = 25),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position = c(0.95, 0.05),  # Adjust x, y coordinates to position the legend
        legend.justification = c(1, 0),
        legend.key.size = unit(1, "cm"),
        legend.text = element_text(size = 20),
        legend.title = element_text(size = 20),
        panel.grid.major = element_line(size = 1.5))

e2 <- ggplot() +
  gg(StatesBoundary.shp) +
  gg(data = Monarch_pred$loglambda, aes(color = NULL))+
  geom_tile()+
  scale_fill_viridis_c(name = "log(mean)")+
  #gg(EndRange, color = "purple", fill = "purple")+
  theme_minimal()+
  scale_x_continuous(breaks = seq(-120, -80, by = 20)) +  
  scale_y_continuous(breaks = seq(15, 50, by = 10))+
  theme(axis.text.x = element_text(size = 25),
        axis.text.y = element_text(size = 25),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position = c(0.95, 0.05),  # Adjust x, y coordinates to position the legend
        legend.justification = c(1, 0),
        legend.key.size = unit(1, "cm"),
        legend.text = element_text(size = 20),
        legend.title = element_text(size = 20),
        panel.grid.major = element_line(size = 1.5))
  
ggarrange(a2, b2, c2, d2, e2, ncol = 1)
#-------------Column 3: Relative Risk Ratio--------------------


common_limits <-  c(-10, 10)
colours_rr <- c("cyan", "black", "red")




a3 <- ggplot() +
  gg(rd_vg_spdf, aes(color = NULL)) +
  geom_tile()+
  gg(StatesBoundary.shp, alpha = 0) +
  scale_fill_gradientn(limits = common_limits,
                       colours = c("#1f78b4", "#b2b2b2", "#e31a1c"),
                       oob = scales::squish,
                       name = "log(obs/sim)")+
  theme_minimal()+
  scale_x_continuous(breaks = seq(-120, -80, by = 20)) +  
  scale_y_continuous(breaks = seq(15, 50, by = 10))+
  theme(axis.text.x = element_text(size = 25),
        axis.text.y = element_text(size = 25),
        axis.title.x = element_blank(),  # Hide x axis label
        axis.title.y = element_blank(),
        legend.position = c(0.95, 0.05),  # Adjust x, y coordinates to position the legend
        legend.justification = c(1, 0),
        legend.key.size = unit(1, "cm"),
        legend.text = element_text(size = 20),
        legend.title = element_text(size = 20),
        plot.margin = unit(c(-1, -1, 0, 0), "cm"),
        panel.grid.major = element_line(size = 1.5))+
  ggtitle("", subtitle = "")+
  annotate("text", x = -100, y = 13,
    label = paste("Value Range:", round(min(rd_vg_spdf$log.difference)), "to", round(max(rd_vg_spdf$log.difference))), # Dynamic label based on data range
    color = "black",                  # Text color
    size = 6)     

b3 <- ggplot() +
  gg(rd_vl_spdf, aes(color = NULL)) +
  geom_tile()+
  gg(StatesBoundary.shp, alpha = 0) +
  scale_fill_gradientn(limits = common_limits,
                       colours = c("#1f78b4", "#b2b2b2", "#e31a1c"),
                       oob = scales::squish,
                       name = "log(obs/sim)")+
  theme_minimal()+
  scale_x_continuous(breaks = seq(-120, -80, by = 20)) +  
  scale_y_continuous(breaks = seq(15, 50, by = 10))+
  theme(axis.text.x = element_text(size = 25),
        axis.text.y = element_text(size = 25),
        axis.title.x = element_blank(),  # Hide x axis label
        axis.title.y = element_blank(),
        legend.position = c(0.95, 0.05),  # Adjust x, y coordinates to position the legend
        legend.justification = c(1, 0),
        legend.key.size = unit(1, "cm"),
        legend.text = element_text(size = 20),
        legend.title = element_text(size = 20),
        plot.margin = unit(c(-1, -1, 0, 0), "cm"),
        panel.grid.major = element_line(size = 1.5))+
  ggtitle("", subtitle = "")+
  annotate(
    "text", x = -100, y = 13,          # Adjust x, y coordinates for text placement
    label = paste("Value Range:", round(min(rd_vl_spdf$log.difference)), "to", round(max(rd_vl_spdf$log.difference))), # Dynamic label based on data range
    color = "black",                  # Text color
    size = 6)     


c3 <- ggplot() +
  gg(rd_mvl_spdf, aes(color = NULL)) +
  geom_tile()+
  gg(StatesBoundary.shp, alpha = 0) +
  scale_fill_gradientn(limits = common_limits,
                       colours = c("#1f78b4", "#b2b2b2", "#e31a1c"),
                       oob = scales::squish,
                       name = "log(obs/sim)")+
  theme_minimal()+
  scale_x_continuous(breaks = seq(-120, -80, by = 20)) +  
  scale_y_continuous(breaks = seq(15, 50, by = 10))+
  theme(axis.text.x = element_text(size = 25),
        axis.text.y = element_text(size = 25),
        axis.title.x = element_blank(),  # Hide x axis label
        axis.title.y = element_blank(),
        legend.position = c(0.95, 0.05),  # Adjust x, y coordinates to position the legend
        legend.justification = c(1, 0),
        legend.key.size = unit(1, "cm"),
        legend.text = element_text(size = 20),
        legend.title = element_text(size = 20),
        plot.margin = unit(c(-1, -1, 0, 0), "cm"),
        panel.grid.major = element_line(size = 1.5))+
  ggtitle("", subtitle = "")+
  annotate(
    "text", x = -100, y = 13,          # Adjust x, y coordinates for text placement
    label = paste("Value Range:", round(min(rd_mvl_spdf$log.difference)), "to", round(max(rd_mvl_spdf$log.difference))), # Dynamic label based on data range
    color = "black",                  # Text color
    size = 6)     

d3 <- ggplot() +
  gg(rd_tl_spdf, aes(color = NULL)) +
  geom_tile()+
  gg(StatesBoundary.shp, alpha = 0) +
  scale_fill_gradientn(limits = common_limits,
                       colours = c("#1f78b4", "#b2b2b2", "#e31a1c"),
                       oob = scales::squish,
                       name = "log(obs/sim)")+
  theme_minimal()+
  scale_x_continuous(breaks = seq(-120, -80, by = 20)) +  
  scale_y_continuous(breaks = seq(15, 50, by = 10))+
  theme(axis.text.x = element_text(size = 25),
        axis.text.y = element_text(size = 25),
        axis.title.x = element_blank(),  # Hide x axis label
        axis.title.y = element_blank(),
        legend.position = c(0.95, 0.05),  # Adjust x, y coordinates to position the legend
        legend.justification = c(1, 0),
        legend.key.size = unit(1, "cm"),
        legend.text = element_text(size = 20),
        legend.title = element_text(size = 20),
        plot.margin = unit(c(-1, -1, 0, 0), "cm"),
        panel.grid.major = element_line(size = 1.5))+
  ggtitle("", subtitle = "")+
  annotate(
    "text", x = -100, y = 13,          # Adjust x, y coordinates for text placement
    label = paste("Value Range:", round(min(rd_tl_spdf$log.difference)), "to", round(max(rd_tl_spdf$log.difference))), # Dynamic label based on data range
    color = "black",                  # Text color
    size = 6)             





#---------------fIGURE 2: Varying beacon size ---------------------
#import Data
Vector_local_pred <- readRDS("Fit Results/Vector_local_pred.rds")
Vector_local_pred60 <- readRDS("Fit Results/Vector_local_pred60.rds")
Vector_local_pred120 <- readRDS("Fit Results/Vector_local_pred120.rds")
Vector_local_pred240 <- readRDS("Fit Results/Vector_local_pred240.rds")

MultiVector_local_pred <- readRDS("Fit Results/MultiVector_local_pred.rds")
MultiVector_local_pred60 <- readRDS("Fit Results/MultiVector_local_pred60.rds")
MultiVector_local_pred120 <- readRDS("Fit Results/MultiVector_local_pred120.rds")
MultiVector_local_pred240 <- readRDS("Fit Results/MultiVector_local_pred240.rds")

#Import shapes
EndRange <- read_sf("Shapes/OverwinterBuffer.shp")
EndRange60 <- read_sf("Shapes/OverwinterBuffer60kilo.shp")
EndRange120 <- read_sf("Shapes/OverwinterBuffer120kilo.shp")
EndRange240 <- read_sf("Shapes/OverwinterBuffer240kilo.shp")

StatesBoundary.shp <- read_sf("Shapes/Prediction_region.shp")
StatesBoundary.shp <- as(StatesBoundary.shp, "Spatial")

#---------------30 km Beacon
ggplot() +
  gg(StatesBoundary.shp) +
  gg(data = Vector_local_pred$loglambda, aes(color = NULL))+
  geom_tile()+
  scale_fill_viridis_c(name = "log(mean)")+
  gg(EndRange, color = "purple", lwd = 1, alpha = 0)+
  theme_minimal()+
  coord_sf(xlim = c(-110, -85), ylim = c(15, 30))+
  scale_x_continuous(breaks = seq(-120, -80, by = 20)) +  
  scale_y_continuous(breaks = seq(15, 50, by = 10),)+
  theme(axis.text.x = element_text(size = 25),
        axis.text.y = element_text(size = 25),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(), 
        legend.justification = c(1, 0),
        legend.key.size = unit(1, "cm"),
        legend.text = element_text(size = 20),
        legend.title = element_text(size = 20),
        panel.grid.major = element_line(size = 1.5))

ggplot() +
  gg(StatesBoundary.shp) +
  gg(data = MultiVector_local_pred$loglambda, aes(color = NULL))+
  geom_tile()+
  scale_fill_viridis_c(name = "log(mean)")+
  gg(EndRange, color = "purple", lwd = 1, alpha = 0)+
  theme_minimal()+
  coord_sf(xlim = c(-110, -85), ylim = c(15, 30))+
  scale_x_continuous(breaks = seq(-120, -80, by = 20)) +  
  scale_y_continuous(breaks = seq(15, 50, by = 10),)+
  theme(axis.text.x = element_text(size = 25),
        axis.text.y = element_text(size = 25),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(), 
        legend.justification = c(1, 0),
        legend.key.size = unit(1, "cm"),
        legend.text = element_text(size = 20),
        legend.title = element_text(size = 20),
        panel.grid.major = element_line(size = 1.5))


#--------------60 km Beacon

ggplot() +
  gg(StatesBoundary.shp) +
  gg(data = Vector_local_pred60$loglambda, aes(color = NULL))+
  geom_tile()+
  scale_fill_viridis_c(name = "log(mean)")+
  gg(EndRange60, color = "purple", lwd = 1, alpha = 0)+
  theme_minimal()+
  coord_sf(xlim = c(-110, -85), ylim = c(15, 30))+
  scale_x_continuous(breaks = seq(-120, -80, by = 20)) +  
  scale_y_continuous(breaks = seq(15, 50, by = 10),)+
  theme(axis.text.x = element_text(size = 25),
        axis.text.y = element_text(size = 25),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(), 
        legend.justification = c(1, 0),
        legend.key.size = unit(1, "cm"),
        legend.text = element_text(size = 20),
        legend.title = element_text(size = 20),
        panel.grid.major = element_line(size = 1.5))

ggplot() +
  gg(StatesBoundary.shp) +
  gg(data = MultiVector_local_pred60$loglambda, aes(color = NULL))+
  geom_tile()+
  scale_fill_viridis_c(name = "log(mean)")+
  gg(EndRange60, color = "purple", lwd = 1, alpha = 0)+
  theme_minimal()+
  coord_sf(xlim = c(-110, -85), ylim = c(15, 30))+
  scale_x_continuous(breaks = seq(-120, -80, by = 20)) +  
  scale_y_continuous(breaks = seq(15, 50, by = 10),)+
  theme(axis.text.x = element_text(size = 25),
        axis.text.y = element_text(size = 25),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(), 
        legend.justification = c(1, 0),
        legend.key.size = unit(1, "cm"),
        legend.text = element_text(size = 20),
        legend.title = element_text(size = 20),
        panel.grid.major = element_line(size = 1.5))

#--------------120 km Beacon

ggplot() +
  gg(StatesBoundary.shp) +
  gg(data = Vector_local_pred120$loglambda, aes(color = NULL))+
  geom_tile()+
  scale_fill_viridis_c(name = "log(mean)")+
  gg(EndRange120, color = "purple", lwd = 1, alpha = 0)+
  theme_minimal()+
  coord_sf(xlim = c(-110, -85), ylim = c(15, 30))+
  scale_x_continuous(breaks = seq(-120, -80, by = 20)) +  
  scale_y_continuous(breaks = seq(15, 50, by = 10),)+
  theme(axis.text.x = element_text(size = 25),
        axis.text.y = element_text(size = 25),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(), 
        legend.justification = c(1, 0),
        legend.key.size = unit(1, "cm"),
        legend.text = element_text(size = 20),
        legend.title = element_text(size = 20),
        panel.grid.major = element_line(size = 1.5))

ggplot() +
  gg(StatesBoundary.shp) +
  gg(data = MultiVector_local_pred120$loglambda, aes(color = NULL))+
  geom_tile()+
  scale_fill_viridis_c(name = "log(mean)")+
  gg(EndRange120, color = "purple", lwd = 1, alpha = 0)+
  theme_minimal()+
  coord_sf(xlim = c(-110, -85), ylim = c(15, 30))+
  scale_x_continuous(breaks = seq(-120, -80, by = 20)) +  
  scale_y_continuous(breaks = seq(15, 50, by = 10),)+
  theme(axis.text.x = element_text(size = 25),
        axis.text.y = element_text(size = 25),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(), 
        legend.justification = c(1, 0),
        legend.key.size = unit(1, "cm"),
        legend.text = element_text(size = 20),
        legend.title = element_text(size = 20),
        panel.grid.major = element_line(size = 1.5))


#--------------240 km Beacon

ggplot() +
  gg(StatesBoundary.shp) +
  gg(data = Vector_local_pred240$loglambda, aes(color = NULL))+
  geom_tile()+
  scale_fill_viridis_c(name = "log(mean)")+
  gg(EndRange240, color = "purple", lwd = 1, alpha = 0)+
  theme_minimal()+
  coord_sf(xlim = c(-110, -85), ylim = c(15, 30))+
  scale_x_continuous(breaks = seq(-120, -80, by = 20)) +  
  scale_y_continuous(breaks = seq(15, 50, by = 10),)+
  theme(axis.text.x = element_text(size = 25),
        axis.text.y = element_text(size = 25),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(), 
        legend.justification = c(1, 0),
        legend.key.size = unit(1, "cm"),
        legend.text = element_text(size = 20),
        legend.title = element_text(size = 20),
        panel.grid.major = element_line(size = 1.5))

ggplot() +
  gg(StatesBoundary.shp) +
  gg(data = MultiVector_local_pred240$loglambda, aes(color = NULL))+
  geom_tile()+
  scale_fill_viridis_c(name = "log(mean)")+
  gg(EndRange240, color = "purple", lwd = 1, alpha = 0)+
  theme_minimal()+
  coord_sf(xlim = c(-110, -85), ylim = c(15, 30))+
  scale_x_continuous(breaks = seq(-120, -80, by = 20)) +  
  scale_y_continuous(breaks = seq(15, 50, by = 10),)+
  theme(axis.text.x = element_text(size = 25),
        axis.text.y = element_text(size = 25),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(), 
        legend.justification = c(1, 0),
        legend.key.size = unit(1, "cm"),
        legend.text = element_text(size = 20),
        legend.title = element_text(size = 20),
        panel.grid.major = element_line(size = 1.5))

#--------------------Supplemental Figure 1-----------------------------------

#-------------Row 1 Density
rd_vl_spdf_k0.61 <- readRDS("Shapes/Vector_local_RD_k0.61.rds")
rd_mvl_spdf_k0.61 <- readRDS("Shapes/MultiVector_local_RD_k0.61.rds")
rd_tl_spdf_k0.61 <- readRDS("Shapes/True_local_RD_k0.61.rds")



b2 <- ggplot() +
  gg(StatesBoundary.shp) +
  gg(data = Vector_local_pred_k0.61$loglambda, aes(color = NULL))+
  geom_tile()+
  scale_fill_viridis_c(name = "log(mean)")+
  #gg(EndRange, color = "purple", fill = "purple")+
  theme_minimal()+
  scale_x_continuous(breaks = seq(-120, -80, by = 20)) +  
  scale_y_continuous(breaks = seq(15, 50, by = 10))+
  theme(axis.text.x = element_text(size = 25),
        axis.text.y = element_text(size = 25),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position = c(0.95, 0.05),  # Adjust x, y coordinates to position the legend
        legend.justification = c(1, 0),
        legend.key.size = unit(1, "cm"),
        legend.text = element_text(size = 20),
        legend.title = element_text(size = 20),
        panel.grid.major = element_line(size = 1.5))

c2 <- ggplot() +
  gg(StatesBoundary.shp) +
  gg(data = MultiVector_local_pred_k0.61$loglambda, aes(color = NULL))+
  geom_tile()+
  scale_fill_viridis_c(name = "log(mean)")+
  #gg(EndRange, color = "purple", fill = "purple")+
  theme_minimal()+
  scale_x_continuous(breaks = seq(-120, -80, by = 20)) +  
  scale_y_continuous(breaks = seq(15, 50, by = 10))+
  theme(axis.text.x = element_text(size = 25),
        axis.text.y = element_text(size = 25),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position = c(0.95, 0.05),  # Adjust x, y coordinates to position the legend
        legend.justification = c(1, 0),
        legend.key.size = unit(1, "cm"),
        legend.text = element_text(size = 20),
        legend.title = element_text(size = 20),
        panel.grid.major = element_line(size = 1.5))

d2 <- ggplot() +
  gg(StatesBoundary.shp) +
  gg(data = True_local_pred_k0.61$loglambda, aes(color = NULL))+
  geom_tile()+
  scale_fill_viridis_c(name = "log(mean)")+
  #gg(EndRange, color = "purple", fill = "purple")+
  theme_minimal()+
  scale_x_continuous(breaks = seq(-120, -80, by = 20)) +  
  scale_y_continuous(breaks = seq(15, 50, by = 10))+
  theme(axis.text.x = element_text(size = 25),
        axis.text.y = element_text(size = 25),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position = c(0.95, 0.05),  # Adjust x, y coordinates to position the legend
        legend.justification = c(1, 0),
        legend.key.size = unit(1, "cm"),
        legend.text = element_text(size = 20),
        legend.title = element_text(size = 20),
        panel.grid.major = element_line(size = 1.5))

e2 <- ggplot() +
  gg(StatesBoundary.shp) +
  gg(data = Monarch_pred$loglambda, aes(color = NULL))+
  geom_tile()+
  scale_fill_viridis_c(name = "log(mean)")+
  #gg(EndRange, color = "purple", fill = "purple")+
  theme_minimal()+
  scale_x_continuous(breaks = seq(-120, -80, by = 20)) +  
  scale_y_continuous(breaks = seq(15, 50, by = 10))+
  theme(axis.text.x = element_text(size = 25),
        axis.text.y = element_text(size = 25),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position = c(0.95, 0.05),  # Adjust x, y coordinates to position the legend
        legend.justification = c(1, 0),
        legend.key.size = unit(1, "cm"),
        legend.text = element_text(size = 20),
        legend.title = element_text(size = 20),
        panel.grid.major = element_line(size = 1.5))

b2 | c2 | d2 | e2


#-------------Relative Ratio

common_limits <-  c(-10, 10)
colours_rr <- c("cyan", "black", "red")


b3 <- ggplot() +
  gg(rd_vl_spdf_k0.61, aes(color = NULL)) +
  geom_tile()+
  gg(StatesBoundary.shp, alpha = 0) +
  scale_fill_gradientn(limits = common_limits,
                       colours = c("#1f78b4", "#b2b2b2", "#e31a1c"),
                       oob = scales::squish,
                       name = "log(obs/sim)")+
  theme_minimal()+
  scale_x_continuous(breaks = seq(-120, -80, by = 20)) +  
  scale_y_continuous(breaks = seq(15, 50, by = 10))+
  theme(axis.text.x = element_text(size = 25),
        axis.text.y = element_text(size = 25),
        axis.title.x = element_blank(),  # Hide x axis label
        axis.title.y = element_blank(),
        legend.position = c(0.95, 0.05),  # Adjust x, y coordinates to position the legend
        legend.justification = c(1, 0),
        legend.key.size = unit(1, "cm"),
        legend.text = element_text(size = 20),
        legend.title = element_text(size = 20),
        plot.margin = unit(c(-1, -1, 0, 0), "cm"),
        panel.grid.major = element_line(size = 1.5))+
  ggtitle("", subtitle = "")+
  annotate(
    "text", x = -100, y = 13,          # Adjust x, y coordinates for text placement
    label = paste("Value Range:", round(min(rd_vl_spdf$log.difference)), "to", round(max(rd_vl_spdf$log.difference))), # Dynamic label based on data range
    color = "black",                  # Text color
    size = 6)     


c3 <- ggplot() +
  gg(rd_mvl_spdf_k0.61, aes(color = NULL)) +
  geom_tile()+
  gg(StatesBoundary.shp, alpha = 0) +
  scale_fill_gradientn(limits = common_limits,
                       colours = c("#1f78b4", "#b2b2b2", "#e31a1c"),
                       oob = scales::squish,
                       name = "log(obs/sim)")+
  theme_minimal()+
  scale_x_continuous(breaks = seq(-120, -80, by = 20)) +  
  scale_y_continuous(breaks = seq(15, 50, by = 10))+
  theme(axis.text.x = element_text(size = 25),
        axis.text.y = element_text(size = 25),
        axis.title.x = element_blank(),  # Hide x axis label
        axis.title.y = element_blank(),
        legend.position = c(0.95, 0.05),  # Adjust x, y coordinates to position the legend
        legend.justification = c(1, 0),
        legend.key.size = unit(1, "cm"),
        legend.text = element_text(size = 20),
        legend.title = element_text(size = 20),
        plot.margin = unit(c(-1, -1, 0, 0), "cm"),
        panel.grid.major = element_line(size = 1.5))+
  ggtitle("", subtitle = "")+
  annotate(
    "text", x = -100, y = 13,          # Adjust x, y coordinates for text placement
    label = paste("Value Range:", round(min(rd_mvl_spdf$log.difference)), "to", round(max(rd_mvl_spdf$log.difference))), # Dynamic label based on data range
    color = "black",                  # Text color
    size = 6)     

d3 <- ggplot() +
  gg(rd_tl_spdf_k0.61, aes(color = NULL)) +
  geom_tile()+
  gg(StatesBoundary.shp, alpha = 0) +
  scale_fill_gradientn(limits = common_limits,
                       colours = c("#1f78b4", "#b2b2b2", "#e31a1c"),
                       oob = scales::squish,
                       name = "log(obs/sim)")+
  theme_minimal()+
  scale_x_continuous(breaks = seq(-120, -80, by = 20)) +  
  scale_y_continuous(breaks = seq(15, 50, by = 10))+
  theme(axis.text.x = element_text(size = 25),
        axis.text.y = element_text(size = 25),
        axis.title.x = element_blank(),  # Hide x axis label
        axis.title.y = element_blank(),
        legend.position = c(0.95, 0.05),  # Adjust x, y coordinates to position the legend
        legend.justification = c(1, 0),
        legend.key.size = unit(1, "cm"),
        legend.text = element_text(size = 20),
        legend.title = element_text(size = 20),
        plot.margin = unit(c(-1, -1, 0, 0), "cm"),
        panel.grid.major = element_line(size = 1.5))+
  ggtitle("", subtitle = "")+
  annotate(
    "text", x = -100, y = 13,          # Adjust x, y coordinates for text placement
    label = paste("Value Range:", round(min(rd_tl_spdf_k0.61$log.difference)), 
                  "to", round(max(rd_tl_spdf_k0.61$log.difference))), # Dynamic label based on data range
    color = "black",                  # Text color
    size = 6)           



#------------TMAP-------------------

tmap_mode("view")
tm_shape(altitude.sp) +
  tm_raster(midpoint = NA)+
  tm_shape(lv_ends_sf)+
  tm_dots(size = 0.1, col = "red")+
  tm_view(bbox = st_bbox(c(xmin = -96.81250 - 0.18, ymin = 46.23452 - 0.18, xmax = -96.81250 + 0.18, ymax = 46.23452 + 0.18)))



ggplot() +
  gg(cropped_spdf)+
  geom_sf(data = lv_ends_sf, color = "red", size = 2) +
  geom_sf(data = lines_lv, color = "yellow")+
  coord_sf(xlim = c(-96.81250 - 0.30, -96.81250 + 0.30), ylim = c(46.23452 - 0.30, 46.23452 + 0.30)) +
  theme_minimal() +
  scale_fill_viridis_c()
