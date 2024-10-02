---
title: 'Agent based Model: Eastern Monarch Migration'
---

#Load Libraries: Required----
library(raster)
library(sf)
library(circular)
library(parallel)
library(doParallel)
library(foreach)
library(CircStats)
library(BBmisc)
library(terra)
library(tidyverse)

#Optional
library(tictoc)
library(beepr)



#Directories----
setwd("")

#Set up environment----
landscape <- raster("Covariates/AltitudeRaster.tif")
StartingRange <- read_sf("Shapes/StartingRangeFull.shp")

#Import Specific overwintering location
EndingPoints <- read_sf("Shapes/OverwinterPoint.shp")

#Change to corresponding buffer
EndRange <-  read_sf("Shapes/OverwinterBufferTrueNav.shp")


#Simulation---------

#Parameters

#Standard deviation and Variance of Pied flycatcher orientations 
#assuming normal distribution
sd_rad <- 0.9033
var_rad <- sd_rad^2

#Converting variance to Concentration Parameter for circular dist. used in sim
kappa_rad <- 1/var_rad

#Half the Kappa to lower accuracy
#kappa_rad <- kappa_rad/2
#var_rad <- 1/kappa_rad

#Calculate max steps
max_steps <- 4500/(exp(-0.5*var_rad))

#Historic mean orientation of monarch butterflies
orientation_angle <- 225

#Setting kappa for simulation
kappa <- kappa_rad


#mean number of days
mean_days <- 60

#Mean number of steps in a day
mean_steps <- max_steps/mean_days

#Set range it takes to consider goal reached
goal.reached <- 1000

#Set how many agents you want to record
num.agents <- 1000

#Set Random seed
seed_list <- round(runif(num.agents, min = 0, max = 1), 7)


#Number of observations in our distributions
n <- 1000

#Number of Steps per day distribution (S)
#(if the number of steps per day is low, be careful for negative distances)
dist_walk_distance <- rnorm(n, mean = mean_steps, sd = 3)
#Round to whole number steps. 
dist_walk_distance <- floor(dist_walk_distance)

#Total time distribution (T)
num_day_stop_migration <- rnorm(n, mean = mean_days, sd = 3)
num_day_stop_migration <- floor(num_day_stop_migration)

#Circular distribution for all possible orientations
circle.df <- seq(0, 360, 0.1) 
circle.df2 <- circular(circle.df, units = c("degrees"), 
                       modulo = c("2pi"),
                       template = ("geographics"))


#Creates Empty Matrix for which direction agents move in
vector_walk <- matrix(0, ncol = 1, nrow = 1)

#Creates Empty Matrix used to sample dist_walk_distance to get the actual distance an agent will move.
walk_distance<- matrix(0, ncol = 1, nrow = 1)

#Creates Empty Matrix used to sample num_day_stop_migration to get the temporal length of migration
stop_migration <- matrix(0, ncol = 1, nrow = 1)



#All vector Agent Matrix and Data frames. Used based on whihch format is needed.
all.vector.agents <-  matrix(ncol = 7, nrow = 0)
all.vector.matrix <- matrix(ncol = 7, nrow = 0)
grid_walk_matrix <- matrix(ncol=3, nrow = 0)


#Sampling Distribution function
dis.fun <- function(n) sample(circle.df,
                              n, # n draws            
                              TRUE,  # with replaceme
                              circ.dis[,2] # using t
)


#invert landscape effects
inverted_normalize <- function(r, new_min = 0.016, new_max = 2) {
  
  r[is.na(r)] <- na.value
  
  # Get the minimum and maximum values of the raster
  old_min <- min(r)
  old_max <- max(r)
  
  # Normalize the raster values
  normalized <- new_min + (((r - old_min) * (new_max - new_min)) / (old_max - old_min))
  
  if (any(is.nan(normalized))) {
    inverted <- rep(1, length(r))
  } else {
    # Invert
    inverted <- (new_max+new_min) - normalized
  }
  
  return(inverted)
}

#Vector used to record which Agent is run on which core during loop
current.para <- c(1:num.agents)
#NA Value needs to be large so it is always the most unlikely cell value
na.value <- 7000

#Random seed for random start locations
set.seed(seed_list[1])
random_start_list <- st_coordinates(st_sample(StartingRange, size=num.agents))

#-------------Full SIMULATION-----------
tic("")
print("")

#Detect and assign cores!# lower n.cores for stability
parallel::detectCores()
n.cores <- parallel::detectCores() - 1
#create the cluster #PSOCK for window; FORK for MAC
my.cluster <- parallel::makeCluster(
  n.cores, 
  type = "FORK"
)

#check cluster definition (optional)
print(my.cluster)

#register it to be used by %dopar%
doParallel::registerDoParallel(cl = my.cluster)

#check if it is registered (optional)
foreach::getDoParRegistered()

#how many workers are available? (optional)
foreach::getDoParWorkers()

#Loop below#

#Set Current Agent to 0 to start the loop
current.agent <- 0


output <- foreach(j = 1:num.agents, .combine=rbind,
                  .packages = c('circular', 'CircStats', 'raster', 'BBmisc', 'sf', 'terra')) %dopar% {
                    set.seed(seed_list[j])
                    ###Reseting Values for New Agent###
                    stop_migration[1,1] <- sample(num_day_stop_migration,1)
                    nd <- 0 #Sets day to 0
                    agent.matrix <- matrix(ncol=6, nrow=0) #Clears agent.matrix data from previous agent
                    
                    #Setting Random start location
                    random_start <- matrix(random_start_list[j,], ncol=2)
                    location_cell <- cellFromXY(landscape, random_start)
                    location.cord <- xyFromCell(landscape, location_cell)
                    start <- rowColFromCell(landscape, location_cell)
                    location <- start
                    # Resets and sets the first step to starting location
                    all.step.matrix <- matrix(c(0, location[1], location[2], location.cord[1], location.cord[2]), nrow = 1, ncol = 5)
                    
                    #### Loop to generate a single agent####
                    repeat{
                      
                      walk_distance[1,1] <- sample(dist_walk_distance, 1)#Sample "distance walk distribution for how many steps a day#
                      
                      ns <- 0 #Sets number of steps to zero to start day
                      
                      grid_walk <- location
                      
                      #Determine distance from nearest overwintering location
                      location_sf <- st_sfc(st_point(location.cord)) %>% st_set_crs(4326)
                      dist.away.from.end <- min(as.data.frame(st_distance(location_sf, EndingPoints)))
                      
                      #Determine if Agents are in the home in range
                      is_within <- st_within(st_point(location.cord), EndRange)
                      
                      all.step.matrix <- matrix(c(0, location[1], location[2], location.cord[1], location.cord[2]), nrow = 1, ncol = 5)
                      
                      
                      #Normal Vector Day
                      if(length(unlist(is_within[[1]])) == 0){
                        repeat{
                          
                          # Converts the slope into degrees using the arch tangential equation             
                          degs <-  orientation_angle
                          
                          #Converts "degs" into a circular variable with north at zero         
                          mu <-  circular(degs, units = c("degrees"), 
                                          modulo = c("2pi"),
                                          template = ("geographics"))
                          
                          #Creates the orientation distribution and samples from it
                          circ.dis <- data.frame(Angle= circle.df,
                                                 Density = dvonmises(circle.df2,
                                                                     mu,
                                                                     kappa,
                                                                     log = F))
                          
                          
                          #find landscape conditions based on location           
                          land.loc <- floor(location)  
                          
                          row <- c(land.loc[1]-1, land.loc[1]-1, land.loc[1], land.loc[1] + 1,
                                   land.loc[1] + 1, land.loc[1]+1, land.loc[1], land.loc[1]-1)
                          
                          col <- c(land.loc[2], land.loc[2]+1, land.loc[2]+1, land.loc[2]+1,
                                   land.loc[2], land.loc[2]-1, land.loc[2]-1, land.loc[2]-1)
                          
                          
                          #Modify Distribution by taking values from landscape
                          matrix.mod <- landscape[cbind(row,col)]   
                          
                          #Allows navigators to traverse landscape with no data      
                          
                          
                          matrix.mod <- inverted_normalize(matrix.mod)
                          
                          
                          #Modify distribution based on squares (square numbering starts at top center and goes clockwise#              
                          #Square 1               
                          circ.dis[circ.dis$Angle<=22.5, 2] <- circ.dis[circ.dis$Angle<=22.5,2]*matrix.mod[1]
                          circ.dis[circ.dis$Angle>=337.5, 2] <- circ.dis[circ.dis$Angle>=337.5,2]*matrix.mod[1]               
                          #Square 2            
                          circ.dis[circ.dis$Angle>22.5&circ.dis$Angle<67.5, 2]<- circ.dis[circ.dis$Angle>22.5&circ.dis$Angle<67.5, 2]*matrix.mod[2]           
                          #Square 3            
                          circ.dis[circ.dis$Angle>=67.5&circ.dis$Angle<=112.5, 2] <- circ.dis[circ.dis$Angle>=67.5&circ.dis$Angle<=112.5, 2]*matrix.mod[3]          
                          #Square 4                
                          circ.dis[circ.dis$Angle>112.5&circ.dis$Angle<157.5, 2] <- circ.dis[circ.dis$Angle>112.5&circ.dis$Angle<157.5, 2]*matrix.mod[4]         
                          #Square 5         
                          circ.dis[circ.dis$Angle>=157.5&circ.dis$Angle<=202.5, 2] <- circ.dis[circ.dis$Angle>=157.5&circ.dis$Angle<=202.5, 2]*matrix.mod[5]    
                          #square 6
                          circ.dis[circ.dis$Angle>202.5&circ.dis$Angle<247.5, 2] <- circ.dis[circ.dis$Angle>202.5&circ.dis$Angle<247.5, 2]*matrix.mod[6]     
                          #Square 7         
                          circ.dis[circ.dis$Angle>=247.5&circ.dis$Angle<=292.5, 2] <- circ.dis[circ.dis$Angle>=247.5&circ.dis$Angle<=292.5, 2]*matrix.mod[7]         
                          #Square 8           
                          circ.dis[circ.dis$Angle>292.5&circ.dis$Angle<337.5, 2] <- circ.dis[circ.dis$Angle>292.5&circ.dis$Angle<337.5, 2]*matrix.mod[8]
                          
                          
                          #Sample from modified distribution by converting distrubtion into a function
                          dis.fun <- function(n) sample(circle.df,                
                                                        n, # n draws                 
                                                        TRUE,  # with replacement
                                                        circ.dis[,2] # using these probabilities
                          )    
                          #Sample function        
                          vector_walk <- dis.fun(1)
                          
                          ###based on orientation selected, moves the agent to the correct square                 
                          if(vector_walk>=337.5){
                            grid_walk <- rbind(grid_walk, c(-1,0))
                          }else if(vector_walk<=22.5){
                            grid_walk <- rbind(grid_walk, c(-1,0))
                          }else if(vector_walk>22.5&vector_walk<67.5){
                            grid_walk <- rbind(grid_walk, c(-1,1))
                          } else if(vector_walk>=67.5&vector_walk<=112.5){
                            grid_walk <- rbind(grid_walk, c(0,1))
                          } else if(vector_walk>112.5&vector_walk<157.5){
                            grid_walk <- rbind(grid_walk, c(1,1))
                          } else if(vector_walk>=157.5&vector_walk<=202.5){
                            grid_walk <- rbind(grid_walk, c(1,0))
                          } else if(vector_walk>202.5&vector_walk<247.5){
                            grid_walk <- rbind(grid_walk, c(1,-1))
                          } else if(vector_walk>=247.5&vector_walk<=292.5){
                            grid_walk <- rbind(grid_walk, c(0,-1))
                          } else{
                            grid_walk <- rbind(grid_walk, c(-1,-1))}
                          
                          #record 1 single step based on the orientation chosen       
                          i <- length(grid_walk[,1])                
                          df.sum<- colSums(tail(grid_walk,n=2))                 
                          grid_walk[i, 1:2] <- df.sum[1:2]               
                          
                          #Setting "ocation" to the current position of the agent           
                          location <- tail(grid_walk, n=1)
                          
                          location_cell <- cellFromRowCol(landscape, location[1,1], location[1,2])
                          location.cord <- xyFromCell(landscape, location_cell)
                          is_within <- st_within(st_point(location.cord), EndRange)
                          
                          #End the Day loop based on number of walks that day##             
                          ns <- ns + 1
                          step.matrix<- cbind(ns, df.sum[1], df.sum[2],location.cord[1], location.cord[2])
                          all.step.matrix <- rbind(all.step.matrix, step.matrix)
                          final.step.location <- tail(all.step.matrix[,2:3],1)
                          
                          if(ns == walk_distance | length(is_within[[1]]) == 1){break}
                        }}else{
                          repeat{
                            
                            location_sf <- st_sfc(st_point(location.cord)) %>% st_set_crs(4326)
                            dist.points<- as.matrix(st_distance(location_sf, EndingPoints))
                            min.point<- min(dist.points)
                            home.overwinter <- which(dist.points==min.point)
                            chosen.overwinter <- st_coordinates(EndingPoints[home.overwinter,2])
                            
                            
                            location_cell <- cellFromXY(landscape, location.cord)
                            
                            location<- rowColFromCell(landscape, location_cell)
                            
                            x <- chosen.overwinter[1,1]- location.cord[1,1]
                            y <- chosen.overwinter[1,2]- location.cord[1,2]

                            # Converts the slope into degrees using the arch tangential equation                  
                            degs <- ((180/pi)*atan2(x,y))+360 #Set at zero for Compass navigation
                            
                            #Converts "degs" into a circular variable with north at zero                 
                            mu <-  circular(degs, units = c("degrees"),                 
                                            modulo = c("2pi"),               
                                            template = ("geographics"))
                            #Creates the orientation distribution and samples from it
                            circ.dis <- data.frame(Angle= circle.df,
                                                   Density = dvonmises(circle.df2,                  
                                                                       mu,                 
                                                                       kappa,                 
                                                                       log = F))
                            #find landscape conditions based on location              
                            land.loc <- floor(location) 
                            row <- c(land.loc[1]-1, land.loc[1]-1, land.loc[1], land.loc[1] + 1,                  
                                     land.loc[1] + 1, land.loc[1]+1, land.loc[1], land.loc[1]-1)
                            
                            col <- c(land.loc[2], land.loc[2]+1, land.loc[2]+1, land.loc[2]+1,                  
                                     land.loc[2], land.loc[2]-1, land.loc[2]-1, land.loc[2]-1)
                            
                            
                            #Modify Distribution by taking values from landscape                 
                            matrix.mod <- landscape[cbind(row,col)] 
                            
                            #Allows navigators to traverse landscape with no data            
                            matrix.mod <- inverted_normalize(matrix.mod)
                            
                            #Modify distribution based on squares (square numbering starts at top center and goes clockwise#              
                            #Square 1               
                            circ.dis[circ.dis$Angle<=22.5, 2] <- circ.dis[circ.dis$Angle<=22.5,2]*matrix.mod[1]
                            circ.dis[circ.dis$Angle>=337.5, 2] <- circ.dis[circ.dis$Angle>=337.5,2]*matrix.mod[1]               
                            #Square 2            
                            circ.dis[circ.dis$Angle>22.5&circ.dis$Angle<67.5, 2]<- circ.dis[circ.dis$Angle>22.5&circ.dis$Angle<67.5, 2]*matrix.mod[2]           
                            #Square 3            
                            circ.dis[circ.dis$Angle>=67.5&circ.dis$Angle<=112.5, 2] <- circ.dis[circ.dis$Angle>=67.5&circ.dis$Angle<=112.5, 2]*matrix.mod[3]          
                            #Square 4                
                            circ.dis[circ.dis$Angle>112.5&circ.dis$Angle<157.5, 2] <- circ.dis[circ.dis$Angle>112.5&circ.dis$Angle<157.5, 2]*matrix.mod[4]         
                            #Square 5         
                            circ.dis[circ.dis$Angle>=157.5&circ.dis$Angle<=202.5, 2] <- circ.dis[circ.dis$Angle>=157.5&circ.dis$Angle<=202.5, 2]*matrix.mod[5]    
                            #square 6
                            circ.dis[circ.dis$Angle>202.5&circ.dis$Angle<247.5, 2] <- circ.dis[circ.dis$Angle>202.5&circ.dis$Angle<247.5, 2]*matrix.mod[6]     
                            #Square 7         
                            circ.dis[circ.dis$Angle>=247.5&circ.dis$Angle<=292.5, 2] <- circ.dis[circ.dis$Angle>=247.5&circ.dis$Angle<=292.5, 2]*matrix.mod[7]         
                            #Square 8           
                            circ.dis[circ.dis$Angle>292.5&circ.dis$Angle<337.5, 2] <- circ.dis[circ.dis$Angle>292.5&circ.dis$Angle<337.5, 2]*matrix.mod[8]
                            
                            #Sample from modified distribution by converting distrubtion into a function
                            
                            dis.fun <- function(n) sample(circle.df,
                                                          n, # n draws             
                                                          TRUE,  # with replacement     
                                                          circ.dis[,2] # using these probabilities
                            )
                            
                            
                            #Sample function         
                            vector_walk <- dis.fun(1)
                            
                            
                            ###based on orientation selected, moves the agent to the correct square               
                            if(vector_walk>=337.5){
                              grid_walk <- rbind(grid_walk, c(-1,0))
                            }else if(vector_walk<=22.5){
                              grid_walk <- rbind(grid_walk, c(-1,0))
                            }else if(vector_walk>22.5&vector_walk<67.5){
                              grid_walk <- rbind(grid_walk, c(-1,1))
                            } else if(vector_walk>=67.5&vector_walk<=112.5){
                              grid_walk <- rbind(grid_walk, c(0,1))
                            } else if(vector_walk>112.5&vector_walk<157.5){
                              grid_walk <- rbind(grid_walk, c(1,1))
                            } else if(vector_walk>=157.5&vector_walk<=202.5){
                              grid_walk <- rbind(grid_walk, c(1,0))
                            } else if(vector_walk>202.5&vector_walk<247.5){
                              grid_walk <- rbind(grid_walk, c(1,-1))
                            } else if(vector_walk>=247.5&vector_walk<=292.5){
                              grid_walk <- rbind(grid_walk, c(0,-1))
                            } else{
                              grid_walk <- rbind(grid_walk, c(-1,-1))}
                            
                            
                            #record 1 single step based on the orientation chosen               
                            i <- length(grid_walk[,1])        
                            df.sum<- colSums(tail(grid_walk,n=2))            
                            grid_walk[i, 1:2] <- df.sum[1:2]
                            
                            #Setting "start" to the current position of the agent
                            location <- tail(grid_walk, n=1)
                            
                            location_cell <- cellFromRowCol(landscape, location[1,1], location[1,2])
                            location.cord <- xyFromCell(landscape, location_cell)
                            
                            
                            is_within <- st_within(st_point(location.cord), EndRange)
                            
                            #End the Day loop based on number of walks that day##  
                            ns <- ns + 1   
                            step.matrix<- cbind(ns, df.sum[1], df.sum[2], location.cord[1], location.cord[2])      
                            all.step.matrix <- rbind(all.step.matrix, step.matrix)
                            final.step.location <- tail(all.step.matrix[,2:3],1)
                            
                            location_sf <- st_sfc(st_point(location.cord)) %>% st_set_crs(4326)
                            dist.away.from.end<- min(as.data.frame(st_distance(location_sf, EndingPoints)))
                            
                            
                            if(ns == walk_distance | dist.away.from.end < goal.reached){break}}
                          
                        }
                      
                      #Create Matrix to store steps for each day of the agents movement
                      
                      nd <- nd+1                
                      day.matrix <- matrix(data = c(rep(nd,length(all.step.matrix[,1])), all.step.matrix), ncol = 6, nrow = length(all.step.matrix[,1]))             
                      agent.matrix <- rbind(agent.matrix, day.matrix)
                      
                      
                      #End the Agent Loop based on number of days moved###               
                      
                      if(nd == stop_migration | dist.away.from.end < goal.reached){break}
                      
                    }
                    
                    
                    current.agent <- current.agent + 1

                    all.vector.matrix <- cbind(agent.matrix, c(current.para[j]))
                    
                    
                    all.vector.matrix

                  }



parallel::stopCluster(cl = my.cluster)
colnames(output) <- c("Day", "Step", "Row", "Column", "X", "Y", "Agent")
toc()
print("DONE!")

#Prepare Output for Export
df_output <- as.data.frame(output)
beep(3)

#------Save Data----------
write.csv(df_output, ".csv", row.names = F)


#
grouped_data <- df_output %>% group_by(Agent)

# Select the final row for each agent
final_rows <- grouped_data %>% slice_tail(n = 1)

# Create a new data frame with the selected final rows
Final_location <- data.frame(final_rows)

write.csv(Final_location, ".csv")
