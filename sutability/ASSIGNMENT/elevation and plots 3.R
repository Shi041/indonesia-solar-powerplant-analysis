install.packages("rgdal")
library(readr)
library(sf)
library(tmap) 
library(leaflet) 
library(dplyr)
Indonesia_map = st_read("idn_admbnda_adm0_bps_20200401.shp")
library(ggplot2)
install.packages("rgdal")
install.packages("terra")
library(terra)
library(raster)
library(sp)

#ELEVATION INDONESIA
library(terra)
elevation<-rast("D:/SPATIAL DATA ANALYSIS/ASSIGNMENT2/IDN_msk_alt.vrt")
crs(Indonesia_map)==crs(elevation)
Indonesia_map <-st_transform(Indonesia_map, st_crs(elevation))
elevation_mask <- mask(elevation,Indonesia_map) #note the ordering, the second argument is extent we want to crop raster to and the first argument is one you want to crop a bit.
plot(elevation_mask)

slope = terrain(elevation, v='slope', unit='degrees') 
#  reclassify slope data, assign suitable area as 1, and unsuitable as 0.
slope_m<- matrix(c(0, 10, 2,
                   10, 55, 1), ncol=3, byrow=TRUE) 
elevation_rc <- classify(slope, slope_m, include.lowest=TRUE )

plot (elevation_rc)




#quick_mask_raster <- function(raster_data, masking_vector){
 # masking_vector <- st_transform(masking_vector, st_crs(raster_data))
 # masked_raster_data <- mask(raster_data, masking_vector)
 # return(masked_raster_data)}




#TEMPERATURE

Temperature<-rast("D:/SPATIAL DATA ANALYSIS/ASSIGNMENT2/TEMP.tif")
Temperature_mask <- mask(Temperature,Indonesia_map) #note the ordering, the second argument is extent we want to crop raster to and the first argument is one you want to crop a bit.
plot(Temperature_mask)

r_matrix<-matrix (c( 0,  10, 0,
                     10, 20, 1,
                     20, 25, 2,
                     25, 40, 3), ncol=3, byrow= TRUE)   #This code chunk indicates that we want to make cell with value between 10-25 into 1, ans cell with value 25-50 into 2, and so on. 

r_matrix

Temperature_rc <- classify(Temperature, r_matrix, include.lowest=TRUE ) #use predined matrix to guide the reclassification.
plot(Temperature_rc)

#GRID INDONESIA

Indonesia_grid  <- read_sf('grid.geojson')
Indonesia_grid <- st_transform(Indonesia_grid, st_crs(Indonesia_map))
grid_Indonesia <- st_intersection ( Indonesia_grid, Indonesia_map )



library(ggplot2)
ggplot() +
  geom_sf(data=Indonesia_map, fill='antiquewhite') +

  
    geom_sf(data=grid_Indonesia, color='blue') + 
  theme_classic()

  tm_shape(Indonesia_map)+
    tm_fill( col="antiquewhite", alpha=.75)+
    tm_shape(Indonesia_map)+
    tm_borders(lwd=2)+
    tm_shape(grid_Indonesia)+
    tm_lines(col="blue", lwd=3, lty="dashed")
   
  
  
  
  

grid_Indonesia_20km <-st_buffer(grid_Indonesia, 5000) 
plot(grid_Indonesia20km$geometry)




#Roads Indonesia
Indonesia_roads <-st_read('IDN_roads.shp')
Indonesia_map<- st_read("idn_admbnda_adm0_bps_20200401.shp")
Indonesia_roads <- st_transform(Indonesia_roads, st_crs(Indonesia_map))
road_Indonesia <- st_intersection ( Indonesia_roads, Indonesia_map )

library(ggplot2)
ggplot() +
  geom_sf(data=Indonesia_map, fill='grey') +
  geom_sf(data=road_Indonesia, color='orange') + 
      theme_classic()

road_Indonesia_5km <-st_buffer(road_Indonesia, 5000) 
plot(road_Indonesia_5km$geometry)
