setwd("D:/SPATIAL DATA ANALYSIS/ASSIGNMENT2")

getwd()

library(sf)
library(tmap) 
library(leaflet) 
library(dplyr)
library(ggplot2)
install.packages("rgdal")
install.packages("terra")
library(terra)
library(raster)
install.packages("readxl")
library(readxl)



# Read the shape file

Indonesia_map<- st_read("idn_admbnda_adm0_bps_20200401.shp")

library(tmap)
tmap_mode("view")

1.#LAND COVER
landcover<-rast("D:/SPATIAL DATA ANALYSIS/ASSIGNMENT2/IDN_msk_cov.vrt")
quick_mask_raster <- function(raster_data, masking_vector)
 {
  masking_vector <- st_transform(masking_vector, st_crs(raster_data))
masked_raster_data <- mask(raster_data, masking_vector)
return(masked_raster_data)}

raster_template = rast( resolution = 0.05,
                        xmin=95.01079 , ymin=-11.00762  ,xmax=141.01940 , ymax=6.07693  ,  crs = st_crs(Indonesia_map)$wkt)

land_crop<-crop(landcover, raster_template)
land_crop<-resample(land_crop,raster_template)
r_matrix<-matrix (c(0 ,10, 0,
             10,15, 1,
             15, 16, 2,
             16, 20, 3,
             20, 25, 4), ncol=3, byrow= TRUE)
land_potential<-classify(land_crop, r_matrix,include.lowest =TRUE)
land_potential <- quick_mask_raster(land_potential,Indonesia_map)
plot(land_potential)

tmap_mode('plot')
tm_shape(land_potential)+tm_raster(style='cat',alpha = 0.7)+
  tm_shape(Indonesia_map)+tm_borders()+
  tm_compass(position=c('0.9','0.8'),size =0.8)+
  tm_scale_bar(position=c('0.','0.05'), width =0.2)+
  tm_layout(legend.position=c('0.003','0.03'),legend.text.size=0.6,legend.title.size=1)
  tm_layout(main.title="Landcover_potential")
             
  



2.#Peat land
peatland <-st_read("D:/SPATIAL DATA ANALYSIS/ASSIGNMENT2/Indonesia_peat_lands.shp")
raster_template = rast( resolution = 0.05,
                        xmin=95.01079 , ymin=-11.00762  ,xmax=141.01940 , ymax=6.07693  ,  crs = st_crs(Indonesia_map)$wkt)
peatland_raster <- rasterize(peatland, raster_template)
plot(peatland_raster)

tmap_mode('plot')
tm_shape(peatland_raster)+tm_raster(palette= 'darkgreen')+
  tm_shape(Indonesia_map)+tm_borders()+
  tm_compass(position=c('0.9','0.8'),size =0.8)+
  tm_scale_bar(position=c('0.','0.05'), width =0.2)+
  tm_layout(legend.position=c('0.003','0.03'),legend.text.size=0.6,legend.title.size=1)
  tm_layout(main.title="Peatland")




#Peatland method2
peatland <-st_read("D:/SPATIAL DATA ANALYSIS/ASSIGNMENT2/Indonesia_peat_lands.shp")
raster_template = rast( resolution = 0.05,
                        xmin=95.01079 , ymin=-11.00762  ,xmax=141.01940 , ymax=6.07693  ,  crs = st_crs(Indonesia_map)$wkt)
peatland_raster <- rasterize(peatland, raster_template)
quick_mask_raster <- function(raster_data, masking_vector)
{
  masking_vector <- st_transform(masking_vector, st_crs(raster_data))
  masked_raster_data <- mask(raster_data, masking_vector)
  return(masked_raster_data)}

peatland_crop<-crop(peatland_raster, raster_template)
peatland_crop<-resample(peatland_crop,raster_template)
plot(peatland_crop)
r_matrix<-matrix (c(0 ,2000000000, 0,
                    2000000000,4000000000, 1,
                    4000000000,6000000000, 2,
                    6000000000, 8000000000, 3,
                    8000000000, 10000000000, 4,
                    10000000000,15000000000, 5), ncol=3, byrow= TRUE)
peatland_potential<-classify(peatland_crop, r_matrix,include.lowest =TRUE)
peatland_potential <- quick_mask_raster(peatland_potential,Indonesia_map)
plot(peatland_potential)

3.#Population

population<-rast("D:/SPATIAL DATA ANALYSIS/ASSIGNMENT2/IDN_msk_pop.vrt")
quick_mask_raster <- function(raster_data, masking_vector)
{
  masking_vector <- st_transform(masking_vector, st_crs(raster_data))
  masked_raster_data <- mask(raster_data, masking_vector)
  return(masked_raster_data)}

raster_template = rast( resolution = 0.05,
                        xmin=95.01079 , ymin=-11.00762  ,xmax=141.01940 , ymax=6.07693  ,  crs = st_crs(Indonesia_map)$wkt)

pop_crop<-crop(population, raster_template)
pop_crop<-resample(pop_crop,raster_template)
plot(pop_crop)
r_matrix<-matrix (c(0 ,10, 0,
                    10,15, 1,
                    15, 20, 2,
                    20, 25, 3,
                    25, 30, 4), ncol=3, byrow= TRUE)
pop_raster<-classify(pop_crop, r_matrix,include.lowest =TRUE)
pop_raster <- quick_mask_raster(pop_raster,Indonesia_map)
plot(pop_raster)

tmap_mode('plot')
tm_shape(pop_raster)+tm_raster(style='pretty',alpha = 0.7)+
  tm_shape(Indonesia_map)+tm_borders()+
  tm_compass(position=c('0.9','0.8'),size =0.8)+
  tm_scale_bar(position=c('0.7','0.05'), width =0.2)+
  tm_layout(legend.position=c('0.003','0.03'),legend.text.size=0.6,legend.title.size=1)
  tm_layout(main.title='Settlement')


4.#Protected
protected <-st_read("D:/SPATIAL DATA ANALYSIS/ASSIGNMENT2/WDPA_WDOECM_Jan2024_Public_IDN_shp-polygons.shp")
raster_template = rast( resolution = 0.05,
                        xmin=95.01079 , ymin=-11.00762  ,xmax=141.01940 , ymax=6.07693  ,  crs = st_crs(Indonesia_map)$wkt)
protected_raster <- rasterize(protected, raster_template)
plot(protected_raster)

tmap_mode('plot')
tm_shape(pop_raster)+tm_raster(palette='darkgreen')+
  tm_shape(Indonesia_map)+tm_borders()+
  tm_compass(position=c('0.9','0.8'),size =0.8)+
  tm_scale_bar(position=c('0.','0.05'), width =0.2)+
  tm_layout(legend.position=c('0.003','0.03'),legend.text.size=0.6,legend.title.size=1)
  tm_layout(main.title='Protected Land')

#Constrained plot


constrained_map <- c(protected_raster, pop_raster, land_potential, peatland_potential)

names(constrained_map)=c("protected_raster","pop_raster","land_potential","peatland_potential")


constrained = raster_template = rast( resolution = 0.05,
                                      xmin=95.01079 , ymin=-11.00762  ,xmax=141.01940 , ymax=6.07693  ,  crs = st_crs(Indonesia_map)$wkt)


constrained_map_df=as.data.frame(constrained_map, XY=TRUE)
id= which(constrained_map_df$ 'protected_raster'==1|
            constrained_map_df$ 'pop_raster'==0|
            constrained_map_df$ 'land_potential'==0 |
            constrained_map_df$ 'land_potential'==2 |
            constrained_map_df$ 'peatland_potential'==1)
constrained[id]=1
constrained_mask<-quick_mask_raster(constrained, Indonesia_map)

# Replace NA with 0 to indicate non-constrained areas
constrained[is.na(constrained)] <- 0

# Plot the constrained raster
plot(constrained_mask)

tmap_mode('plot')
tm_shape(constrained_mask)+tm_raster(style='cat',alpha = 0.7)+
  tm_shape(Indonesia_map)+tm_borders()+
  tm_compass(position=c('0.9','0.8'),size =0.8)+
  tm_scale_bar(position=c('0.','0.05'), width =0.2)+
  tm_layout(legend.position=c('0.003','0.03'),legend.text.size=0.6,legend.title.size=1)
  tm_layout(main.title='Constrained Land Plot')

