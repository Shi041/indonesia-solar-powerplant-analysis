setwd("F:/Masters/Term 1/BENV0093 Spatial Analysis of Energy Data/assignments/spatial assignment 2/assignment 2/task 2")

library(sf)
library(spdep) #key function used for conducting spatial autocorrelation
library(dplyr)
library(tmap)
library(terra)
library(extrafont)
library(ggplot2)
library(ggspatial)
library(patchwork)
library(scico)
library(tidyverse)
library(sp)
library(raster)
library(ggmap)
library(RColorBrewer)
library(knitr)    # For knitting document and include_graphics function
library(png) 
library(geojsonsf)
library(foreign)
library(lwgeom)
library(readxl)
library(leaflet) 

quick_mask_raster <- function(raster_data, masking_vector){
  masking_vector <- st_transform(masking_vector, st_crs(raster_data))
  masked_raster_data <- mask(raster_data, masking_vector)
  return(masked_raster_data)}


#general constraints 


# Reading administrative areas
Administrative_areas <- read_sf("F:/Masters/Term 1/BENV0093 Spatial Analysis of Energy Data/assignments/spatial assignment 2/assignment 2/topography/idn_adm_bps/idn_admbnda_adm0_bps_20200401.shp")
Administrative_areas1 <- read_sf("F:/Masters/Term 1/BENV0093 Spatial Analysis of Energy Data/assignments/spatial assignment 2/assignment 2/topography/idn_adm_bps/idn_admbnda_adm1_bps_20200401.shp")

# Reading protected areas
protected_areas0 <- read_sf ("F:/Masters/Term 1/BENV0093 Spatial Analysis of Energy Data/assignments/spatial assignment 2/assignment 2/topography/sensitive natural areas/WDPA_WDOECM_Jan2024_Public_IDN_shp/WDPA_WDOECM_Jan2024_Public_IDN_shp_0/WDPA_WDOECM_Jan2024_Public_IDN_shp-polygons.shp")
protected_areas0_point <- read_sf ("F:/Masters/Term 1/BENV0093 Spatial Analysis of Energy Data/assignments/spatial assignment 2/assignment 2/topography/sensitive natural areas/WDPA_WDOECM_Jan2024_Public_IDN_shp/WDPA_WDOECM_Jan2024_Public_IDN_shp_0/WDPA_WDOECM_Jan2024_Public_IDN_shp-points.shp")
protected_areas1 <- read_sf ("F:/Masters/Term 1/BENV0093 Spatial Analysis of Energy Data/assignments/spatial assignment 2/assignment 2/topography/sensitive natural areas/WDPA_WDOECM_Jan2024_Public_IDN_shp/WDPA_WDOECM_Jan2024_Public_IDN_shp_1/WDPA_WDOECM_Jan2024_Public_IDN_shp-polygons.shp")
protected_areas1_point <- read_sf ("F:/Masters/Term 1/BENV0093 Spatial Analysis of Energy Data/assignments/spatial assignment 2/assignment 2/topography/sensitive natural areas/WDPA_WDOECM_Jan2024_Public_IDN_shp/WDPA_WDOECM_Jan2024_Public_IDN_shp_1/WDPA_WDOECM_Jan2024_Public_IDN_shp-points.shp")
protected_areas2 <- read_sf ("F:/Masters/Term 1/BENV0093 Spatial Analysis of Energy Data/assignments/spatial assignment 2/assignment 2/topography/sensitive natural areas/WDPA_WDOECM_Jan2024_Public_IDN_shp/WDPA_WDOECM_Jan2024_Public_IDN_shp_2/WDPA_WDOECM_Jan2024_Public_IDN_shp-polygons.shp")
protected_areas2_point <- read_sf ("F:/Masters/Term 1/BENV0093 Spatial Analysis of Energy Data/assignments/spatial assignment 2/assignment 2/topography/sensitive natural areas/WDPA_WDOECM_Jan2024_Public_IDN_shp/WDPA_WDOECM_Jan2024_Public_IDN_shp_2/WDPA_WDOECM_Jan2024_Public_IDN_shp-points.shp")


# Reading water bodies and other geographical features
water <- rast("F:/Masters/Term 1/BENV0093 Spatial Analysis of Energy Data/assignments/spatial assignment 2/assignment 2/topography/water/waterIndonesia.tif")
population_areas <- rast("F:/Masters/Term 1/BENV0093 Spatial Analysis of Energy Data/assignments/spatial assignment 2/assignment 2/topography/population areas/idn_bsgme_v0a_100m_2020.tif")
Inland_water	<- read_sf("F:/Masters/Term 1/BENV0093 Spatial Analysis of Energy Data/assignments/spatial assignment 2/assignment 2/topography/IDN_wat/IDN_water_areas_dcw.shp")
Elevation <- raster("F:/Masters/Term 1/BENV0093 Spatial Analysis of Energy Data/assignments/spatial assignment 2/assignment 2/topography/IDN_msk_alt/IDN_msk_alt.gri")
Land_cover	<- raster("F:/Masters/Term 1/BENV0093 Spatial Analysis of Energy Data/assignments/spatial assignment 2/assignment 2/topography/IDN_msk_cov/IDN_msk_cov.gri")
Population <- raster("F:/Masters/Term 1/BENV0093 Spatial Analysis of Energy Data/assignments/spatial assignment 2/assignment 2/topography/IDN_msk_pop/idn_msk_pop.gri")
Gazetteer <- read.dbf("F:/Masters/Term 1/BENV0093 Spatial Analysis of Energy Data/assignments/spatial assignment 2/assignment 2/topography/IDN_gaz/IDN.dbf")
peatland<- read_sf("F:/Masters/Term 1/BENV0093 Spatial Analysis of Energy Data/assignments/spatial assignment 2/assignment 2/topography/peatland/Indonesia_peat_lands/Indonesia_peat_lands.shp")
land_use <-  read_sf("F:/Masters/Term 1/BENV0093 Spatial Analysis of Energy Data/assignments/spatial assignment 2/assignment 2/topography/land use/ind_gc_adg_1.shp")

Roads<- read_sf("F:/Masters/Term 1/BENV0093 Spatial Analysis of Energy Data/assignments/spatial assignment 2/assignment 2/topography/IDN_rds/IDN_roads.shp")
Railroads<- read_sf("F:/Masters/Term 1/BENV0093 Spatial Analysis of Energy Data/assignments/spatial assignment 2/assignment 2/topography/IDN_rrd/IDN_rails.shp")
grid <- read_sf("F:/Masters/Term 1/BENV0093 Spatial Analysis of Energy Data/assignments/spatial assignment 2/assignment 2/topography/grid/grid.geojson")

# Reading global data for Indonesia
global_data<- read_sf("F:/Masters/Term 1/BENV0093 Spatial Analysis of Energy Data/assignments/spatial assignment 2/assignment 2/data/World_Countries_Generalized/World_Countries_Generalized.shp")
indo_map <- filter(global_data, COUNTRY == "Indonesia")



quick_mask_raster <- function(raster_data, masking_vector){
  masking_vector <- st_transform(masking_vector, st_crs(raster_data))
  masked_raster_data <- mask(raster_data, masking_vector)
  return(masked_raster_data)
}


#land cover 



r_matrix<-matrix (c(0 ,10, 0,
                    10,15, 1,
                    15, 16, 2,
                    16, 20, 3,
                    20, 25, 4), ncol=3, byrow= TRUE)
land_potential<- reclassify(Land_cover, r_matrix,include.lowest =TRUE)
plot(land_potential)

#water
Inland_water_buffered <- st_buffer(Inland_water, dist = 2000)

plot(st_geometry(Inland_water))
plot(st_geometry(Inland_water_buffered), add = TRUE, border = 'red')

#peatland
peatland_buffered <- st_buffer(peatland, dist = 2000)

plot(st_geometry(peatland_buffered))
plot(st_geometry(peatland_buffered), add = TRUE, border = 'red')

#population areas 

plot(Population)

p_matrix<-matrix (c(0 ,5000, 0,
                    5000,10000, 1,
                    10000,15000, 2), ncol=3, byrow= TRUE)
Population_reclass<- reclassify(Population, p_matrix,include.lowest =TRUE)
plot(Population_reclass, zlim=c(0, 2))

population_poly <- rasterToPolygons(Population_reclass, fun=function(x){x==1}, dissolve=TRUE)

population_sf <- st_as_sf(population_poly)

population_buffered <- st_buffer(population_sf, dist = 2000)

population_buffered_raster <- raster(extent(Population_reclass), ncol=ncol(Population_reclass), nrow=nrow(Population_reclass))

population_buffered_raster <- rasterize(population_buffered, population_buffered_raster, field=1)


plot(population_buffered_raster,col="black")


#protected areas 
combined_protected_areas <- rbind(protected_areas0, protected_areas1, protected_areas2)
plot(combined_protected_areas$geometry)

protected_areas_buffered <- st_buffer(combined_protected_areas, dist = 2000)


plot(st_geometry(combined_protected_areas))
plot(st_geometry(protected_areas_buffered), add = TRUE, border = 'red')

#roads
Roads_buffered <- st_buffer(Roads, dist = 2000)


plot(st_geometry(Roads))
plot(st_geometry(Roads_buffered), add = TRUE, border = 'red')

#railroad
Railroads_buffered <- st_buffer(Railroads, dist = 2000)


plot(st_geometry(Railroads))
plot(st_geometry(Railroads_buffered), add = TRUE, border = 'red')


#grid 
grid 
grid <- st_transform(grid, st_crs(ssrd_reclass_masked))
grid_buffered <- st_buffer(grid, dist = 5000)

# 
# distances <- st_distance(grid, grid_buffered, by_element = TRUE)
# 
# numeric_distances <- as.numeric(distances)
# 
# grid$index <- ifelse(numeric_distances <= 1000, 3, 
#                      ifelse(numeric_distances <= 5000, 2, 1))
# library(units)
# 
# one_km <- set_units(1000, "m")
# five_km <- set_units(5000, "m")
# 
# grid$index <- ifelse(distances <= one_km, 3, 
#                      ifelse(distances <= five_km, 2, 1))


plot(st_geometry(grid))
plot(st_geometry(grid_buffered), add = TRUE, border = 'red')


#combined constraints
indonesia_map<-Administrative_areas$geometry

ggplot()+
  geom_sf(data = indonesia_map)+

  # Add roads and their buffer
  geom_sf(data = Roads, color = "grey", fill = NA) +
  geom_sf(data = Roads_buffered, color = "orange", fill = NA) +
  
  # Add inland water and its buffer
  geom_sf(data = Inland_water, color = "blue", fill = "blue") +
  geom_sf(data = Inland_water_buffered, color = "cyan", fill = NA) +
  
  # Add peatland and its buffer
  geom_sf(data = peatland_buffered, color = "green", fill = NA) +
  
  # Add population areas (reclassified raster)
   geom_tile(data = population_buffered_raster, aes(x = x, y = y, fill = layer)) +
  
  # Add protected areas and their buffer
  geom_sf(data = combined_protected_areas, color = "yellow", fill = NA) +
  geom_sf(data = protected_areas_buffered, color = "purple", fill = NA) +
  

  
  # Add railroads and their buffer
  geom_sf(data = Railroads, color = "brown", fill = NA) +
  geom_sf(data = Railroads_buffered, color = "pink", fill = NA) +
  
  # Add railroads and their buffer
  geom_sf(data = grid, color = "black", fill = NA) +
  geom_sf(data = grid_buffered, color = "red", fill = NA) +
  
  # Set other plot parameters
  labs(title = "Combined Spatial Data with Buffers") +
  coord_sf() +
  theme_minimal()

geom_tile(data = land_potential_df, aes(x = x, y = y, fill = layer)) +
  scale_fill_viridis_c()


##ssrd 
library(RColorBrewer)
library(readxl)
library(dplyr)

# Read and clean data
power_plant_Indonesia <- read_excel("F:/Masters/Term 1/BENV0093 Spatial Analysis of Energy Data/assignments/spatial assignment 2/assignment 2/data/power plant Indonesia.xlsx")
power_plant_Indonesia_clean <- na.omit(power_plant_Indonesia)

# Convert to sf object
power_plant_Indonesia_sf <- st_as_sf(power_plant_Indonesia_clean, coords = c("longitude", "latitude"), crs = st_crs(4326))
power_plant_Indonesia_sf$status <- tolower(power_plant_Indonesia_sf$status)
power_plant_Indonesia_sf$type <- tolower(power_plant_Indonesia_sf$type)

# Filter for solar plants
solar_plant <- filter(power_plant_Indonesia_sf, type=="solar")

# Load and summarize NetCDF data
library(ncdf4) 
era2022 <- nc_open("F:/Masters/Term 1/BENV0093 Spatial Analysis of Energy Data/assignments/spatial assignment 2/assignment 2/solar nc/2022_4_data.nc" )
summary(era2022)

# Retrieve longitude, latitude, and time
lon <- ncvar_get(era2022, "longitude")
lat <- ncvar_get(era2022, "latitude")
time <- ncvar_get(era2022, "time")

# Extract 'ssrd' variable from NetCDF
ssrd_array2022 <- ncvar_get(era2022,"ssrd") 

# Calculate average SSRD
avg_ssrd_spatial = apply(ssrd_array2022, c(1, 2), mean, na.rm = TRUE)

# Plot average SSRD
image(avg_ssrd_spatial, col=rev(brewer.pal(10,"RdBu")) )

# Prepare data frame from average SSRD
lonlat <- as.matrix(expand.grid(lon, lat)) 
ssrd_vec_avg_year <- as.vector(avg_ssrd_spatial) 
avg_year_ssrd_df <- data.frame(cbind(lonlat, ssrd_vec_avg_year))
colnames(avg_year_ssrd_df) <- c("lon", "lat", "ssrd")
avg_year_ssrd_df_value <- na.omit(avg_year_ssrd_df)

# Convert to sf object
library(sf)
avg_year_ssrd_sf <- st_as_sf(avg_year_ssrd_df_value, coords = c("lon", "lat"), crs = 4326)
st_crs(avg_year_ssrd_sf) <- 4326
avg_year_ssrd_sf <- st_transform(avg_year_ssrd_sf, 4326)

# Visualize with tmap
library(tmap)
tmap_mode("view")
tm_shape(avg_year_ssrd_sf) +
  tm_dots(col="ssrd", style = "quantile", size=.001, palette = "viridis")

# Retrieve units attribute for 'ssrd'
ncatt_get(era2022,"ssrd","units") 

# Assuming avg_year is a matrix or similar structure that can be converted to a raster
ssrd_raster_converted <- raster(avg_year_ssrd_sf)
plot(ssrd_raster_converted)

avg_year_ssrd_spat <- vect(avg_year_ssrd_sf)

# Rasterize the point data
raster_template = rast(resolution=0.1, xmin=95.01079 , ymin=-11.00762  ,xmax=141.01940 , ymax=6.07693  ,  crs = st_crs(Administrative_areas)$wkt)   

ssrd_raster <- rasterize(avg_year_ssrd_spat, raster_template, field = "ssrd", fun = mean)

# Reclassify the raster based on your matrix
reclass_matrix <- matrix(c(0, 10000000, 0,
                           10000000, 12000000, 1,
                           12000000, 14000000, 2,
                           14000000, 16000000, 3,
                           16000000, 18000000, 4,
                           18000000, 20000000, 5,
                           20000000, 30000000, 6), ncol = 3, byrow = TRUE)

ssrd_reclass <- classify(ssrd_raster, reclass_matrix, include.lowest = TRUE)

# Plot the reclassified raster
plot(ssrd_reclass)
plot(Administrative_areas, add=TRUE)



Administrative_areas <- st_transform(Administrative_areas, crs(ssrd_reclass))

ssrd_reclass_masked <- mask(ssrd_reclass, Administrative_areas)
plot(avg_year_ssrd_sf)
plot(ssrd_reclass_masked)

plot(avg_year_ssrd_sf)
plot(Administrative_areas, add = TRUE, border = "black", lwd = 2, col = NA)


ssrd_reclass_masked <- mask(ssrd_reclass, Administrative_areas)

Roads_masked <- st_intersection(Roads, Administrative_areas)

Roads_masked <- mask(Roads, Administrative_areas)
Roads_buffered_masked <- mask(Roads_buffered, Administrative_areas)

Inland_water_masked <- mask(Inland_water, Administrative_areas)
Inland_water_buffered_masked <- mask(Inland_water_buffered, Administrative_areas)

ssrd_reclass_masked <- mask(peatland_buffered, Administrative_areas)
ssrd_reclass_masked <- mask(population_buffered_raster, Administrative_areas)

combined_protected_areas_masked <- mask(combined_protected_areas, Administrative_areas)
protected_areas_buffered_masked <- mask(protected_areas_buffered, Administrative_areas)

Railroads_masked <- mask(Railroads, Administrative_areas)
Railroads_buffered_masked <- mask(Railroads_buffered, Administrative_areas)

grid_masked <- mask(grid, Administrative_areas)
grid_buffered_masked <- mask(grid_buffered, Administrative_areas)

land_potential_masked <- mask(land_potential_df, Administrative_areas)

plot(st_geometry(ssrd_reclass_masked))
plot(st_geometry(grid), add = TRUE,border="black")
plot(st_geometry(grid_buffered), add = TRUE, border = 'purple')

#all combined 

plot(ssrd_reclass_masked)
#plot(Administrative_areas$geometry, add = TRUE, border = "black", lwd = 2, col = NA)



plot(st_geometry(Roads), add = TRUE,)
plot(st_geometry(Roads_buffered), add = TRUE, border = 'red')


plot(st_geometry(Railroads), add = TRUE,)
plot(st_geometry(Railroads_buffered), add = TRUE, border = 'red')



plot(st_geometry(Inland_water), add = TRUE,)
plot(st_geometry(Inland_water_buffered), add = TRUE, border = 'cyan')



plot(st_geometry(peatland_buffered), add = TRUE,)
plot(st_geometry(peatland_buffered), add = TRUE, border = 'cyan')

plot(st_geometry(combined_protected_areas), add = TRUE,)
plot(st_geometry(protected_areas_buffered), add = TRUE, border = 'red')

#plot(land_potential, add = TRUE,)
#plot(population_buffered_raster,col="navy", add = TRUE,)

plot(st_geometry(grid), add = TRUE,border="black")
plot(st_geometry(grid_buffered), add = TRUE, border = 'purple')
plot(Administrative_areas$geometry, add = TRUE, border = "black", lwd = 2, col = NA)






tmap_options(check.and.fix = TRUE)
tmap_mode("view")
tm_shape(avg_year_ssrd_sf) +
  tm_dots(col="ssrd", style = "quantile", size=.001, palette = "RdYlGn")+
  tm_shape(Roads) +
  tm_lines(col = "black") + # Change color as needed
  tm_shape(Roads_buffered) +
  tm_borders(col = 'red') +
  tm_shape(Railroads) +
  tm_lines(col = "black") + # Change color as needed
  tm_shape(Railroads_buffered) +
  tm_borders(col = 'red') +
  tm_shape(Inland_water) +
  tm_dots(col = "cyan") + # Change color as needed
  tm_shape(Inland_water_buffered) +
  tm_dots(col = 'blue') +
  tm_shape(peatland_buffered) +
  tm_dots(col = 'blue') +
  tm_shape(combined_protected_areas) +
  tm_borders(col = 'orange') +
  tm_shape(grid) +
  tm_lines(col = "black") +
  tm_shape(grid_buffered) +
  tm_borders(col = 'purple') 
 # tm_shape(avg_year_ssrd_sf) +
  #tm_dots(col = "ssrd", style = "quantile", size = 0.1, palette = "viridis") # Adjust size as needed








# Transform each spatial object to CRS EPSG:4326
Inland_water_buffered <- st_transform(Inland_water_buffered, crs = 4326)
peatland_buffered <- st_transform(peatland_buffered, crs = 4326)
population_buffered <- st_transform(population_buffered, crs = 4326)
protected_areas_buffered <- st_transform(protected_areas_buffered, crs = 4326)
Roads_buffered <- st_transform(Roads_buffered, crs = 4326)
Railroads_buffered <- st_transform(Railroads_buffered, crs = 4326)
grid_buffered <- st_transform(grid_buffered, crs = 4326)


Inland_water_buffered <- st_simplify(Inland_water_buffered, dTolerance = 10)
peatland_buffered <- st_simplify(peatland_buffered, dTolerance = 10)
population_buffered <- st_simplify(population_buffered, dTolerance = 10)
protected_areas_buffered <- st_simplify(protected_areas_buffered, dTolerance = 0.5)
Roads_buffered <- st_simplify(Roads_buffered, dTolerance = 0.5)
Railroads_buffered <- st_simplify(Railroads_buffered, dTolerance = 0.5)
grid_buffered <- st_simplify(grid_buffered, dTolerance = 0.5)





