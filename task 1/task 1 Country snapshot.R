
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
library(readxl)
library(forecast)


setwd("F:/Masters/Term 1/BENV0093 Spatial Analysis of Energy Data/assignments/spatial assignment 2/assignment 2/task 1")

population <- read_excel("F:/Masters/Term 1/BENV0093 Spatial Analysis of Energy Data/assignments/spatial assignment 2/assignment 2/task 1/data/population.xlsx")
plot(population)

ggplot(data = population, aes(x=year,y=population))+geom_line()+geom_point()+theme_minimal()

consumption_per_capita <- read_excel("F:/Masters/Term 1/BENV0093 Spatial Analysis of Energy Data/assignments/spatial assignment 2/assignment 2/task 1/data/consumption.xlsx")
plot(consumption_per_capita)
ggplot(data = consumption_per_capita, aes(x=year,y=`consumption per capita (kwh)`))+geom_line()+geom_point()+theme_minimal()


population <- population[-63,]
population <- population[-62,]
population <- population[-61,]

year<-population$year
demand <- data.frame(year)
demand$electricity_demands_kWh <- c(consumption_per_capita$`consumption per capita (kwh)`*population$population)

plot(demand$year,demand$electricity_demands_TWh)


# Fitting an ARIMA model to the entire filtered dataset
fit <- auto.arima(demand$electricity_demands_kWh)

# Forecasting from the last year in the dataset to 2030
last_year_in_data <- max(demand$year)
years_to_forecast <- 2030 - last_year_in_data
forecasted_values_2030 <- forecast(fit, h = years_to_forecast)

indo_demands_filtered <- demand
tail(indo_demands_filtered)

# Creating a combined data frame for actual and forecasted values
forecast_years <- seq(last_year_in_data + 1, 2030)
forecasted_data <- data.frame(year = forecast_years, electricity_demands_kWh = forecasted_values_2030$mean)
# Assuming indo_demands_filtered and forecasted_data are already defined

str(indo_demands_filtered)
str(forecasted_data)

combined_data <- bind_rows(indo_demands_filtered, forecasted_data)
tail(combined_data)
plot(combined_data)
data = filter(combined_data, year > last_year_in_data)
data
combined_data
# Plotting the data
ggplot(combined_data, aes(x = year , y = electricity_demands_kWh))+
     geom_line() +
     geom_point(data = data, aes( col="electricity_demands_kWh") ) +
      xlab("Year") + 
      ylab("Electricity Demand") +
      ggtitle("Actual and Forecasted Electricity Demand in Indonesia (1990 - 2030)") +
  theme_minimal()+
  scale_x_continuous( limits=c(1960,2030)) 
                        

forecasted_demand_2030 <- tail(forecasted_values_2030$mean, 1)

print(paste("electricity demand for 2022",(demand$electricity_demands_kWh[60]), "Predicted electricity demand for 2030:", forecasted_demand_2030,"KWh"))






Administrative_areas <- read_sf("F:/Masters/Term 1/BENV0093 Spatial Analysis of Energy Data/assignments/spatial assignment 2/assignment 2/topography/IDN_adm/IDN_adm0.shp")
#plot(Administrative_areas)

Inland_water	<- read_sf("F:/Masters/Term 1/BENV0093 Spatial Analysis of Energy Data/assignments/spatial assignment 2/assignment 2/topography/IDN_wat/IDN_water_areas_dcw.shp")
#plot(Inland_water)

Inland_water_lines	<- read_sf("F:/Masters/Term 1/BENV0093 Spatial Analysis of Energy Data/assignments/spatial assignment 2/assignment 2/topography/IDN_wat/IDN_water_lines_dcw.shp")
#plot(Inland_water_lines$geometry)

Roads<- read_sf("F:/Masters/Term 1/BENV0093 Spatial Analysis of Energy Data/assignments/spatial assignment 2/assignment 2/topography/IDN_rds/IDN_roads.shp")
#plot(Roads)

Railroads<- read_sf("F:/Masters/Term 1/BENV0093 Spatial Analysis of Energy Data/assignments/spatial assignment 2/assignment 2/topography/IDN_rrd/IDN_rails.shp")
#plot(Railroads)

Elevation <- raster("F:/Masters/Term 1/BENV0093 Spatial Analysis of Energy Data/assignments/spatial assignment 2/assignment 2/topography/IDN_msk_alt/IDN_msk_alt.gri")
#plot(Elevation)

Land_cover	<- raster("F:/Masters/Term 1/BENV0093 Spatial Analysis of Energy Data/assignments/spatial assignment 2/assignment 2/topography/IDN_msk_cov/IDN_msk_cov.gri")
#plot(Land_cover)

Population <- raster("F:/Masters/Term 1/BENV0093 Spatial Analysis of Energy Data/assignments/spatial assignment 2/assignment 2/topography/IDN_msk_pop/idn_msk_pop.gri")
#plot(Population)

Gazetteer <- read.dbf("F:/Masters/Term 1/BENV0093 Spatial Analysis of Energy Data/assignments/spatial assignment 2/assignment 2/topography/IDN_gaz/IDN.dbf")
#plot(Gazetteer)

global_data<- read_sf("F://Masters//Term 1//BENV0093 Spatial Analysis of Energy Data//assignments//spatial assignment 2//assignment 2//data//World_Countries_Generalized//World_Countries_Generalized.shp")

indo_map<- filter(global_data,COUNTRY == "Indonesia")
#plot(indo_map)

power_plant_Indonesia<- read_sf("F:/Masters/Term 1/BENV0093 Spatial Analysis of Energy Data/assignments/spatial assignment 2/assignment 2/data/power plant Indonesia.xlsx")
power_plant_Indonesia$status <- tolower(power_plant_Indonesia$status)
power_plant_Indonesia$type <- tolower(power_plant_Indonesia$type)
power_plant_Indonesia<-subset(power_plant_Indonesia, select=-c(Field8))
power_plant_Indonesia_cleaned<-na.omit(power_plant_Indonesia) 
solar<- filter(power_plant_Indonesia_cleaned,type == "solar")

power_plant_Indonesia_sf <- st_as_sf(power_plant_Indonesia_cleaned, coords = c("longitude", "latitude"), crs = st_crs(4326))
solar_sf <- st_as_sf(solar, coords = c("longitude", "latitude"), crs = st_crs(4326))

Administrative_areas_sf <- st_as_sf(Administrative_areas, coords = c("longitude", "latitude"), crs = st_crs(4326))


tmap_mode('view')
tmap_mode('plot')

tm_shape(Administrative_areas) +
  tm_polygons(fill=NA) +
  tm_shape(power_plant_Indonesia_sf) +
  tm_bubbles(col = "type", size=0.05, shape="status") +
  tm_compass(type = "arrow", position = c("left", "top")) + # Add the north arrow here if needed
  tm_scale_bar(position = c("left", "bottom")) + # Add the scale bar here
  tm_layout(
    title = "Power Plants in Indonesia",
    title.position = c("center", "top"),
    legend.position = c("left", "bottom"),
    outer.margins = 0.05
  )

ggplot() +
  geom_sf(data = Administrative_areas) +
  geom_sf(data = power_plant_Indonesia_sf, aes(col = type, shape = status), alpha = 0.5) +
  annotation_scale(location = "br", width_hint = 0.1) + # Add scale bar at bottom left
  annotation_north_arrow(location = "bl", which_north = "true", pad_x = unit(0.05, "in"), pad_y = unit(0.05, "in")) +
  theme_minimal() + # Use a minimal theme
  labs(title = 'Power Plants in Indonesia') +
  theme(
    plot.title = element_text(hjust = 0.5), # Center the title
    legend.position = "right" # Place legend at the bottom
  )


ggplot() +
  geom_sf(data = Administrative_areas) +
  geom_sf(data = solar_sf, aes( color = status,shape=status,size = capacity_mw ), alpha = 0.8) +
  annotation_scale(location = "br", width_hint = 0.1) + # Add scale bar at bottom left
  annotation_north_arrow(location = "bl", which_north = "true", pad_x = unit(0.05, "in"), pad_y = unit(0.05, "in")) +
  theme_minimal() + # Use a minimal theme
  labs(title = 'Solar Plants in Indonesia') +
  theme(
    plot.title = element_text(hjust = 0.5), # Center the title
    legend.position = "right" # Place legend at the bottom
  )
  
selected_location <- read_sf("F:/Masters/Term 1/BENV0093 Spatial Analysis of Energy Data/assignments/spatial assignment 2/filtered_locations6.csv") 
selected_location_sf <- st_as_sf(selected_location, coords = c("x", "y"), crs = st_crs(4326))
selected_location2 <- selected_location[-c(3,4,5),]
selected_location_sf2 <- st_as_sf(selected_location2, coords = c("x", "y"), crs = st_crs(4326))
Gazetteer_sf <- st_as_sf(Gazetteer, coords = c("LONG", "LAT"), crs = st_crs(4326))

selected_location_sf2
ggplot() +
  geom_sf(data = Administrative_areas) +
  geom_sf(data = solar_sf, aes( color = status, shape=status ),size = 3, alpha = 0.8) +
  geom_sf(data = selected_location_sf, color = "purple",aes(color = "purple"), size = 3, alpha = 0.8) +
  annotation_scale(location = "br", width_hint = 0.1) + # Add scale bar at bottom left
  annotation_north_arrow(location = "bl", which_north = "true", pad_x = unit(0.05, "in"), pad_y = unit(0.05, "in")) +
  theme_minimal() + # Use a minimal theme
  labs(title = 'Solar Plants in Indonesia') +
  theme(
    plot.title = element_text(hjust = 0.5), # Center the title
    legend.position = "right" # Place legend at the bottom
  )

ggplot() +
  geom_sf(data = Administrative_areas) +
  geom_sf(data = selected_location_sf, color = "purple", size = 3, alpha = 0.8) +
#  geom_sf_label(data=Gazetteer_sf,col="black",aes(labels=NAME))
  annotation_scale(location = "br", width_hint = 0.1) + # Add scale bar at bottom left
  annotation_north_arrow(location = "bl", which_north = "true", pad_x = unit(0.05, "in"), pad_y = unit(0.05, "in")) +
  theme_minimal() + # Use a minimal theme
  xlim(119,125) + ylim(-14,0)+
  labs(title = 'selected location for Solar Plants in Indonesia') +
  theme(
    plot.title = element_text(hjust = 0.5), # Center the title
    legend.position = "right" # Place legend at the bottom
  )



library(ncdf4) #library to read and process netcdf data
era <- nc_open("2022_4_data.nc" )
era

lon <- ncvar_get(era, "longitude")
lat <- ncvar_get(era, "latitude")
time <- ncvar_get(era, "time")
time

dim(time)

tunits <- ncatt_get(era,"time","units")
tunits

library(chron)


#convert time -- split the time units string into fields
tustr <- strsplit(tunits$value, " ") #strsplit: split the element of character vector. we can convert  "hours since 1900-01-01" to "hours"      "since"      "1900-01-01"
tdstr <- strsplit(unlist(tustr)[3], "-") #convert "1900-01-01" to "1900" "01"   "01"
tyear <- as.integer(unlist(tdstr)[1]) 
tmonth <- as.integer(unlist(tdstr)[2])
tday <- as.integer(unlist(tdstr)[3])

chron(time/24, origin=c(tmonth, tday, tyear) ) #this function is of great help. It can convert the hours since format to the format we are quite familiar with.


ssrd_array <- ncvar_get(era,"ssrd") #get the Surface solar radiation downwards
ssrd_array
dim(ssrd_array)


dlname <- ncatt_get(era,"ssrd","long_name")
dunits <- ncatt_get(era,"ssrd","units")
fillvalue <- ncatt_get(era,"ssrd","_FillValue")

library(lattice)

library(RColorBrewer)


library(gstat)

#AVERAGE SSRD

ssrd_slices <- ssrd_array[,,1:48]

dim(ssrd_slices )
average_slices <- apply(ssrd_slices, c(1, 2), mean, na.rm = TRUE)

overall_average_slice <- apply(average_slices, c(1, 2), mean, na.rm = TRUE)
lonlat <- as.matrix( (expand.grid(lon, lat))) 


ssrd_vec <- as.vector(overall_average_slice)
ssrd_df <- data.frame(cbind(lonlat, ssrd_vec))
colnames(ssrd_df) <- c("lon", "lat", "ssrd")
ssrd_df <- na.omit(ssrd_df)

library(sf)
ssrd_sf <- st_as_sf(ssrd_df, coords = c("lon", "lat"), crs = 4326)
ssrd_sf <- st_transform(ssrd_sf, 4326)
ssrd_sf$ssrd 

radiation_to_power <- function(G, A=12*10^6  , r=0.175, p=0.6, hours=1){
  kWh <- G * A * r * p * (hours/3600) / 1000
  return(kWh)
}

ssrd_df1 <- as.data.frame(radiation_to_power(ssrd_df))
ssrd_df2 <- cbind(ssrd_df, ssrd_df1$ssrd)
head(ssrd_df2)
colnames(ssrd_df2)[4] <- 'generation_kwh'

ssrd_sf1 <- st_as_sf(ssrd_df2, coords = c("lon", "lat"), crs = st_crs(4326))
ssrd_st <- st_transform(ssrd_sf1)
ssrd_st




tmap_mode("view")
tm_shape(ssrd_st)+
  tm_dots(col="generation kwh", style = "quantile", size=.001, palette = "YlOrRd")+
tm_shape(selected_location_sf)+ 
  tm_dots(col = "locations",size =0.1, alpha=0.8)
 # xlim(119,125) + ylim(-14,0)

ssrd_raster <- rasterize(ssrd_sf, raster_template, field = "ssrd", fun = mean)

library(tmap)
tmap_mode("view")

tm_shape(ssrd_sf) +
  tm_dots(col = "ssrd", style = "quantile", size = .001, palette = "YlOrRd") +
  tm_layout(title = "Average SSRD for 2022")

geom_sf(data = selected_location_sf, color = "purple",aes(label = name), size = 3, alpha = 0.8) +
  
  tmap_mode("view")

tm_shape(ssrd_sf)+
  tm_dots(col="ssrd", style = "quantile", size=.001, palette = "YlOrRd")+
  tm_layout(title = "Average SSRD for 2022")+
 #tm_shape(solar_sf)+ 
  #tm_dots(col = "status", palette = c("green", "blue"),size=0.1, alpha=0.8)+
tm_shape(selected_location_sf2)+ 
  tm_dots(col = "purple",size =0.1, alpha=0.8)


## spatial interpolation 
solar_sf = st_transform(ssrd_st, 4326)
indonesia_areas = st_transform(Administrative_areas, st_crs(solar_sf))

coor = as.data.frame(st_coordinates(solar_sf))
solar_sf$x = coor$X
solar_sf$y = coor$Y
solar_nogeom = st_drop_geometry(solar_sf) #get rid of geometry but keep all other attributes
solar_nogeom=na.omit(solar_nogeom)

ssrd_sp <- as(ssrd_st, "Spatial")
library(gstat)
gs <- gstat(formula=ssrd~1, locations=~x+y, data=solar_nogeom, nmax=Inf, set=list(idp=2)) #data should be in data frame format
gs1 <- gstat(formula=generation_kwh~1, locations=~x+y, data=solar_nogeom, nmax=Inf, set=list(idp=2)) #data should be in data frame format

gs
gs1$data

st_bbox(Administrative_areas)
raster_template = rast( resolution = 0.05,
                        xmin=95.01079 , ymin=-11.00762  ,xmax=141.01940 , ymax=6.07693  ,  crs = st_crs(Administrative_areas)$wkt)
raster_template 

idw <- interpolate(raster_template, gs, debug.level=0) #interpolate is the function comes with terra
idw1 <- interpolate(raster_template, gs1, debug.level=0) #interpolate is the function comes with terra

plot(idw$var1.pred)

idw_mask <- mask(idw, indonesia_areas)
plot(idw_mask$var1.pred)

idw_mask1 <- mask(idw1, indonesia_areas)
plot(idw_mask1$var1.pred)

names(idw_mask) = c( "predicted ssrd","observed" )
names(idw_mask1) = c( "predicted generation kwh","observed" )

tm_shape(idw_mask$`predicted ssrd`) + 
  tm_raster(col="predicted ssrd", style = "quantile", n = 10, palette= "RdYlGn", legend.show = TRUE)
tm_shape(idw_mask1$`predicted generation kwh`) + 
  tm_raster(col="predicted generation kwh", style = "quantile", n = 10, palette= "RdYlGn", legend.show = TRUE)



writeRaster(idw_mask$`predicted ssrd`, filename="solar123.tif",overwrite=TRUE)
writeRaster(idw_mask1$`predicted generation kwh`, filename="generation123.tif",overwrite=TRUE)

solar <-rast( "solar123.tif")
gen <-rast( "generation123.tif")

selected_location50 <- read_sf("F:/Masters/Term 1/BENV0093 Spatial Analysis of Energy Data/assignments/spatial assignment 2/book9.csv") 
selected_location_sf50 <- st_as_sf(selected_location50, coords = c("x", "y"), crs = st_crs(4326))

ggplot() +
  geom_sf(data = Administrative_areas) +
  geom_sf(data = solar_sf, aes( color = status, shape=status ),size = 3, alpha = 0.8) +
  geom_sf(data = selected_location_sf50, color = "purple",aes(color = "purple"), size = 1, alpha = 0.8) +
  annotation_scale(location = "br", width_hint = 0.1) + # Add scale bar at bottom left
  annotation_north_arrow(location = "bl", which_north = "true", pad_x = unit(0.05, "in"), pad_y = unit(0.05, "in")) +
  theme_minimal() + # Use a minimal theme
  labs(title = 'Solar Plants and unselected locations in Indonesia') +
  theme(
    plot.title = element_text(hjust = 0.5), # Center the title
    legend.position = "right" # Place legend at the bottom
  )

ggplot() +
  geom_sf(data = Administrative_areas) +
  geom_sf(data = solar_sf, aes( color = status, shape=status ),size = 3, alpha = 0.8) +
  geom_sf(data = selected_location_sf9, color = "purple",aes(color = "purples"), size = 3, alpha = 0.8) +
  annotation_scale(location = "br", width_hint = 0.1) + # Add scale bar at bottom left
  annotation_north_arrow(location = "bl", which_north = "true", pad_x = unit(0.05, "in"), pad_y = unit(0.05, "in")) +
  theme_minimal() + # Use a minimal theme
  labs(title = 'Solar Plants and selected location in Indonesia') +
  theme(
    plot.title = element_text(hjust = 0.5), # Center the title
    legend.position = "right" # Place legend at the bottom
  )

selected_location9 <- read_sf("F:/Masters/Term 1/BENV0093 Spatial Analysis of Energy Data/assignments/spatial assignment 2/book008.csv") 
selected_location_sf9 <- st_as_sf(selected_location9, coords = c("x", "y"), crs = st_crs(4326))

tmap_mode("view")
tm_shape(idw_mask$`predicted ssrd`) + 
  tm_raster(col="predicted ssrd", style = "quantile", n = 10, palette= "RdYlGn", legend.show = TRUE)
tm_shape(selected_location_sf9) + 
  tm_dots(col= "location",palette= "Purples",size = 0.05, legend.show = TRUE)

plot(idw_mask$`predicted ssrd`>16647857)

tmap_mode("view")
tm_shape(idw_mask1$`predicted generation kwh`) + 
  tm_raster(col="predicted generation kwh", style = "quantile", n = 10, palette= "RdYlGn", legend.show = TRUE)+
  tm_shape(selected_location_sf9) + 
  tm_dots(col= "location",palette= "Purples",size = 0.05, legend.show = TRUE)


# 
# RMSE <- function(observed, predicted) {
#   sqrt(mean((predicted - observed)^2, na.rm=TRUE))
# }
# 
# 
# 
# null <- RMSE(mean(solar_sf$ssrd), solar_sf$ssrd)
# null #1.656786 is the baseline. 
# 
# n_idp =2 #examine power ranging from 1 to 20
# n_fold =10
# 
# rmse <- rep(NA, n_fold) #generate 10 NA
# set.seed(7713)
# kf <- sample(1:n_fold, nrow(solar_nogeom), replace=TRUE)
# va = data.frame( c(1:n_idp), NA)
# colnames(va) =c("idp","rmse") 
# 
# 
# 
# for (j in 1:n_idp) 
# {
#   for (i in 1:n_fold) {
#     test <- solar_nogeom[kf == 1, ]
#     train <- solar_nogeom[kf != 1, ]
#     gs <- gstat(formula=ssrd~1, locations=~x+y, data=train, nmax=Inf, set=list(idp=j))
#     pre = predict(gs, test, debug.level=0 )
#     rmse[i] <- RMSE(test$ssrd, pre$var1.pred)
#   }
#   va[j,2] = (mean(rmse) )
# }
# 
# va[which(va$rmse==min(va)),]
# 
# library(ggplot2)
# ggplot(va) +
#   geom_point(aes(x = idp, y= rmse))+
#   geom_hline(yintercept=min(va), linetype="dashed", color = "red")+
#   theme_classic()

constrained <- raster("F:/Masters/Term 1/BENV0093 Spatial Analysis of Energy Data/assignments/spatial assignment 2/New folder/constrained_mask.tif")

plot(constrained$lyr.1!=1)
plot(Administrative_areas_sf$geometry,add=TRUE)
title("combined constrained plot")