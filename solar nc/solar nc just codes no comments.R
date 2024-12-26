
setwd("F:/Masters/Term 1/BENV0093 Spatial Analysis of Energy Data/assignments/spatial assignment 2/assignment 2/solar nc")
library(RColorBrewer)

library(ncdf4) 
era2022 <- nc_open("2022_48data.nc" )
era2022 
summary(era2022)

lon <- ncvar_get(era2022, "longitude")
lat <- ncvar_get(era2022, "latitude")
time <- ncvar_get(era2022, "time")
time

dim(time) 

tunits <- ncatt_get(era2022,"time","units") 

(time[5]%%(24*365) ) %% 24

library(chron) 

ssrd_array2022 <- ncvar_get(era2022,"ssrd") 
dim(ssrd_array2022) 

ssrd_array2022
dim(ssrd_array2022 )
ssrd_array2022[,,48]

ssrd_slice2022 <- ssrd_array2022[,,48] 

length(na.omit(as.vector(ssrd_slice2022))) /length(as.vector(ssrd_slice2022)) 
dim(ssrd_slice2022 )

image(ssrd_slice2022, col=rev(brewer.pal(10,"RdBu")) )
image(ssrd_array2022[,,7] , col=rev(brewer.pal(10,"RdBu")) )

avg_ssrd_spatial = apply(ssrd_array2022, c(1, 2), mean, na.rm = TRUE)
image(avg_ssrd_spatial, col=rev(brewer.pal(10,"RdBu")) )


max_rad <- max(ssrd_slice2022, na.rm=TRUE)
max_rad


# Winter: January, February, December
winter_months = c(1, 2, 3, 4, 5, 6, 7, 8, 45, 46, 47, 48)

# Spring: March, April, May
spring_months = c(9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20)

# Summer: June, July, August
summer_months = c(21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32)

# Autumn: September, October, November
autumn_months = c(33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44)


# Extract data for each season
spring_data = ssrd_array2022[,,spring_months]
summer_data = ssrd_array2022[,,summer_months]
autumn_data = ssrd_array2022[,,autumn_months]
winter_data = ssrd_array2022[,,winter_months]

dim(spring_data)
dim(summer_data)
dim(autumn_data)
dim(winter_data)



# Calculate average for each season
avg_spring = apply(spring_data, c(1, 2), mean, na.rm = TRUE)
avg_summer = apply(summer_data, c(1, 2), mean, na.rm = TRUE)
avg_autumn = apply(autumn_data, c(1, 2), mean, na.rm = TRUE)
avg_winter = apply(winter_data, c(1, 2), mean, na.rm = TRUE)

image(avg_spring, col=rev(brewer.pal(10,"RdBu")) )
image(avg_summer, col=rev(brewer.pal(10,"RdBu")) )
image(avg_autumn, col=rev(brewer.pal(10,"RdBu")) )
image(avg_winter, col=rev(brewer.pal(10,"RdBu")) )







#example: check one max point
avg_spring_max_rad <- max(avg_spring, na.rm=TRUE)
avg_summer_max_rad <- max(avg_summer, na.rm=TRUE)
avg_autumn_max_rad <- max(avg_autumn, na.rm=TRUE)
avg_winter_max_rad <- max(avg_winter, na.rm=TRUE)

avg_spring_max_rad 
avg_summer_max_rad 
avg_autumn_max_rad 
avg_winter_max_rad 



lonlat <- as.matrix( (expand.grid(lon, lat))) 
dim(lonlat) 


ssrd_vec_avg_spring <- as.vector(avg_spring)
ssrd_vec_avg_summer <- as.vector(avg_summer)
ssrd_vec_avg_autumn <- as.vector(avg_autumn)
ssrd_vec_avg_winter <- as.vector(avg_winter) 



length(ssrd_vec_avg_spring)
length(ssrd_vec_avg_summer)
length(ssrd_vec_avg_autumn)
length(ssrd_vec_avg_winter)


avg_spring_ssrd_df <- data.frame(cbind(lonlat, ssrd_vec_avg_spring))
avg_summer_ssrd_df <- data.frame(cbind(lonlat, ssrd_vec_avg_summer))
avg_autumn_ssrd_df <- data.frame(cbind(lonlat, ssrd_vec_avg_autumn))
avg_winter_ssrd_df <- data.frame(cbind(lonlat,ssrd_vec_avg_winter))



colnames(avg_spring_ssrd_df) <- c("lon", "lat", "ssrd")
colnames(avg_summer_ssrd_df) <- c("lon", "lat", "ssrd")
colnames(avg_autumn_ssrd_df) <- c("lon", "lat", "ssrd")
colnames(avg_winter_ssrd_df) <- c("lon", "lat", "ssrd")



avg_spring_ssrd_df_value <- na.omit(avg_spring_ssrd_df)
avg_summer_ssrd_df_value <- na.omit(avg_summer_ssrd_df)
avg_autumn_ssrd_df_value <- na.omit(avg_autumn_ssrd_df)
avg_winter_ssrd_df_value <- na.omit(avg_winter_ssrd_df)


head(avg_spring_ssrd_df_value, 3)
head(avg_summer_ssrd_df_value, 3)
head(avg_autumn_ssrd_df_value, 3)
head(avg_winter_ssrd_df_value, 3) 


library(sf)

avg_spring_ssrd_sf <- st_as_sf(avg_spring_ssrd_df_value, coords = c("lon", "lat"))
avg_summer_ssrd_sf <- st_as_sf(avg_summer_ssrd_df_value, coords = c("lon", "lat"))
avg_autumn_ssrd_sf <- st_as_sf(avg_autumn_ssrd_df_value, coords = c("lon", "lat"))
avg_winter_ssrd_sf<- st_as_sf( avg_winter_ssrd_df_value, coords = c(  "lon", "lat")  )



#convert long and lat to point in simple feature format
#To make it a complete geographical object we assign the WGS84 projection, which has the EPSG code 4326
st_crs(avg_spring_ssrd_sf) <- 4326
st_crs(avg_summer_ssrd_sf) <- 4326
st_crs(avg_autumn_ssrd_sf) <- 4326
st_crs(avg_winter_ssrd_sf) <- 4326 

avg_spring_ssrd_sf <- st_transform(avg_spring_ssrd_sf, 4326)
avg_summer_ssrd_sf <- st_transform(avg_summer_ssrd_sf, 4326)
avg_autumn_ssrd_sf <- st_transform(avg_autumn_ssrd_sf, 4326)
avg_winter_ssrd_sf <- st_transform(avg_winter_ssrd_sf, 4326)


library(tmap)
tmap_mode("view")

tm_shape(avg_spring_ssrd_sf) +
  tm_dots(col = "ssrd", style = "quantile", size = 0.001, palette = "viridis")

tm_shape(avg_summer_ssrd_sf) +
  tm_dots(col = "ssrd", style = "quantile", size = 0.001, palette = "viridis")

tm_shape(avg_autumn_ssrd_sf) +
  tm_dots(col = "ssrd", style = "quantile", size = 0.001, palette = "viridis")


tm_shape(avg_winter_ssrd_sf)+
  tm_dots(col="ssrd", style = "quantile", size=.001, palette = "viridis")


ncatt_get(era2022,"ssrd","units") 

# an example of a 1m2 (A) solar panel
radiation_to_power <- function(G, A=500000, r=0.175, p=0.6, hours=1){
  kWh <- G * A * r * p * (hours/3600) / 1000
  return(kWh)
}

solar_plant$size_m2 = c(solar_plant$capacity_mw*1000*9.2903)


# Radiation data for solar electric (photovoltaic) systems are often represented as kilowatt-hours per square meter (kWh/m2)
# 1 joule/m2 = 1/3600/1000 kWh / m2 (one 1KWh contains 3.6×106 Joules)

avg_spring_ssrd_kwh <- as.data.frame(radiation_to_power(avg_spring_ssrd_df_value))
avg_summer_ssrd_kwh <- as.data.frame(radiation_to_power(avg_summer_ssrd_df_value))
avg_autumn_ssrd_kwh <- as.data.frame(radiation_to_power(avg_autumn_ssrd_df_value))
avg_winter_ssrd_kwh <- as.data.frame (radiation_to_power (avg_winter_ssrd_df_value))


avg_spring_ssrd_df_value <- cbind(avg_spring_ssrd_df_value, avg_spring_ssrd_kwh$ssrd)
avg_summer_ssrd_df_value <- cbind(avg_summer_ssrd_df_value, avg_summer_ssrd_kwh$ssrd)
avg_autumn_ssrd_df_value <- cbind(avg_autumn_ssrd_df_value, avg_autumn_ssrd_kwh$ssrd)
avg_winter_ssrd_df_value <- cbind(avg_winter_ssrd_df_value,avg_winter_ssrd_kwh$ssrd)


colnames(avg_spring_ssrd_df_value)[4] <- 'ssrd_kwh'
colnames(avg_summer_ssrd_df_value)[4] <- 'ssrd_kwh'
colnames(avg_autumn_ssrd_df_value)[4] <- 'ssrd_kwh'
colnames(avg_winter_ssrd_df_value) [4] <- 'ssrd_kwh'

avg_spring_ssrd_sf$ssrd_kwh = avg_spring_ssrd_df_value$ssrd_kwh
avg_summer_ssrd_sf$ssrd_kwh = avg_summer_ssrd_df_value$ssrd_kwh
avg_autumn_ssrd_sf$ssrd_kwh = avg_autumn_ssrd_df_value$ssrd_kwh
avg_winter_ssrd_sf$ssrd_kwh = avg_winter_ssrd_df_value$ssrd_kwh


tm_shape(avg_spring_ssrd_sf)+
  tm_dots(col="ssrd_kwh", style = "quantile", size=.001, palette = "YlOrRd")

tm_shape(avg_summer_ssrd_sf)+
  tm_dots(col="ssrd_kwh", style = "quantile", size=.001, palette = "YlOrRd")

tm_shape(avg_autumn_ssrd_sf)+
  tm_dots(col="ssrd_kwh", style = "quantile", size=.001, palette = "YlOrRd")

tm_shape(avg_winter_ssrd_sf)+
  tm_dots(col="ssrd_kwh", style = "quantile", size=.001, palette = "YlOrRd")



library(readxl)
library(dplyr)

power_plant_Indonesia <- read_excel("F:/Masters/Term 1/BENV0093 Spatial Analysis of Energy Data/assignments/spatial assignment 2/assignment 2/data/power plant Indonesia.xlsx")

power_plant_Indonesia_clean <- na.omit(power_plant_Indonesia)


power_plant_Indonesia_sf <- st_as_sf(power_plant_Indonesia_clean, coords = c("longitude", "latitude"), crs = st_crs(4326))

power_plant_Indonesia_sf$status <- tolower(power_plant_Indonesia_sf$status)
power_plant_Indonesia_sf$type <- tolower(power_plant_Indonesia_sf$type)

solar_plant <- filter(power_plant_Indonesia_sf,type=="solar")


tm_shape(avg_spring_ssrd_sf)+
  tm_dots(col="ssrd_kwh", style = "quantile", size=.001, palette = "YlOrRd")+
  tm_shape(solar_plant)+ 
  tm_dots(col = "status", palette = c("green", "blue"),size ="capacity_mw" ,alpha=0.8)

tm_shape(avg_summer_ssrd_sf)+
  tm_dots(col="ssrd_kwh", style = "quantile", size=.001, palette = "YlOrRd")+
  tm_shape(solar_plant)+ 
  tm_dots(col = "status", palette = c("green", "blue"),size ="capacity_mw", alpha=0.8)

tm_shape(avg_autumn_ssrd_sf)+
  tm_dots(col="ssrd_kwh", style = "quantile", size=.001, palette = "YlOrRd")+
  tm_shape(solar_plant)+ 
  tm_dots(col = "status",palette = c("green", "blue"),size ="capacity_mw", alpha=0.8)

tm_shape(avg_winter_ssrd_sf)+
  tm_dots(col="ssrd_kwh", style = "quantile", size=.001, palette = "YlOrRd")+
  tm_shape(solar_plant)+ 
  tm_dots(col = "status", palette = c("green", "blue"),size ="capacity_mw", alpha=0.8)

library(sf)
library(sp)
library(gstat)

# Convert sf to Spatial object
avg_spring_ssrd_sp <- as(avg_spring_ssrd_sf, "Spatial")
avg_summer_ssrd_sp <- as(avg_summer_ssrd_sf, "Spatial")
avg_autumn_ssrd_sp <- as(avg_autumn_ssrd_sf, "Spatial")
avg_winter_ssrd_sp <- as(avg_winter_ssrd_sf, "Spatial")

# Now you can set coordinates (if needed, but usually not necessary for Spatial objects)
# coordinates(avg_spring_ssrd_sp) <- ~lon+lat

# IDW interpolation
idw_model_spring <- idw(formula = ssrd_kwh ~ 1, locations = avg_spring_ssrd_sf, newdata = solar_plants_sf, nmax = 7, idp = 2.0)
idw_model_summer <- idw(formula = ssrd_kwh ~ 1, locations = avg_summer_ssrd_sf, newdata = solar_plants_sf, nmax = 7, idp = 2.0)
idw_model_autumn <- idw(formula = ssrd_kwh ~ 1, locations = avg_autumn_ssrd_sf, newdata = solar_plants_sf, nmax = 7, idp = 2.0)
idw_model_winter <- idw(formula = ssrd_kwh ~ 1, locations = avg_winter_ssrd_sf, newdata = solar_plants_sf, nmax = 7, idp = 2.0)



# Add the predicted ssrd values to the solar plants data
solar_plants_sf$predicted_ssrd_spring <- idw_model_spring$var1.pred
solar_plants_sf$predicted_ssrd_summer <- idw_model_summer$var1.pred
solar_plants_sf$predicted_ssrd_autumn <- idw_model_autumn$var1.pred
solar_plants_sf$predicted_ssrd_winter <- idw_model_winter$var1.pred

solar_plants_sf$electricity_generated_spring_Kw <- solar_plants_sf$predicted_ssrd_spring *3600* 12 *91
solar_plants_sf$electricity_generated_summer_Kw <- solar_plants_sf$predicted_ssrd_summer *3600* 12 *92
solar_plants_sf$electricity_generated_autumn_Kw <- solar_plants_sf$predicted_ssrd_autumn *3600* 12 *91
solar_plants_sf$electricity_generated_winter_Kw <- solar_plants_sf$predicted_ssrd_winter *3600* 12 *91

# Sum up the electricity generation for each season
total_spring <- sum(solar_plants_sf$electricity_generated_spring_Kw, na.rm = TRUE)
total_summer <- sum(solar_plants_sf$electricity_generated_summer_Kw, na.rm = TRUE)
total_autumn <- sum(solar_plants_sf$electricity_generated_autumn_Kw, na.rm = TRUE)
total_winter <- sum(solar_plants_sf$electricity_generated_winter_Kw, na.rm = TRUE)

total_predicted_electricity_generated=+total_spring+total_summer+total_autumn+total_winter
total_predicted_electricity_generated/ 1e9

total_spring_TW <- total_spring / 1e9
total_summer_TW <- total_summer / 1e9
total_autumn_TW <- total_autumn / 1e9
total_winter_TW <- total_winter / 1e9

# Create strings with values and unit
total_spring_TW_str <- paste(total_spring_TW, "TW")
total_summer_TW_str <- paste(total_summer_TW, "TW")
total_autumn_TW_str <- paste(total_autumn_TW, "TW")
total_winter_TW_str <- paste(total_winter_TW, "TW")

# Print the values
print(total_spring_TW_str)
print(total_summer_TW_str)
print(total_autumn_TW_str)
print(total_winter_TW_str)

# Convert the predictions to sf object if needed
#ssrd_prediction_spring_sf <- st_as_sf(ssrd_prediction_spring)


# #install.packages("gstat")
# library(gstat)
# # Assuming you want to interpolate for the spring season
# ssrd_data <- avg_spring_ssrd_sf
# avg_spring_ssrd_sf
# # Create a variogram model
# ssrd_vgm <- variogram(ssrd ~ 1, data = ssrd_data)
# plot(ssrd_vgm)
# max(ssrd_vgm$dist)
# 
# vaBin  <- variogram(ssrd_kwh ~ 1, ssrd_data,width=max(ssrd_vgm$dist) / 2)
# #vaBin  <- variogram(ssrd_kwh ~ 1, ssrd_data,width=max(vacloud$dist) / 30)
# 
# length (vaBin$gamma) #gamma is number of points representing each bin
# 
# COL <- adjustcolor("#009999", alpha.f = 0.4)
# plot(vaBin,pch=19,col=COL)
# 
# print(show.vgms())  #variogram fits/shapes available
# 
# vgms()
# 
# v.fit.exp = fit.variogram(vaBin, model=vgm("Exp", psill=3000, range=220000, nugget=0))
# 
# plot(vaBin, v.fit.exp)
# 
# 
# coor = as.data.frame(st_coordinates(ssrd_data))
# ssrd_data$x = coor$X
# ssrd_data$y = coor$Y
# ssrd_nogeom = st_drop_geometry(ssrd_data)
# 
# kr <- gstat(formula=ssrd_kwh~1, locations=~x+y, data=ssrd_nogeom, model=v.fit.exp)
# 
# library(dplyr)
# library(terra)
# library(raster)
# 
# # Define the extent, resolution, and CRS for your raster grid
# ext <- extent(c(-597650, 4770078, -1234396, 819629))
# res <- 3000  # Adjust the resolution as needed
# crs <- "+proj=longlat +datum=WGS84"  # Define the CRS
# # Define the extent
# ext <- extent(c(-597650, 4770078, -1234396, 819629))
# 
# # Define the resolution
# res <- 3000  # Adjust the resolution as needed
# 
# # Define the CRS
# crs <- "+proj=longlat +datum=WGS84"
# 
# # Create the raster object
# raster_template <- raster(ext, res=res, crs=crs)
# 
# raster_template #EPSG:32735
# 
# 
# k_intp <- interpolate(raster_template, kr, debug.level=0)
# 
# plot(k_intp$var1.pred)
# 
# V_solar.sf = st_as_sf(solar_plant)
# raster_V_solar = rasterize(V_solar.sf, raster_template, "solar")
# plot(raster_V_solar)
