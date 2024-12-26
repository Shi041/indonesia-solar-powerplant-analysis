
setwd("F:/Masters/Term 1/BENV0093 Spatial Analysis of Energy Data/assignments/spatial assignment 2/assignment 2/solar nc")
library(RColorBrewer)
library(readxl)
library(dplyr)

power_plant_Indonesia <- read_excel("F:/Masters/Term 1/BENV0093 Spatial Analysis of Energy Data/assignments/spatial assignment 2/assignment 2/data/power plant Indonesia.xlsx")

power_plant_Indonesia_clean <- na.omit(power_plant_Indonesia)


power_plant_Indonesia_sf <- st_as_sf(power_plant_Indonesia_clean, coords = c("longitude", "latitude"), crs = st_crs(4326))

power_plant_Indonesia_sf$status <- tolower(power_plant_Indonesia_sf$status)
power_plant_Indonesia_sf$type <- tolower(power_plant_Indonesia_sf$type)

solar_plant <- filter(power_plant_Indonesia_sf,type=="solar")

library(ncdf4) 
era2022 <- nc_open("2022_4_data.nc" )
era2022 
summary(era2022)

lon <- ncvar_get(era2022, "longitude")
lat <- ncvar_get(era2022, "latitude")
time <- ncvar_get(era2022, "time")
time

dim(time) 

tunits <- ncatt_get(era2022,"time","units") 

(time[1]%%(24*365) ) %% 24
(time[2]%%(24*365) ) %% 24
(time[3]%%(24*365) ) %% 24
(time[4]%%(24*365) ) %% 24
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

mean_rad1 <- mean(spring_data, na.rm=TRUE)
mean_rad2 <- mean(summer_data, na.rm=TRUE)
mean_rad3 <- mean(autumn_data, na.rm=TRUE)
mean_rad4 <- mean(winter_data, na.rm=TRUE)
mean_rad_t  <- mean_rad1+mean_rad2+mean_rad3+mean_rad4
mean_rad <- mean_rad_t/4
mean_rad

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


library(readxl)
power_plant_Indonesia <- read_excel("F:/Masters/Term 1/BENV0093 Spatial Analysis of Energy Data/assignments/spatial assignment 2/assignment 2/data/power plant Indonesia.xlsx")

power_plant_Indonesia_clean <- na.omit(power_plant_Indonesia)


power_plant_Indonesia_sf <- st_as_sf(power_plant_Indonesia_clean, coords = c("longitude", "latitude"), crs = st_crs(4326))

power_plant_Indonesia_sf$status <- tolower(power_plant_Indonesia_sf$status)

solar_plant <- filter(power_plant_Indonesia_sf, type == "solar")

sum(solar_plant$capacity_mw)
mean(solar_plant$capacity_mw)

Area_per_kW =9.2903 # 1kW solar = 100 square feel = 9.2903 square
solar_plant$area_m2 = c(mean(solar_plant$capacity_mw)*1000*Area_per_kW)
solar_plant$area_m2[1]
# Exclude row 11
946546774.5/(9.29030*1000)
#solar_plant_excluding_outlier <- solar_plant[-11, ]

# Calculate the average area
#average_area <- mean(solar_plant_excluding_outlier$area_m2, na.rm = TRUE)
#average_area
# an example of a 1m2 (A) solar panel
radiation_to_power <- function(G, A=solar_plant$area_m2[1], r=0.175, p=0.6, hours=1){
  kWh <- G * A * r * p * (hours/3600) / 1000
  return(kWh)
}

solar_plant1<-solar_plant



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

avg_spring_ssrd_sf$ssrd_GWh = avg_spring_ssrd_df_value$ssrd_kwh/1e6
avg_summer_ssrd_sf$ssrd_GWh = avg_summer_ssrd_df_value$ssrd_kwh/1e6
avg_autumn_ssrd_sf$ssrd_GWh = avg_autumn_ssrd_df_value$ssrd_kwh/1e6
avg_winter_ssrd_sf$ssrd_GWh = avg_winter_ssrd_df_value$ssrd_kwh/1e6

tm_shape(avg_spring_ssrd_sf)+
  tm_dots(col="ssrd_GWh", style = "quantile", size=.001, palette = "YlOrRd")

tm_shape(avg_summer_ssrd_sf)+
  tm_dots(col="ssrd_GWh", style = "quantile", size=.001, palette = "YlOrRd")

tm_shape(avg_autumn_ssrd_sf)+
  tm_dots(col="ssrd_GWh", style = "quantile", size=.001, palette = "YlOrRd")

tm_shape(avg_winter_ssrd_sf)+
  tm_dots(col="ssrd_GWh", style = "quantile", size=.001, palette = "YlOrRd")






tm_shape(avg_spring_ssrd_sf)+
  tm_dots(col="ssrd_GWh", style = "quantile", size=.001, palette = "YlOrRd")+
  tm_shape(solar_plant)+ 
  tm_dots(col = "status", palette = c("green", "blue"),size ="capacity_mw" ,alpha=0.8)

tm_shape(avg_summer_ssrd_sf)+
  tm_dots(col="ssrd_GWh", style = "quantile", size=.001, palette = "YlOrRd")+
  tm_shape(solar_plant)+ 
  tm_dots(col = "status", palette = c("green", "blue"),size ="capacity_mw", alpha=0.8)

tm_shape(avg_autumn_ssrd_sf)+
  tm_dots(col="ssrd_GWh", style = "quantile", size=.001, palette = "YlOrRd")+
  tm_shape(solar_plant)+ 
  tm_dots(col = "status",palette = c("green", "blue"),size ="capacity_mw", alpha=0.8)

tm_shape(avg_winter_ssrd_sf)+
  tm_dots(col="ssrd_GWh", style = "quantile", size=.001, palette = "YlOrRd")+
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

# IDW interpolation
idw_model_spring <- idw(formula = ssrd_kwh ~ 1, locations = avg_spring_ssrd_sf, newdata = solar_plant, nmax = 7, idp = 2.0)
idw_model_summer <- idw(formula = ssrd_kwh ~ 1, locations = avg_summer_ssrd_sf, newdata = solar_plant, nmax = 7, idp = 2.0)
idw_model_autumn <- idw(formula = ssrd_kwh ~ 1, locations = avg_autumn_ssrd_sf, newdata = solar_plant, nmax = 7, idp = 2.0)
idw_model_winter <- idw(formula = ssrd_kwh ~ 1, locations = avg_winter_ssrd_sf, newdata = solar_plant, nmax = 7, idp = 2.0)



# Add the predicted ssrd values to the solar plants data
solar_plant$predicted_ssrd_spring <- idw_model_spring$var1.pred
solar_plant$predicted_ssrd_summer <- idw_model_summer$var1.pred
solar_plant$predicted_ssrd_autumn <- idw_model_autumn$var1.pred
solar_plant$predicted_ssrd_winter <- idw_model_winter$var1.pred

solar_plant$predicted_ssrd_spring_GWh <- solar_plant$predicted_ssrd_spring /1e6
solar_plant$predicted_ssrd_summer_GWh <- solar_plant$predicted_ssrd_summer/1e6
solar_plant$predicted_ssrd_autumn_GWh <- solar_plant$predicted_ssrd_autumn/1e6
solar_plant$predicted_ssrd_winter_GWh <- solar_plant$predicted_ssrd_winter/1e6


solar_plant$electricity_generated_spring_Kw <- solar_plant$predicted_ssrd_spring * 12 *91
solar_plant$electricity_generated_summer_Kw <- solar_plant$predicted_ssrd_summer * 12 *92
solar_plant$electricity_generated_autumn_Kw <- solar_plant$predicted_ssrd_autumn * 12 *91
solar_plant$electricity_generated_winter_Kw <- solar_plant$predicted_ssrd_winter * 12 *91

# Sum up the electricity generation for each season
total_spring <- sum(solar_plant$electricity_generated_spring_Kw, na.rm = TRUE)
total_summer <- sum(solar_plant$electricity_generated_summer_Kw, na.rm = TRUE)
total_autumn <- sum(solar_plant$electricity_generated_autumn_Kw, na.rm = TRUE)
total_winter <- sum(solar_plant$electricity_generated_winter_Kw, na.rm = TRUE)

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




total_in_mWh1 <-sum(solar_plant$predicted_ssrd_spring,solar_plant$predicted_ssrd_summer,
                   solar_plant$predicted_ssrd_autumn,solar_plant$predicted_ssrd_winter )/1e6
print ( "projected solar plant generation is ") 
total_in_mWh1



all_season<-(total_spring_TW+total_summer_TW+total_autumn_TW+total_winter_TW)

all_season/365


# for (i in 2:length(solar_plant1$area_m2)) {
#   solar_plant1$area_m2[i]
#   radiation_to_power1 <- function(G, A=solar_plant1$area_m2[i], r=0.175, p=0.6, hours=1){
#     kWh <- G * A * r * p * (hours/3600) / 1000
#     return(kWh)
#   }
#   
#   avg_spring_ssrd_sf1 <- st_as_sf(avg_spring_ssrd_df_value, coords = c("lon", "lat"))
#   avg_summer_ssrd_sf1 <- st_as_sf(avg_summer_ssrd_df_value, coords = c("lon", "lat"))
#   avg_autumn_ssrd_sf1 <- st_as_sf(avg_autumn_ssrd_df_value, coords = c("lon", "lat"))
#   avg_winter_ssrd_sf1<- st_as_sf( avg_winter_ssrd_df_value, coords = c(  "lon", "lat")  )
#  
#    st_crs(avg_spring_ssrd_sf1) <- 4326
#   st_crs(avg_summer_ssrd_sf1) <- 4326
#   st_crs(avg_autumn_ssrd_sf1) <- 4326
#   st_crs(avg_winter_ssrd_sf1) <- 4326 
#   
#   avg_spring_ssrd_sf1 <- st_transform(avg_spring_ssrd_sf1, 4326)
#   avg_summer_ssrd_sf1 <- st_transform(avg_summer_ssrd_sf1, 4326)
#   avg_autumn_ssrd_sf1 <- st_transform(avg_autumn_ssrd_sf1, 4326)
#   avg_winter_ssrd_sf1 <- st_transform(avg_winter_ssrd_sf1, 4326)
#   
#   avg_spring_ssrd_kwh1 <- as.data.frame(radiation_to_power1(avg_spring_ssrd_df_value))
#   avg_summer_ssrd_kwh1 <- as.data.frame(radiation_to_power1(avg_summer_ssrd_df_value))
#   avg_autumn_ssrd_kwh1 <- as.data.frame(radiation_to_power1(avg_autumn_ssrd_df_value))
#   avg_winter_ssrd_kwh1 <- as.data.frame (radiation_to_power1 (avg_winter_ssrd_df_value))
#   
#   
#   avg_spring_ssrd_df_value1 <- cbind(avg_spring_ssrd_df_value, avg_spring_ssrd_kwh1$ssrd)
#   avg_summer_ssrd_df_value1 <- cbind(avg_summer_ssrd_df_value, avg_summer_ssrd_kwh1$ssrd)
#   avg_autumn_ssrd_df_value1 <- cbind(avg_autumn_ssrd_df_value, avg_autumn_ssrd_kwh1$ssrd)
#   avg_winter_ssrd_df_value1 <- cbind(avg_winter_ssrd_df_value,avg_winter_ssrd_kwh1$ssrd)
#   
#   colnames(avg_spring_ssrd_df_value1)[4] <- 'ssrd_kwh'
#   colnames(avg_summer_ssrd_df_value1)[4] <- 'ssrd_kwh'
#   colnames(avg_autumn_ssrd_df_value1)[4] <- 'ssrd_kwh'
#   colnames(avg_winter_ssrd_df_value1) [4] <- 'ssrd_kwh'
#   
#   avg_spring_ssrd_sf1$ssrd_kwh = avg_spring_ssrd_df_value$ssrd_kwh
#   avg_summer_ssrd_sf1$ssrd_kwh = avg_summer_ssrd_df_value$ssrd_kwh
#   avg_autumn_ssrd_sf1$ssrd_kwh = avg_autumn_ssrd_df_value$ssrd_kwh
#   avg_winter_ssrd_sf1$ssrd_kwh = avg_winter_ssrd_df_value$ssrd_kwh
#   
#   
#   # Convert sf to Spatial object
#   avg_spring_ssrd_sp1 <- as(avg_spring_ssrd_sf1, "Spatial")
#   avg_summer_ssrd_sp1 <- as(avg_summer_ssrd_sf1, "Spatial")
#   avg_autumn_ssrd_sp1 <- as(avg_autumn_ssrd_sf1, "Spatial")
#   avg_winter_ssrd_sp1 <- as(avg_winter_ssrd_sf1, "Spatial")
#   
# 
#   # IDW interpolation
#   idw_model_spring1 <- idw(formula = ssrd_kwh ~ 1, locations = avg_spring_ssrd_sf1, newdata = solar_plant1, nmax = 7, idp = 2.0)
#   idw_model_summer1 <- idw(formula = ssrd_kwh ~ 1, locations = avg_summer_ssrd_sf1, newdata = solar_plant1, nmax = 7, idp = 2.0)
#   idw_model_autumn1 <- idw(formula = ssrd_kwh ~ 1, locations = avg_autumn_ssrd_sf1, newdata = solar_plant1, nmax = 7, idp = 2.0)
#   idw_model_winter1 <- idw(formula = ssrd_kwh ~ 1, locations = avg_winter_ssrd_sf1, newdata = solar_plant1, nmax = 7, idp = 2.0)
#   
#   
#   
#   # Add the predicted ssrd values to the solar plants data
#   solar_plant1$predicted_ssrd_spring[i] <- idw_model_spring1$var1.pred
#   solar_plant1$predicted_ssrd_summer[i] <- idw_model_summer1$var1.pred
#   solar_plant1$predicted_ssrd_autumn[i] <- idw_model_autumn1$var1.pred
#   solar_plant1$predicted_ssrd_winter[i] <- idw_model_winter1$var1.pred
# }



