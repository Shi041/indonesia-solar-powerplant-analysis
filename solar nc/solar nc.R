
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
library(ggplot2)
library(ggmap)
library(RColorBrewer)
library(knitr)    # For knitting document and include_graphics function
library(ggplot2)  # For plotting
library(png) 

setwd("F:/Masters/Term 1/BENV0093 Spatial Analysis of Energy Data/assignments/spatial assignment 2/assignment 2/solar nc")

library(ncdf4) #library to read and process netcdf data
era2022 <- nc_open("2022_48data.nc" )
era2022 #3 variables [u10, v10, ssrd], 3 dimensions [longitude,latitude,time] 
summary(era2022)

lon <- ncvar_get(era2022, "longitude")
lat <- ncvar_get(era2022, "latitude")
time <- ncvar_get(era2022, "time")
time


dim(time) #you can also use dim() to try dim(lat) or dim(lon), and compare the results .


# get the unit of data
tunits <- ncatt_get(era2022,"time","units") #tunits <- ncatt_get(era2022,"longitude","units")

tunits


(time[1]%%(24*365) ) %% 24
(time[2]%%(24*365) ) %% 24
(time[3]%%(24*365) ) %% 24
(time[4]%%(24*365) ) %% 24
(time[5]%%(24*365) ) %% 24
(time[6]%%(24*365) ) %% 24
(time[7]%%(24*365) ) %% 24

(time[84]%%(24*365) ) %% 24

library(chron) #deal with chronological objects
# 
# #convert time -- split the time units string into fields
# tustr <- strsplit(tunits$value, " ") #strsplit: split the element of character vector. we can convert  "hours since 1900-01-01" to "hours"      "since"      "1900-01-01"
# tustr
# tdstr <- strsplit(unlist(tustr)[3], "-") #convert "1900-01-01" to "1900" "01"   "01"
# tdstr
# tyear <- as.integer(unlist(tdstr)[1]) 
# tyear
# tmonth <- as.integer(unlist(tdstr)[3])
# tmonth
# tday <- as.integer(unlist(tdstr)[3])
# tday
# 
# chron(time/24, origin=c(tmonth, tday, tyear) ) #this function is of great help. It can convert the hours since format to the format we are quite familiar with.
# 

#get Variables :u10,v10, ssrd. similar to get dimension data, we are going to use the same method, ncvar_get()
ssrd_array2022 <- ncvar_get(era2022,"ssrd") #get the Surface solar radiation downwards
dim(ssrd_array2022) #dimension is 501 * 186 *8. Think about the Figure 1. The reason why it is called array is it is composed of 8 slices 
# 
# 
# dlname <- ncatt_get(era2022,"ssrd","long_name")
# dlname
# dunits <- ncatt_get(era2022,"ssrd","units")
# dunits
# fillvalue <- ncatt_get(era2022,"ssrd","_FillValue")
# fillvalue

library(lattice)

library(RColorBrewer)

ssrd_array2022
#Get a single time slice of the data using ssrd_array2022
dim(ssrd_array2022 )
ssrd_array2022[,,48]
#ssrd_array2022[,,84] means 12/01/2022 at 20:00 
#when (time[84]%%(24*365) ) %% 24
# [1] 20
# > (time[85]%%(24*365) ) %% 24
# [1] NA 
#so for jan = ssrd_array2022[,,1 to 7]
#febuary = ssrd_array2022[,,8-14]...

ssrd_slice2022 <- ssrd_array2022[,,48] 
#The ssrd_slice is actually a matrix. class(ssrd_slice)
# What does 2 in ssrd_array2022[,,2] indicate?  What if I want to slice all four time slice for "07/01/19"? 

length(na.omit(as.vector(ssrd_slice2022))) /length(as.vector(ssrd_slice2022)) #8.5% are valid
dim(ssrd_slice2022 )


# Example: Converting a vector to a matrix with 10 rows and N/10 columns
#ssrd_slice_matrix <- matrix(ssrd_slice)

#image(ssrd_slice_matrix, col=rev(brewer.pal(10,"RdBu")))

#plot(ssrd_slice2022_matrix)

image(ssrd_slice2022, col=rev(brewer.pal(10,"RdBu")) )
image(ssrd_array2022[,,1] , col=rev(brewer.pal(10,"RdBu")) )

#ssrd_slice2022


#example: check one max point
max_rad <- max(ssrd_slice2022, na.rm=TRUE)
max_rad



lonlat <- as.matrix( (expand.grid(lon, lat))) #lon and lat are what we extracted in step 2.
dim(lonlat) #we now have a matrix that has 93186 rows and 2columns. 93186=501*186

ssrd_vec <- as.vector( ssrd_slice2022) 
length(ssrd_vec)

ssrd_df <- data.frame( cbind( lonlat,ssrd_vec  ))
colnames(ssrd_df) <- c("lon", "lat", "ssrd")
ssrd_df_value <- na.omit (ssrd_df)
head(ssrd_df_value, 3) 



library(sf)
ssrd_sf<- st_as_sf( ssrd_df_value, coords = c(  "lon", "lat")  ) #convert long and lat to point in simple feature format
#To make it a complete geographical object we assign the WGS84 projection, which has the EPSG code 4326
st_crs(ssrd_sf) <- 4326 
ssrd_sf <- st_transform(ssrd_sf, 4326 )

library(tmap)
tmap_mode("view")

tm_shape(ssrd_sf)+
  tm_dots(col="ssrd", style = "quantile", size=.001, palette = "viridis")


ncatt_get(era2022,"ssrd","units") #joul per metre2 

# an example of a 1m2 (A) solar panel
radiation_to_power <- function(G, A=1, r=0.175, p=0.6, hours=1){
  kWh <- G * A * r * p * (hours/3600) / 1000
  return(kWh)
}
# Radiation data for solar electric (photovoltaic) systems are often represented as kilowatt-hours per square meter (kWh/m2)
# 1 joule/m2 = 1/3600/1000 kWh / m2 (one 1KWh contains 3.6×106 Joules)

ssrd_kwh <- as.data.frame (radiation_to_power (ssrd_df_value))
ssrd_df_value <- cbind(ssrd_df_value,ssrd_kwh$ssrd)
colnames(ssrd_df_value) [4] <- 'ssrd_kwh'
ssrd_sf$ssrd_kwh = ssrd_kwh$ssrd

tm_shape(ssrd_sf)+
  tm_dots(col="ssrd_kwh", style = "quantile", size=.001, palette = "YlOrRd")

