plot(Administrative_areas_sf$geometry)
plot(landcover,add=TRUE)
title("Land cover")
north_arrow_minimal()
scalebar()

plot(land_potential$IDN_msk_cov<=1)
plot(Administrative_areas_sf$geometry,add=TRUE)
title("Land cover constrained Less than 15")

plot(protected_raster$layer)
plot(Administrative_areas_sf$geometry,add=TRUE)
title("protected areas")


plot(peatland_raster$layer)
plot(Administrative_areas_sf$geometry,add=TRUE)
title("peatland areas")

plot(pop_crop, col = terrain.colors(1000) )
plot(Administrative_areas_sf$geometry,add=TRUE)
title("population")

plot(pop_raster$idn_msk_pop>3)
plot(Administrative_areas_sf$geometry,add=TRUE)
title("population constrained less than 1000")

custom_colors <- c("blue", "green", "red")
plot(Indonesia_grid$geometry,col="black")
plot(ind_grid_combine_fix,add=TRUE,col=custom_colors)
plot(Administrative_areas_sf$geometry,add=TRUE)
title("distance to grid zones (km)")


plot(road_Indonesia$geometry)
plot(road_Indonesia_1km_raster,add=TRUE)
plot(Administrative_areas_sf$geometry,add=TRUE)
title("distance to road zones 1km")

plot(elevation_mask)
plot(Administrative_areas_sf$geometry,add=TRUE)
title("elevation")

plot(slope)
plot(Administrative_areas_sf$geometry,add=TRUE)
title("slope")

plot(elevation_rc$slope)
plot(Administrative_areas_sf$geometry,add=TRUE)
title("slope constrained ")

plot(elevation_rc$slope>4)
plot(Administrative_areas_sf$geometry,add=TRUE)
title("slope constrained less than 20 degrees ")

plot(idw_mask$`predicted ssrd`>16000000)
plot(Administrative_areas_sf$geometry,add=TRUE)
title("solar irradiation ssrd above 16000000")

constrained<- raster("D:/SPATIAL DATA ANALYSIS/ASSIGNMENT2/constrained.tif")
plot(constrained, col="green")
crs(constrained)=4326
crs(Administrative_areas_sf)=4326
plot(Administrative_areas_sf$geometry,add=TRUE)
title("combined constrained")
