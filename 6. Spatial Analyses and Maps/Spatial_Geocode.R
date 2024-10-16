#==============================================================================
#                              Analysis Repository:                           #
#                                Spatial Geocoder                             #      
#==============================================================================

#===========================================
# Load Libraries                           #
#==========================================-
library(tidygeocoder) # has all geocode tools
library(tidyverse) #dplyr, ggplot2, tidyr, readr
#library(tmap) # used in tm mapps
#library(sf) #Used in much spatial and mapping analyses

#library(viridis) #Has some nice colors
#library(mapview) #interactive maps

#library(tigris)

#library(rgdal)
#library(raster)
#library(spdep)
#library(plyr)


#===========================================
# Simulate Data OR LOAD DATA               #
#==========================================-

dat <- (read.csv("data/mt.csv", header=TRUE))

dat$Num_Street <- paste(dat$NUMBER,dat$STREET)

dat$Address <- paste(dat$Num_Street,dat$CITY,dat$REGION,dat$POSTCODE)

dat <- dat[1:100,]

# Read in the shape file
#dat.shape <- rgdal::readOGR(".shp")

# Convert shape file to sf format
#dat.sf <- sf::st_as_sf(dat.shape)


#===========================================
# Geocode               #
#==========================================-

# Will geocode using the tidygeocoder
# Many methods that can be used, mostly osm, census, and arggis

address_lat_long_OSM <- dat %>%
  geocode(street = Num_Street, city = CITY, 
          state = REGION, postalcode = POSTCODE, 
          method = 'osm', 
          lat = latitude, 
          long = longitude)
# In general OSM does not seem very great or reliabile

address_lat_long_CENSUS <- dat %>%
  geocode(street = Num_Street, city = CITY, 
          state = REGION, postalcode = POSTCODE, 
          method = 'census', 
          lat = latitude, 
          long = longitude,
          full_results = T,
          api_options = list(census_return_type = 'geographies'))
# Census is much better
# Can add census api options to get census geographies 

address_lat_long_ARCGIS <- dat %>%
  geocode(address = Address, 
          method = 'arcgis', 
          lat = latitude, 
          long = longitude)
# ArcGIS seems to be the BEST one


ggplot(address_lat_long_ARCGIS, aes(longitude, latitude), color = "grey99") +
  borders("state") + geom_point() +
  theme_void()


#===========================================
# Write and Export as needed               #
#==========================================-

write.csv(address_lat_long_ARCGIS, "Address_Geocode.csv")

#shapefile(address_lat_long_ARCGIS, filename='Address_Geocode.shp')

