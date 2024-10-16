#==============================================================================
#                              Analysis Repository:                           #
#                                 Spatial Merge                               #      
#==============================================================================

#===========================================
# Load Libraries                           #
#==========================================-
library(tidygeocoder) # has all geocode tools
library(tidyverse) #dplyr, ggplot2, tidyr, readr
library(sf) #Used in much spatial and mapping analyses
library(tigris) #used to download shape

#library(tmap) # used in tm mapps
#library(viridis) #Has some nice colors
#library(mapview) #interactive maps

#library(rgdal)
#library(raster)
#library(spdep)
#library(plyr)

#===========================================
# Simulate Data OR LOAD SPATIAL DATA To be joined TO!  #
#==========================================-

# Get shape file
MT.bg <- tigris::counties(state = 30, year=2019)

MT.dat.sf <- st_as_sf(MT.bg)
MT.dat.sf <- sf::st_transform(MT.bg, "NAD27")

# Make sure it looks good
ggplot() +
  geom_sf(data = MT.dat.sf)

# Can also load in shape files
dat.shape <- rgdal::readOGR(".shp")
dat.shape <- sf::st_as_sf(dat.shape)

# Also note that may need to adjust the coordinate projection for these shape files
dat.shape <- sf::st_transform(dat.shape, "NAD27")


#===========================================
# Simulate Data OR LOAD data that will be joined to spatial  #
#==========================================-

# Will load some data that has been geocoded
dat <- (read.csv("data/mt.csv", header=TRUE))
dat <- dat[1:100,]

# SF transform can't use missing, so select only files with Lat and long
dat <-  dat %>%                                  
  filter(!is.na(LON))

# Convert data into sf object
dat.sf <- sf::st_as_sf(dat, coords = c("LON","LAT"), crs = "NAD27",remove = F)

# make sure the data looks good
ggplot() +
  geom_sf(data=dat.sf)


#===========================================
# Join data                                #
#==========================================-

# Can get creative here, this is a simple join
# But the data strucutre can get complicated, fast! 
MT.dat.sf.joined <- sf::st_join(MT.dat.sf, dat.sf)

head(MT.dat.sf.joined)

MT.dat.sf.joined <- MT.dat.sf.joined %>%
  group_by(GEOID) %>%
  summarise(n = n())

# This one will make many duplicates

# lets look once more again
ggplot() +
  geom_sf(data = MT.dat.sf.joined)


