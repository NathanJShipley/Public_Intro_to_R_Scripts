#==============================================================================
#                              Analysis Repository:                           #
#                                     Maps                                    #      
#==============================================================================

#===========================================
# Load Libraries                           #
#==========================================-
#library(tidycensus)
library(tidyverse) #dplyr, ggplot2, tidyr, readr
library(sf) #Used in much spatial and mapping analyses
library(tmap) # used in tm mapps
library(viridis) #Has some nice colors
library(mapview) #interactive maps


#===========================================
# Simulate Data OR LOAD DATA               #
#==========================================-

# Read in the shape file
dat.shape <- rgdal::readOGR("data/E_C_S_M/Evictions_Census_Spatial_Merged.shp")

# Convert shape file to sf format
dat.sf <- sf::st_as_sf(dat.shape)

dat.sf <- dat.sf %>% 
  mutate(Tot_PpE = na_if(Tot_PpE,-999))


#===========================================
# Some BASIC Chorpleth maps                #
#==========================================-

# polygons is a wrapper for fill and borders
tm_shape(dat.sf) +
  tm_polygons("Tot_PpE")

tm_shape(dat.sf) +
  tm_borders()

tm_shape(dat.sf) +
  tm_fill()

tm_shape(dat.sf) +
  tm_borders() + 
  tm_fill("Tot_PpE")
# Can seperate polygons into borders and fill, combine to make same map
# offers more customization 

# Now lets add some Colors
tm_shape(dat.sf) +
  tm_borders() + 
  tm_fill("Tot_PpE", palette = "Reds")

tm_shape(dat.sf) +
  tm_borders() + 
  tm_fill("Tot_PpE", palette = "cividis")

# Can add a title to the legend
tm_shape(dat.sf) +
  tm_borders() + 
  tm_fill("Tot_PpE", palette = "cividis", title = "Total Population")

# Can move the legend
tm_shape(dat.sf) +
  tm_borders() + 
  tm_fill("Tot_PpE", palette = "cividis", title = "Total Population") + 
  tm_layout(legend.position = c("right", "top"))

# can move legend outside the figure
tm_shape(dat.sf) +
  tm_borders() + 
  tm_fill("Tot_PpE", palette = "cividis", title = "Total Population") + 
  tm_layout(legend.outside = T, legend.outside.position = "right")

# Further add a hitogram
tm_shape(dat.sf) +
  tm_borders() + 
  tm_fill("Tot_PpE", palette = "cividis", title = "Total Population", legend.hist=TRUE) + 
  tm_layout(legend.outside = T, legend.outside.position = "right")

# Add a title to the overall map
tm_shape(dat.sf) +
  tm_borders() + 
  tm_fill("Tot_PpE", palette = "cividis", title = "Total Population", legend.hist=TRUE,
          style = "jenks", n=5) + 
  tm_layout(legend.outside = T, legend.outside.position = "right",
            main.title = "Total Population of Dane County, 2015-2019 ACS",
            main.title.size = 1.7, main.title.position = c("left", "top")) +
  tm_compass(type = "arrow", position = c("left", "top"))

#====================== =
# Types of Map Breaks # #
#====================== =

#### Base feature of TM called "Pretty maps"
tm_shape(dat.sf) +
  tm_borders() + 
  tm_fill("Tot_PpE", palette = "cividis", title = "Total Population", legend.hist=TRUE) + 
  tm_layout(legend.outside = T, legend.outside.position = "right",
            main.title = "Total Population of Dane County, 2015-2019 ACS",
            main.title.size = 1.7, main.title.position = c("left", "top")) +
  tm_compass(type = "arrow", position = c("left", "top"))


#### QUANTILE MAP, breaks into 5 bins of equal size 
tm_shape(dat.sf) +
  tm_borders() + 
  tm_fill("Tot_PpE", palette = "cividis", title = "Total Population", legend.hist=TRUE,
          style = "quantile") + 
  tm_layout(legend.outside = T, legend.outside.position = "right",
            main.title = "Total Population of Dane County, 2015-2019 ACS",
            main.title.size = 1.7, main.title.position = c("left", "top")) +
  tm_compass(type = "arrow", position = c("left", "top"))


#### QUartile MAP, breaks into 4 bins of equal size
tm_shape(dat.sf) +
  tm_borders() + 
  tm_fill("Tot_PpE", palette = "cividis", title = "Total Population", legend.hist=TRUE,
          style = "quantile", n=4) + 
  tm_layout(legend.outside = T, legend.outside.position = "right",
            main.title = "Total Population of Dane County, 2015-2019 ACS",
            main.title.size = 1.7, main.title.position = c("left", "top")) +
  tm_compass(type = "arrow", position = c("left", "top"))


#### Natural Breaks, uses JENKS
tm_shape(dat.sf) +
  tm_borders() + 
  tm_fill("Tot_PpE", palette = "cividis", title = "Total Population", legend.hist=TRUE,
          style = "jenks", n=4) + 
  tm_layout(legend.outside = T, legend.outside.position = "right",
            main.title = "Total Population of Dane County, 2015-2019 ACS",
            main.title.size = 1.7, main.title.position = c("left", "top")) +
  tm_compass(type = "arrow", position = c("left", "top"))


#### Equal Interval breaks, sets the intervals to be equal
tm_shape(dat.sf) +
  tm_borders() + 
  tm_fill("Tot_PpE", palette = "cividis", title = "Total Population", legend.hist=TRUE,
          style = "equal", n=4) + 
  tm_layout(legend.outside = T, legend.outside.position = "right",
            main.title = "Total Population of Dane County, 2015-2019 ACS",
            main.title.size = 1.7, main.title.position = c("left", "top")) +
  tm_compass(type = "arrow", position = c("left", "top"))


###### Final overall map that displays everything
tm_shape(dat.sf) +
  tm_borders() + 
  tm_fill("Tot_PpE", palette = "cividis", title = "Total Population", legend.hist=TRUE,
          style = "jenks", n=5,legend.is.portrait = TRUE) + 
  tm_layout(legend.outside = T, legend.outside.position = "right",
            main.title = "Total Population of Dane County, 2015-2019 ACS",
            main.title.size = 1.7, main.title.position = c("left", "top")) +
  tm_compass(type = "arrow", position = c("left", "top"))


#====================== =
# Interactive Maps     # #
#====================== =

mapview(dat.sf, zcol = "Tot_PpE", legend = TRUE)


#============================= =
# GGplot maps also are good  # #
#============================= =

ggplot(dat.sf, aes(fill = Tot_PpE)) + 
  geom_sf() +
  theme_classic() + 
  scale_fill_viridis(option = "cividis") + 
  labs(title = "Total Population of Dane County, 2015-2019 ACS", 
       subtitle = "Block groups in Dane County, Wisconsin",
       fill = "Total Population") +
  scale_x_continuous(expand = c(0, 0)) + 
  scale_y_continuous(expand = c(0, 0)) + 
  expand_limits(x = dat.sf$X, y = dat.sf$Y) + 
  theme(axis.line = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        #legend.position = "none",
        plot.title = element_text(size = 16, face = "bold"),
        plot.subtitle = element_text(size = 14))


#============================= =
# Dot Map                    # #
#============================= =

#census_api_key("7a68fa479e8bcac3209caa8b994d5b438b547eab",install = T)

vars <- c(Tot_Pop = "B02001_001", Tot_White = "B02001_002", Tot_Black = "B02001_003",
          Tot_AmInd = "B02001_004", Tot_Asian = "B02001_005", Tot_NaHaw = "B02001_006",
          Tot_HispLat = "B03002_012",
          Tot_Other = "B02001_007", Tot_Two = "B02001_008")

Census.Pop.dat <- tidycensus::get_acs(geography = "block group",
                                      variables = vars,
                                      state = "WI",
                                      county = "Dane",
                                      year = 2019,
                                      survey = "acs5",
                                      geometry = T, #this adds the geometry SF information
                                      output= "wide")

Census.Pop.dat <- Census.Pop.dat %>%
  filter(!(GEOID == '550259917020'|GEOID == '550259917030'))

Census.Pop.dat <- dplyr::select(Census.Pop.dat, -c(4,6,8,10,12,14,16,18,20))

#### Take each pop for each race and divide by 7.5 to reduce the population density
Census.Pop.dat$Tot_WhiteE <- round(Census.Pop.dat$Tot_WhiteE/20,0)
Census.Pop.dat$Tot_BlackE <- round(Census.Pop.dat$Tot_BlackE/20,0)
Census.Pop.dat$Tot_AmIndE <-  round(Census.Pop.dat$Tot_AmIndE/20,0)
Census.Pop.dat$Tot_AsianE <- round(Census.Pop.dat$Tot_AsianE/20,0)
Census.Pop.dat$Tot_NaHawE <- round(Census.Pop.dat$Tot_NaHawE/20,0)
Census.Pop.dat$Tot_HispLatE <- round(Census.Pop.dat$Tot_HispLatE/20,0)
Census.Pop.dat$Tot_OtherE <- round(Census.Pop.dat$Tot_OtherE/20,0)
Census.Pop.dat$Tot_TwoE <- round(Census.Pop.dat$Tot_TwoE/20,0)

#### Now will use ST sample to simualte 
Pop_White <- st_sample(Census.Pop.dat,Census.Pop.dat$Tot_WhiteE,by_polygon=T)
Pop_Black <- st_sample(Census.Pop.dat,Census.Pop.dat$Tot_BlackE,by_polygon=T)
Pop_AmInd <- st_sample(Census.Pop.dat,Census.Pop.dat$Tot_AmIndE,by_polygon=T)
Pop_Asian <- st_sample(Census.Pop.dat,Census.Pop.dat$Tot_AsianE,by_polygon=T)
Pop_NaHaw <- st_sample(Census.Pop.dat,Census.Pop.dat$Tot_NaHawE,by_polygon=T)
Pop_HispLat <- st_sample(Census.Pop.dat,Census.Pop.dat$Tot_HispLatE,by_polygon=T)
Pop_Other <- st_sample(Census.Pop.dat,Census.Pop.dat$Tot_OtherE,by_polygon=T)
Pop_Two <- st_sample(Census.Pop.dat,Census.Pop.dat$Tot_TwoE,by_polygon=T)

# Now using GGPLOT can map everything at once
# Likely an idea way to 
ggplot() + 
  geom_sf(data = Census.Pop.dat,alpha=.2) +
  theme_classic() + 
  labs(title = "Racial Dot Map of Dane County by Block Group, 2015-2019 ACS") +
  scale_x_continuous(expand = c(0, 0)) + 
  scale_y_continuous(expand = c(0, 0)) + 
  theme(axis.line = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(size = 16, face = "bold")) + 
  geom_sf(data = Pop_White, color = "#73B2FF", size = .01, alpha=.3) + 
  geom_sf(data = Pop_Asian, color = "#FF0000", size = .01, alpha=.3) + 
  geom_sf(data = Pop_HispLat, color = "#FFAA00", size = .01, alpha=.3) +
  geom_sf(data = Pop_Black, color = "#55FF00", size = .01, alpha=.3)

# Note can save the above as Pop_Dot_Map_4 <- 
# then use ggsave(Pop_Dot_Map_4, file = './Pop_Dot_Map_4.jpg', width = 10.6, height = 7.1)

