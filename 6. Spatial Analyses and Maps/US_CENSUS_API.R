#==============================================================================
#                              Analysis Repository:                           #
#                                 US Census API                               #      
#==============================================================================

#===========================================
# Load Libraries                           #
#==========================================-
library(tidycensus) #Has all census api info
library(tidyverse) #dplyr, ggplot2, tidyr, readr

#library(sf) #Used in much spatial and mapping analyses
#library(tmap) # used in tm mapps
#library(viridis) #Has some nice colors
#library(mapview) #interactive maps
#library(rgdal)


#===========================================
# overview                                 #
#==========================================-

# Will be using known variables to pull data from US census

# help using tidy census and analyzing census data in R
# https://walker-data.com/tidycensus/articles/basic-usage.html
# https://walker-data.com/isds-webinar/#1
# https://walker-data.com/census-r/index.html

#===========================================
# Load Census API Key                      #
#==========================================-

# Need to first enter a cesus api key 
# Only need to do once, can request https://api.census.gov/data/key_signup.html
# census_api_key("7a68fa479e8bcac3209caa8b994d5b438b547eab",install = T)


#===========================================
# Model Load variables of interest         #
#==========================================-

vars <- c(Tot_Pop = "B02001_001", Tot_White = "B02001_002", Tot_Black = "B02001_003",
          Tot_AmInd = "B02001_004", Tot_Asian = "B02001_005", Tot_NaHaw = "B02001_006",
          Tot_HispLat = "B03002_012",
          Tot_Other = "B02001_007", Tot_Two = "B02001_008")

#===========================================
# Pull from Census API         #
#==========================================-

# Note you can adjust MANY aspects of this code
# Geography = "block group", "tract", "county"
# state = can be many
# county
# year
# survey
# geometry = adds info for SF and mapping
# NOTE this code also pulls the estimate and margin of error for each! 

?get_acs

Census.dat <- tidycensus::get_acs(geography = "block group",
                                  variables = vars,
                                  state = "WI",
                                  county = "Dane",
                                  year = 2019,
                                  survey = "acs5",
                                  geometry = T, #this adds the geometry SF information
                                  output= "wide")

#===========================================
# Data Clean Up                            #
#==========================================-

# Remove the two water block groups if desired
Census.dat <- Census.dat %>%
  filter(!(GEOID == '550259917020'|GEOID == '550259917030'))

# Will also need to calculate MOE and Estimates if combined data is wanted
# E.g., if you want # of adults above a certian age, will to add and adjust the estimate and MOE
# can use code like moe_prop

# ALSO note that this code uses NA for missing data but there are also NaN if variables are created when there is actually ZERO!
# If save file as shape, it will not code this information well
# May decide to code true missing data as -999
# Also make sure to code NaN as 0

# e.g., 
Census.dat <- Census.dat %>% 
  mutate(Tot_PopE = replace(Tot_PopE, Tot_PopE == "NaN", 0))

Census.dat <- Census.dat %>% 
  mutate(Tot_PopE = replace(Tot_PopE, is.na(Tot_PopE), -999))

# Don't forget to remove data that is not needed 
Census.dat <- dplyr::select(Census.dat, -c())

#===========================================
# Export as needed                         #
#==========================================-

#write.csv(Census.dat, "Census_Data.csv")

#sf::st_write(Census.dat,"Census_Shape/Census.shp")

