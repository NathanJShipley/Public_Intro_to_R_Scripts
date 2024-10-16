#==============================================================================
#                              Analysis Repository:                           #
#                         Race/Ethnicity and Gender Coder                     #      
#==============================================================================

#===========================================
# Load Libraries                           #
#==========================================-
library(tidyverse)  #has dplyr and ggplot2, etc
library(readxl) # read xlsx
library(wru) # race/ethnicity coder
library(tidycensus) # used to get census geoid
library(gender) # gender coder


#===========================================
# Simulate Data OR LOAD DATA               #
#==========================================-

data("voters")
dat <- voters

#dat.pay <- read.csv("fake.data.csv", header=T)
#dat <- readxl::read_xlsx("fake.data.xlsx")

#===========================================
# Overview and Data Manipulation           #
#==========================================-

# There are two packages used in this code
# WRU is the package to implement the coder for race/ethnicity
# gender is the package to implement the coder for gender


#===========================================
# Predict Race/Ethnicity using Surname/ Last Name  #
#==========================================-

# WRU needs as few pieces of information to run
# First, if using census information to code name, then you need to have COUNTY and TRACT GEOID information in the data
# Second, you also need to have a column named "surname"
# Also note that a census key will need to be provided - this can be obtained using the census website

predicted_race.dat.v1 <- predict_race(voter.file = dat, census.geo = "tract",
                              census.key = "7a68fa479e8bcac3209caa8b994d5b438b547eab")

predicted_race.dat.v1

# Note that for complex data, census geo information can be "pre-downloaded"
# Can enter much information
census.download <- get_census_data(key = "7a68fa479e8bcac3209caa8b994d5b438b547eab", state = c("NY", "NJ", "DC"), 
                                   age = F, sex = F, census.geo = "tract")    


predicted_race.dat.v2 <- predict_race(voter.file = dat, census.geo = "tract",
                                      census.key = "7a68fa479e8bcac3209caa8b994d5b438b547eab",
                                      census.data = census.download)

# Lastly, can use just surname information without tract information by adding (surname.only = T)
predicted_race.dat.v3 <- predict_race(voter.file = dat, census.geo = "tract",
                                      census.key = "7a68fa479e8bcac3209caa8b994d5b438b547eab",
                                      census.data = census.download,
                                      surname.only = T)

# From here, the data can be joined to an existing data set
# Seems that any prob greater than 50% will be used to select race/ethnicity 


#===========================================
# Predict Gender using First Name  #
#==========================================-

# Gender package predicts gender 
# The package requires a vector of names
# Can also use a different method

# Method #1 = Social Security Data
Gender_1 <- gender(dat.name$name, method = "ssa")

# Method #2 = Social Security Data with specified date range
Gender_2 <- gender(dat$name, years = c(1932,2012), method = "ssa")

# Method #3 = integrated public microdata
Gender_3 <- gender(dat$name, years = c(1932,2012), method = "ipums")

# Method #4 = Also uses a probablistic estimate
Gender_4 <- gender(dat$name, method = "kantrowitz")

# Method #5 = Genderize (uses an API to profile across major social networks, MAX 1000 names a day!!!!!)
Gender_5 <- gender(dat$name, method = "genderize")

# Overall genderize seems to be best
# Gives prob for both male and female, also auto codes missing as NA














