#==============================================================================
#                              Analysis Repository:                           #
#                     Running R Code from the Command Line                    #      
#==============================================================================

#===========================================
# Load Libraries                           #
#==========================================-

# Some code to check if packages installed, if not, then load
if (!require(dplyr)) install.packages('dplyr')
if (!require(rgdal)) install.packages('rgdal')
if (!require(sf)) install.packages('sf')
if (!require(readxl)) install.packages('readxl')

library(dplyr)
library(rgdal)
library(sf)
library(readxl)


#===========================================
# Overview                                 #
#==========================================-

# Will need to make sure that R is added to the PC user's main path
# system environment varibales

# Then also create a helpful README.text

# In this case, this code will run by doing the following 
# To run the code above, simply paste the text below into the command line
# *NOTE: Pull up the command line by typing CMD into the start menu search bar
# *NOTE: If the command "Rscript" does not run, then R needs to be added to your path
# *NOTE: The code will need TWO arguments, which are the file locations
#The first arg will be the input file, the second will be the output file

#Rscript "\\Fps4\data4\Fdroot\CARES\Data\CARES Spatial Merge\CARES_MPD_DISTRICT_CODER.R" "\\Fps4\data4\Fdroot\CARES\Data\NFIRS_Master.xlsx" "\\Fps4\data4\Fdroot\CARES\Data\NFIRS_Master_Upload_PowerBI.csv"

#Can change the two arguments to input and output different files

#===========================================
# Import data                              #
#==========================================-

# Tell R to load arg
args <-  commandArgs(trailingOnly=TRUE)

# test if there is at least one argument: if not, return an error
if (length(args)!=2) {
  stop("Need to supply two arguments", call.=FALSE)
} else{
  inputfile <- args[1]
  outputfile <- args[2]
}


# Can use this below to make the code user friendly 
input.dat <- read_xlsx(inputfile)

#===========================================
# Data Processing                      #
#==========================================-

# Do what ever is needed on the data

output.dat <- input.dat

#===========================================
# Save created file                     #
#==========================================-

write.csv(output.dat, outputfile, row.names = F)


