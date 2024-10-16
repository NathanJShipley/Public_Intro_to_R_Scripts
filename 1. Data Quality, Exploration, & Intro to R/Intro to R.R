#==============================================================================
#                              Analysis Repository:                           #
#                                  Intro to R                                 #      
#==============================================================================

# I am assuming anyone reading this has zero knowledge of R
# Note that anything starting with a # (octothorpe, pound sign) is a comment and R won't run what follows
# Also NOTE that to run a line of code, you can press "Ctrl" and "Enter" key at the same time to run each line
# You can also highlight multiple lines of code and press "Ctrl" and "Enter" key to run multiple lines

# R works by using libraries, which provides the algorithms for various functions
# Notable about R is that you must specify which libraries to use
# You tell R to use a library using the "library" function
# NOTE that if you don't have a library installed, you can use the function "install.packages("example_library_name")" to install
# you only need to install a library once! But you have to load the library each time!
#install.packages("psych")

#### libraries
library(psych) # For Descriptive and other useful functions 
library(tidyverse) #has dplyr and ggplot2, dplyr has select function

# We first need to read in our data
# the function "read.csv" will open csv files
# R works best by using the "<-" symbol, which is taking a function or command and binding the result to an object
# for the keyboard shortcut enthusiast, you can create the <- by pressing "Alt" and "-" (minus) key at the same time
# So the command below is loading the Kasky Data CSV and ASSIGNING it to the object name "Kasky.dat"
Kasky.dat <- (read.csv("Kasky_Data_V1.csv", header=TRUE))
# We have now assigned the read-in table to an object

# you can view the object by running the line of code below
Kasky.dat

# This will likely look like a mess, so instead you can use the function "View" to see the table of data
# Note that R is generally flexible, but the "View" function is case sensitive. I would suggest to be case sensitive in all code to avoid problems
View(Kasky.dat)

# We are going to use anticipated Pride and Guilt for my example analysis
# The scale is 10 items measuring two latent constructs, Anticipated Pride and Guilt
# First we are going to get some basic descriptive information

#===========================================
# Example of running descriptive stats     #
#==========================================-

# the 'psych' package has a nice function called "describe"
# I also use the 'dplyr' package, which has a function called "select" which allows you to specify a range of variables based on the column number. 
# I prefer using dplyr, but others will have different preferences on how to chose data (I will show a few examples below)
# In the examples below, dplyr is overkill, but with more complex analyses dplyr can be a lifesaver
# In this case, I know that my pride and guilt items are columns 1 through 10
# so the code below will provide descriptive information for columns 1 through 10, which are the emotion items

describe(select(Kasky.dat, 1:10))
# Will provide means, SD, min, max, range, skewness, kurtosis, and SE for raw data
# AP.a-e are the five items for anticipated pride
# AG.a-e are the five items for anticipated guilt

describe(Kasky.dat[,1:10])
# note this is the same as above, but rather than using dplyr, we are just using brackets to select columns 1:10

# You can also specify what variable you want from the data set
# Any time a $ follows a data frame or model, you can further specify details you want to
describe(Kasky.dat$AP.a)
describe(Kasky.dat$AP.b)
# These will both list descriptive information for items A and B from the anticipated pride scale

# pairs.panels function is found in the 'psych package'
# This creates a nice figure which shows
# 1.) The diagonal shows a histogram and distributions of each item
# 2.) The above diagonal shows the raw correlation table between items
# 3.) The below diagonal shows the scatter plot of the items 
pairs.panels(select(Kasky.dat, 1:10),scale=TRUE) 
# adding the "scale" argument changes the font of correlation to reflect size
