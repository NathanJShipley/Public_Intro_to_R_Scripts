#==============================================================================
#                              Analysis Repository:                           #
#                       Structural Equation Modeling (SEM)                    #      
#==============================================================================

#===========================================
# Load Libraries                           #
#==========================================-
library(psych) #has nice describe function for descriptive info
library(tidyverse) #has dplyr and ggplot2
library(lavaan) #CFA and dataset
library(ggpubr) #has some more GG plots
library(corrplot) #nice cor plot
library(mvnormtest) #normality
library(semTools) #helpful tools for CFA analysis, such as reliability function

#===========================================
# Simulate Data OR LOAD DATA               #
#==========================================-
set.seed(101)

data("PoliticalDemocracy")
dat <- PoliticalDemocracy

#dat <- read.csv("fake.data.csv", header=T)
#dat <- readxl::read_xlsx("fake.data.xlsx")


#===========================================
# Overview                                 #
#==========================================-

# SEM is a large family of analyses that seek to understand COVARIANCE between items and factors
# The analysis can use a variety of metrics, but the traditional form is to measure latent factors
# The SEM process is largely two-fold
# FIRST we start with a CFA model, seeking to understand the MEASUREMENT properties of the items
# SECOND after fitting a CFA, we then look towards understanding the STRUCUTRAL relationships
# We use the 'lavaan' package for CFA and SEM models, which is short for latent variable analysis 

# The basic flow of a CFA is 
# 1.) CFA (IF there are multiple compelx scales)
# 2.) Measurement: How well do our hypo factors fit the data, ITEMS TO LATENT FACTORS
# No directional relationships among factors modeled 
# 3.) Structural: How do our factors relate, regression, path analysis, LATENT FACTORS TO LATENT FACTORS
# Directional relationships among factors ARE modeled 
# Again note, this is structural REGRESSION


#===========================================
# Assess Descriptives                      #
#==========================================-

# Mean, SD, Median, Min, Max, Skewness, and Kurtosis for y
psych::describe(dplyr::select(dat,1:11))
# Check for no extreme skewness or kurtosis, if so, may have to perform other analyses

# Visualizations for distributions
boxplot(dplyr::select(dat,1:11))

# Look at density for all variables, just one shown
ggdensity(dat$y1)

# Look at QQ plots for all variables, just one shown
ggqqplot(dat$y1)

# Assess Multivariate Outliers
psych::outlier(dplyr::select(dat,1:11))

# Assess multivariate normality 
mshapiro.test(t(dplyr::select(dat,1:11)))

# Preliminary Exploration of Associations
pairs.panels(dplyr::select(dat,1:11), scale=TRUE)
corrplot(cor(dplyr::select(dat,1:11), use="complete.obs"), order = "hclust", tl.col='black', tl.cex=.75) 
 
#===========================================
# Confirmatory Factor Analysis: Measurement #
#==========================================-

#====================== =
# Model Fitting       # #
#====================== =

# SEE CFA CODE FOR MORE DETAILS
# Here, skip right to the measurement model because the CFA structure is not compelx, only two scales used

Measurement.model <- ' 
#measurement model
ind60 =~ x1 + x2 + x3
dem60 =~ y1 + y2 + y3 + y4
dem65 =~ y5 + y6 + y7 + y8
'

Measurement.fit <- cfa(Measurement.model, data=dat, missing = 'fiml')
summary(Measurement.fit, standardized = TRUE, fit.measures = TRUE, rsquare=TRUE)
# Evaluate Fit
reliability(Measurement.fit)
# Evaluate factors
modindices(Measurement.fit, sort. = T, power = T)
# Should the model be changed?

# NOTE tht items 1,2,3,4 are the same as 5,6,7,8 measured 5 years later, so we should add covariances

Measurement.model.v2 <- ' 
# measurement model
ind60 =~ x1 + x2 + x3
dem60 =~ y1 + y2 + y3 + y4
dem65 =~ y5 + y6 + y7 + y8

# residual covariances
y1 ~~ y5
y2 ~~ y6
y3 ~~ y7
y4 ~~ y8
'
Measurement.fit.v2 <- cfa(Measurement.model.v2, data=dat, missing = 'fiml')
summary(Measurement.fit.v2, standardized = TRUE, fit.measures = TRUE, rsquare=TRUE)
# Evaluate Fit
reliability(Measurement.fit.v2)


#===========================================
# Structural Equation Modeling             #
#==========================================-

#====================== =
# Model Fitting       # #
#====================== =

Structural.model <- ' 
# measurement model
ind60 =~ x1 + x2 + x3
dem60 =~ y1 + y2 + y3 + y4
dem65 =~ y5 + y6 + y7 + y8

# structural regressions
dem60 ~ ind60
dem65 ~ ind60 + dem60

# residual covariances
y1 ~~ y5
y2 ~~ y6
y3 ~~ y7
y4 ~~ y8
'

# Here, we are saying that IND60 predicted DEM60
# AND that IND60 and DEM60 predicted DEM65
Strucutral.fit <- sem(Structural.model, data=dat, missing = 'fiml')
summary(Strucutral.fit, standardized = TRUE, fit.measures = TRUE, rsquare=TRUE)

# Just as in CFA, other metrics of fit for the SEM model can be assessed
inspect(Strucutral.fit, 'r2') # R-squared
lavInspect(Strucutral.fit, "sampstat")
lavInspect(Strucutral.fit, "implied") # model implied covariances
lavInspect(Strucutral.fit, "residual") # model residuals 
resid(Strucutral.fit, type = 'standardized') #std residuals 
parameterEstimates(Strucutral.fit, standardized = T) # lists out all parameters 

# From here, with a nice fitting model, all that is left is to interpret and write up
# These regression relationships can be further examined through tools as in linear regression

# But for more complicated models, the above model can be adjusted and iterated upon
# Eg, relationships can be set to zero, and models can be compared through ANOVA, other tools
# Other analyses such as measuring indirect/Mediation effects can also be calculated 

# The code above can also be adjusted, such as by pre-multiplying relationships


