#==============================================================================
#                              Analysis Repository:                           #
#                       Exploratory Factor Analysis (EFA)                     #      
#==============================================================================

#===========================================
# Load Libraries                           #
#==========================================-
library(psych) #has nice describe function for descriptive info
library(tidyverse) #has dplyr and ggplot2
library(lavaan) #CFA and dataset
library(ggpubr) #has some more GG plots
library(prcomp) #PCA package
library(factoextra) #visualize pca


#library(nFactors) #parallel analysis
#library(GPArotation) #Used in factor creation
#library(mvnormtest) #normality
#library(corrplot) #nice cor plot

#===========================================
# Simulate Data OR LOAD DATA               #
#==========================================-
set.seed(101)

data("HolzingerSwineford1939")
dat <- HolzingerSwineford1939

#dat <- read.csv("fake.data.csv", header=T)
#dat <- readxl::read_xlsx("fake.data.xlsx")


#===========================================
# Assess Descriptives                      #
#==========================================-

# Mean, SD, Median, Min, Max, Skewness, and Kurtosis for y
psych::describe(dplyr::select(dat,7:15))
# Check for no extreme skewness or kurtosis, if so, may have to perform other analyses

# Visualizations for distributions
boxplot(dplyr::select(dat,7:15))

# Look at density for all variables, just one shown
ggdensity(dat$x1)

# Look at QQ plots for all variables, just one shown
ggqqplot(dat$x1)

# Preliminary Exploration of Associations
pairs.panels(dplyr::select(dat,7:15), scale=TRUE)
corrplot(cor(dplyr::select(dat,7:15), use="complete.obs"), order = "hclust", tl.col='black', tl.cex=.75) 


#===========================================
# Principle Component              #
#==========================================-

PCA.fit <- prcomp(dplyr::select(dat,7:15))

# NOTE can also scale variables
PCA.fit.s <- prcomp(dplyr::select(dat,7:15), scale = T)

summary(PCA.fit)

# Visualize the eigenvalues of the components 
fviz_eig(PCA.fit)
fviz_pca_var(PCA.fit)

PCA.fit$rotation
PCA.fit.s$rotation





