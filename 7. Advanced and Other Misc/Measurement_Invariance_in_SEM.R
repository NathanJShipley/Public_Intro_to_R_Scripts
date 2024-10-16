#==============================================================================
#                              Analysis Repository:                           #
#                          Measurement Invariance in SEM                      #      
#==============================================================================

#===========================================
# Load Libraries                           
#==========================================-
library(psych) #has nice describe function for descriptive info
library(tidyverse) #has dplyr and ggplot2
library(lavaan) #CFA and dataset
library(ggpubr) #has some more GG plots
library(corrplot) #nice cor plot
library(mvnormtest) #normality
library(semTools) #helpful tools for CFA analysis, such as reliability function

#===========================================
# Simulate Data OR LOAD DATA               
#==========================================-
set.seed(101)

dat <- read.csv("data/kasky_dat.csv", header=T)
#dat <- readxl::read_xlsx("fake.data.xlsx")


#===========================================
# Overview                                 #
#==========================================-

# Measurement Invariance
# Important concept to understand differences between groups
# If want to say, look at how men and women differ in something
# Say, the relationship between pride and behavior is stronger in men
# If we want to check how the regression relationship differs, 
# we first need to establish that men and women don't differ in other aspects of the model
# Like, how the variables area measured, differences in loadings, means, etc. 
# Measurement is done in multiple steps 

# 1.) First, a normal CFA/SEM model should be fit! 
# 2.) Second, configuration invariacne is modeled, structrue, does thegeneral model work with multiple groups
# 3.) Third, Metric invariance, are there equal factor loadings between groups
# 4.) Forth, Scalar invariance, are the intercepts equal between groups
# 5.) NOW more complex models can look at residual invariance, but this is not necessary for further analysis
# 6.) So after establishing scalar invariance, differenes in intercepts and regressions can be compared


#===========================================
# Assess Descriptives                      #
#==========================================-

# Mean, SD, Median, Min, Max, Skewness, and Kurtosis for y
psych::describe(dplyr::select(dat,14:18)) # look at descriptives for one construct
psych::describe(dplyr::select(dat,8:13))
# Check for no extreme skewness or kurtosis, if so, may have to perform other analyses

# Visualizations for distributions
boxplot(dplyr::select(dat,14:18))
boxplot(dplyr::select(dat,8:13))

# Look at density for all variables, just one shown
ggdensity(dat$AP.a)
ggdensity(dat$PEB.d)

# Look at QQ plots for all variables, just one shown
ggqqplot(dat$AP.a)
ggqqplot(dat$PEB.d)

# Assess Multivariate Outliers
psych::outlier(dplyr::select(dat,8:18))

# Preliminary Exploration of Associations
pairs.panels(dplyr::select(dat,14:18), scale=TRUE) # 
pairs.panels(dplyr::select(dat,8:13), scale=TRUE) # 

corrplot(cor(dplyr::select(dat,8:18), use="complete.obs"), order = "hclust", tl.col='black', tl.cex=.75) 


#===========================================
# CFA & SEM                                #
#==========================================-

Measurement.model <- '
# Anticipated Emotions
Pride =~ AP.a + AP.b + AP.c + AP.d + AP.e

# Pro-environmental Behavior
PEB =~ PEB.d + PEB.e + PEB.f + PEB.g + PEB.h + PEB.i

# Residual
PEB.g ~~ PEB.h
PEB.d ~~ PEB.e
'
Measurement.fit <- cfa(Measurement.model, data=dat, missing = 'fiml')
summary(Measurement.fit, standardized = TRUE, fit.measures = TRUE, rsquare=TRUE)
modindices(Measurement.fit, sort. = T, power = T)

Structural.model <- '
# Regressions
PEB ~ Pride

# Anticipated Emotions
Pride =~ AP.a + AP.b + AP.c + AP.d + AP.e

# Pro-environmental Behavior
PEB =~ PEB.d + PEB.e + PEB.f + PEB.g + PEB.h + PEB.i

# Residual
PEB.g ~~ PEB.h
PEB.d ~~ PEB.e
'
Structural.fit <- cfa(Structural.model, data=dat, missing = 'fiml')
summary(Structural.fit, standardized = TRUE, fit.measures = TRUE, rsquare=TRUE)


#===========================================
# Measurement Invariance                   #
#==========================================-

#====================== =
# Step A: Configural Invariance (Structure) # #
#====================== =

Groups.Configural.fit <- sem(Structural.model, data=dat, missing = 'fiml', group = "Gen")
summary(Groups.Configural.fit, standardized = TRUE, fit.measures = TRUE) #
# Check and make sure fit is good, appears to be! 


#====================== =
# Step B: Metric Invariance (Factor Loadings) # #
#====================== =

Groups.Metric.fit <- sem(Structural.model, data=dat, missing = 'fiml', group = "Gen", group.equal = c("loadings"))
summary(Groups.Metric.fit, standardized = TRUE, fit.measures = TRUE)
# Check fit, then compare this model to Step A
anova(Groups.Configural.fit,Groups.Metric.fit) #
# If sig, then say the fit changes, CAN'T establish, seek alternatives! 
# If NOT sig, then say holding factor loadings does not change fit, supporting metric invariance 
# If not sig, then move to step c
 

#====================== =
# Step C: Scalar Invariance (Intercepts) # #
#====================== =

Groups.Scalar.fit <- sem(Structural.model, data=dat, missing = 'fiml', group = "Gen", group.equal = c("loadings","intercepts"))
summary(Groups.Scalar.fit, standardized = TRUE, fit.measures = TRUE)
# Check fit, then compare this model to Step A
anova(Groups.Metric.fit,Groups.Scalar.fit) #
# If sig, then say the fit changes, CAN'T establish, seek alternatives! 
# If NOT sig, then say holding factor loadings does not change fit, supporting Scalar 
# If not sig, then move to step d


#====================== =
# Step D: Comparing Structural Parameters # #
#====================== =

Groups.Regression.fit <- sem(Structural.model, data=dat, missing = 'fiml', group = "Gen", group.equal = c("loadings","intercepts","regressions"))
summary(Groups.Regression.fit, standardized = TRUE, fit.measures = TRUE)
# Check fit, then compare this model to Step A
anova(Groups.Scalar.fit,Groups.Regression.fit) #
# If not sig, then there are no differences
# If sig, then suggests regression is different between groups
# If sig, then move to Step E to look at the differences


#====================== =
# Step E: Looking into group differences # #
#====================== =

# first, re-specify model to include variables needed to compare
# Add a1 and a2 as place holders in the model, regression coef for group 1 and 2 
# just like mediation, can use bootstrap to formally compare differences

Structural.model.test <- '
# Regressions
PEB ~ c(a1,a2)*Pride

# Anticipated Emotions
Pride =~ AP.a + AP.b + AP.c + AP.d + AP.e

# Pro-environmental Behavior
PEB =~ PEB.d + PEB.e + PEB.f + PEB.g + PEB.h + PEB.i

# Residual
PEB.g ~~ PEB.h
PEB.d ~~ PEB.e

# Differences
Diff := (a1-a2)
'
# Remember to constrain loadings and intercepts 
Structural.fit.test <- cfa(Structural.model.test, data=dat, missing = 'fiml', group = "Gen", group.equal = c("loadings","intercepts"))
summary(Structural.fit.test, standardized = TRUE, fit.measures = TRUE, rsquare=TRUE)
# Look at summary, but should also bootstrap

Structural.fit.boot <- cfa(Structural.model.test, data=dat, missing = 'fiml', group = "Gen", group.equal = c("loadings","intercepts"), se = "bootstrap", bootstrap = 1000)
summary(Structural.fit.boot, standardized = TRUE, fit.measures = TRUE, rsquare=TRUE)
