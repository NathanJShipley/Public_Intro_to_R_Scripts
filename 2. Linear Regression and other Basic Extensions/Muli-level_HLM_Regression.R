#==============================================================================
#                              Analysis Repository:                           #
#                               Linear Regression                             #      
#==============================================================================

#===========================================
# Load Libraries                           #
#==========================================-

library(psych) #has nice describe function for descriptive info
library(tidyverse) #has dplyr and ggplot2
library(multilevel) # also loads MASS and nlme
library(corrplot) #nice cor plot
library(ggpubr) #has some more GG plots
library(lme4) #tool used for multilevel models
library(jtools) #summ function good to get regression outputs
library(ggstance) #Provides functions that Jtools uses

#library(olsrr) #Some good regression functions and plotting tools
#library(car) #Durbin-Watson test
#library(MASS) #Shapiro-Wilk test
#library(lmtest) #Breusch-Pagan test
#library(pwr) #power analysis
#library(Metrics) # used to calculate RMSE and MAE
#library(caret) # used for cross validation 
#library(mvnormtest) #normality


#===========================================
# Simulate Data OR LOAD DATA               #
#==========================================-
set.seed(101)

data("klein2000")
dat <- klein2000
dat$GRPID <- as.factor(dat$GRPID)

#dat <- read.csv("data.csv", header=T)
#dat <- readxl::read_xlsx("fake.data.xlsx")


#===========================================
# Overview                                 #
#==========================================-

# If running multi-level regression, going to assume many aspects of regression have been used
# If want help running more code for regression, see linear regression
# This code is not exhaustive and mostly but focused on multi-level
# Again, for data processing, assumptions, etc. see linear regression

# Some overview of MLM, HLM, etc
# Fixed effects - effects that don't vary
# Random effects - effects that do vary

#===========================================
# Assess Descriptives                      #
#==========================================-

# Mean, SD, Median, Min, Max, Skewness, and Kurtosis for y
psych::describe(dat)
# Check for no extreme skewness or kurtosis, if so, may have to perform other analyses

# Visualizations for distributions
hist(dat$JOBSAT)
ggdensity(dat$JOBSAT)
boxplot(dat$JOBSAT)
ggqqplot(dat$JOBSAT)

# Preliminary Exploration of Associations
pairs.panels(dat, scale=TRUE)
corrplot(cor(dat, use="complete.obs"), order = "hclust", tl.col='black', tl.cex=.75) 

# Scatter plot of two variables
ggplot(dat,aes(PAY,JOBSAT)) +
  geom_point() + 
  geom_smooth(method="lm", fullrange=T)

# Lets look at this relationship within GROUPS! 
# Can see how relationship may VARY across groups
ggplot(dat,aes(PAY,JOBSAT)) +
  geom_point() + 
  geom_smooth(method="lm", fullrange=T) +
  facet_wrap( ~ GRPID) 

ggplot(dat,aes(POSAFF,JOBSAT)) +
  geom_point() + 
  geom_smooth(method="lm", fullrange=T) +
  facet_wrap( ~ GRPID) 

ggplot(dat,aes(WLOAD,JOBSAT)) +
  geom_point() + 
  geom_smooth(method="lm", fullrange=T) +
  facet_wrap( ~ GRPID) 

ggplot(dat,aes(NEGLEAD,JOBSAT)) +
  geom_point() + 
  geom_smooth(method="lm", fullrange=T) +
  facet_wrap( ~ GRPID) 

ggplot(dat,aes(x=NEGLEAD,y=JOBSAT,color=GRPID)) +
  geom_point(size = .5) + 
  geom_smooth(method ="lm", se=F, aes(group=GRPID)) +
  theme_classic() + 
  theme(legend.position = "none")


#===========================================
# Build Linear Model & Get Output                 #
#==========================================-

# May be interested in first fitting a NORMAL OSL LINEAR model
Reg.fit <- lm(JOBSAT ~ PAY + POSAFF + WLOAD + NEGLEAD, data=dat)
# Predict job satisfaction using pay, positve affect, negative leadership, and workload

jtools::summ(Reg.fit, part.corr=TRUE, vifs=TRUE, digits=3)
# Check relationships and fit 


#===========================================
# Build Multi-level Model & Get Output     #
#==========================================-

# With multilevel modeling, can model both random intercept and random slope models
# Random intercept = relationship between x and y is the same, but the actual predicted values may be higher or lower
# Random slope = relationship between x and y is not the same
# Again, both in reference to differences across groups 

#====================== =
# # Null Model        # #
#====================== =

# Can look 
Random.Null.Fit <- lmer(JOBSAT ~ 1 + (1 | GRPID), 
                              data=dat)

jtools::summ(Random.Null.Fit, digits=3)
summary(Random.Null.Fit)

# NOTE ICC which is the variance of intercept / (var intercept + var of residual)
0.6835/(0.6835+5.4741)
# ICC = .111
# Correlation of job sat within same group is .111
# Also suggests that .111 variance as at the job level, while the remaining is within jobs
# ICC is variation at LEVEL 2

#====================== =
# # Random Intercepts # #
#====================== =

# syntax for random intercept is (1 | groupping variable)
Random.Intercepts.Fit <- lmer(JOBSAT ~ PAY + POSAFF + WLOAD + NEGLEAD + (1 | GRPID), 
                              data=dat)

jtools::summ(Random.Intercepts.Fit, digits=3)
# jtools works with multilevel
# Interpret the est and other fit just like OLS 
# These are the FIXED effects
# NOTE pseudo R-2 fixed = level 1 R2 - explains % variance among individals!!!
# 

# Can compare fit to the null model
anova(Random.Null.Fit, Random.Intercepts.Fit)
# Should be sig if the fixed effects are important

# Get random effect details 
summary(Random.Intercepts.Fit)
# RANDOM EFFECTS INTERPRETATION
# how much Jobsat varies between and within groups! 

# Random effects GRPID = .5908 
# This is how much variation there is BETWEEN GROUPS

# Random effects residual = 3.966
# This is how much variation there is WITHIN GROUPS

# ICC (Inter Class Coefficient)
# Ratio of total variation
# Here is .130, so 13% of variation in jobsat exists between groups
# While the remaining 87% exists within groups 


#====================== =
# # Random Slopes     # #
#====================== =

# syntax for random intercept is (predictor | groupping variable)
Random.Slopes.Fit <- lmer(JOBSAT ~ PAY + POSAFF + WLOAD + NEGLEAD + (NEGLEAD | GRPID), 
                              data=dat)

jtools::summ(Random.Intercepts.Fit, digits=3)
# jtools works with multilevel

# Can compare fit between random intercept and slope
anova(Random.Intercepts.Fit,Random.Slopes.Fit)
# Should be mentioned that modeling random slope is also part theoretical
# Not just data driven, even if improve model fit, consider if worth adding 

# Get random effect details 
summary(Random.Slopes.Fit)

# Now have random effects for intercept, slope, and residual


#====================== =
# # Mean Centering    # #
#====================== =

# A note on mean centering
# Mean centering in MLM is important! 
# Can group and grand mean center
# e.g., center all scores to the grand mean
# OR center scores to GROUP mean
# Most common to center to grand, but can be helpful at times/certain contexts to center to group

# Grand mean - controls for variance at level 1 prior to level 2
# Group mean - does not estimate incremental models, does not control for level 1 variance
#       Further, group mean separately estimates group regression and between group regression 

