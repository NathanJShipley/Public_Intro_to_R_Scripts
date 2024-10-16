#==============================================================================
#                              Analysis Repository:                           #
#                                 Meta Analysis                               #      
#==============================================================================

#===========================================
# Load Libraries                           #
#==========================================-

library(metafor) # Primary meta 
library(psych) #has nice describe function for descriptive info
#library(robumeta)
library(tidyverse) #has dplyr and ggplot2
library(ggpubr) #has some more GG plots

#library(ggplot2)
#library(jtools)
#library(matrixcalc)
#library(lqmm)
#library(multcomp)
#library(psych)


#library(olsrr) #Some good regression functions and plotting tools
#library(jtools) #summ function good to get regression output
#library(ggstance) #Provides functions that Jtools uses

#library(car) #Durbin-Watson test
#library(MASS) #Shapiro-Wilk test
#library(lmtest) #Breusch-Pagan test
#library(pwr) #power analysis
#library(Metrics) # used to calculate RMSE and MAE
#library(caret) # used for cross validation 
#library(mvnormtest) #normality
#library(corrplot) #nice cor plot

#===========================================
# Simulate Data OR LOAD DATA               #
#==========================================-

set.seed(101)

data("dat.molloy2014")
dat <- dat.molloy2014

#dat <- read.csv("data.csv", header=T)
#dat <- readxl::read_xlsx("fake.data.xlsx")


#===========================================
# Overview                                 #
#==========================================-

# Meta analysis is a tool to examine the variability in effect sizes
# It seeks to measure a true effect size for some effect using a collection of studies
# Meta analysis itself is focused on the size of the effect, not sig
# Seeks to estimate the TRUE effect in the population by using effects from other studies
# This also allows for moderator analysis, seeks to explain heterogeneity
# Heterogenity is the bane of meta-analysis, want little OR want to exaplin it!
# Too much heteroheneity indicates that the studies may not be measuring the "same" thing

# The steps for meta-analysis are fairly simple
# Start with some measure of effect, common are MEAN DIFFERENCES
# Can also use odd ratios and correlations
# The next step is to transform the effect (correlation to fisher's z) for more normality
# Then the effect is weighed by the sample size (inverse )
# Model building is then farily simple, followed by more complex assessment of rigor

#===========================================
# Assess Descriptives                      #
#==========================================-

psych::describe(dat)

# Visualizations for distributions
hist(dat$ri)
ggdensity(dat$ri)
boxplot(dat$ri)
ggqqplot(dat$ri)


#===========================================
# Preping Data and Building Model          #
#==========================================-

# Convert correlation to fisher's and create sampling variances 
dat <- escalc(measure="ZCOR", ri=ri, ni=ni, data=dat) #convert r to z metric
# Note sampling variacnes are inverses, related to the sample size


# Build model
# Meta-analysis models can be fixed and random effects models
# Most basic is fixed effect, but assumes no between study heterogeneity
# Implication of fixed is that the treatment effects only vary because of chance differences
# If infinity sample size, there would be no differences
# I-squared is the metric used to evaluate heterogeneity and used to determine if a fixed or random effect should be used

# RANDOM effect meta assume observed effects vary across studies 
# That is, the effects differ because of real differences in treatment and chance
# I-squared = variability in effect to due heterogeneity, the rest due to chance

# Fit both a fixed and random effects model
fixed.cor <- rma(yi, vi, method="FE", data=dat, slab=paste(authors))
res.cor <- rma(yi, vi, method="REML", data=dat, slab=paste(authors))
fixed.cor
res.cor
# Can see high I-squared and test of heterogeneity is sig, suggesting we should use a Random effects model


#===========================================
# Understanding Model Output               #
#==========================================-

# Look at model output
summary.rma(res.cor)
# I-squared = % of variability attributed to between study differences
# Q test for hetero = if there is sig variability
# estimate = fisher's Z, indicates if sig and CI upper and lower bounds
# Note, there is heterogeneity present, which suggests a moderator analysis may be needed

# NOTE that the above estimates are FISHER'Z
# Convert to correlations
predict.rma(res.cor, digits=3, transf=transf.ztor)
# See the meta-correlation is .149

# Lets plot the effects
forest(res.cor, xlim=c(-.9,1.2), xlab = "Fisher's Z", 
       at=c(-.4,-.2,0,.2,.4,.6,.8), 
       digits=c(2,1), cex=1,
       mlab="Random Effects Model")
text(-.52,18, "Forest plot of effect sizes Studies", cex=1.2)


#===========================================
# Evaluate Model Fit                       #
#==========================================-

# First, check to see if the model was correctly profiled
profile(res.cor)
# The model should peak and model full slope, should converge 
# Peak at the corresponding REML estimate

# Also check for outliers and other extreme effects 
Cooks.Cor <- cooks.distance(res.cor, progbar=TRUE)
plot(Cooks.Cor, xlab = "Study #", ylab = "Cook's Distance", main = "Cook's d of model effects")
# Check for outliers

# normal QQ plots
qqnorm(res)

# Also look at effects each study has on various metrics
cor.inf <- influence(res.cor)
plot(cor.inf)

#===========================================
# Publication Bias and other Indexes       #
#==========================================-

# A main issue in meta-analysis is publication bias
# Or the increased likelihood that studies with higher effects will get published more compared to smaller effects

# A common test of publication bias is a funnel plot
# Tests funnel plot asymmetry, given the observed effects
funnel(res.cor, xlab = "Fisher's Z")
regtest(res.cor)
# Can formally test asymmetry 
baujat(res.cor)
# look more at studies overly contributing to heterogeneity 

# IF there is some evidence of asymmetry based on funnel and regtest, then can use trim and fill
# Adjusts the model to "trim" effects
# Again, keep in mind the assumption is there are more studies with stronger effects
# assumption is fewer studies with small effects, so a trim and fill make make a more accurate estimate
trimfill(res.cor)
TAF <- trimfill(res.cor)
funnel(res.cor)
# A new funnel plot

# Another way to assess publication bias is through Fail-safe N
# these are a series of techniques that estimate the number of studies that would have to be published to produce null results 
fsn (yi, vi, data=dat, type="Rosenthal") # of null added to reduce alpha to .05
fsn (yi, vi, data=dat, type="Orwin") # of null to reduce average effect of half size
fsn (yi, vi, data=dat, target=.1, type="Orwin") # of null to reduce effect to a target
fsn (yi, vi, data=dat, type="Rosenberg") # of null to reduce weighted effect to .05
# While interesting, the usefulness of these metrics has been debated
# Some people demand them, but really these metrics are not useful 


# Lastly, another way to assess the model is through a 
# LEAVE ON OUT analysis
# This acts much like a sensativty analysis
# IF a study was removed, how much would the effects change? 
leave1out(res.cor)
inf <- influence(res.cor)
plot(inf)
# Looks like the estimate ranges from .1353 to .1647
# also helpful to ID potentially influential studies 

#===========================================
# Moderation Analyses       #
#==========================================-

# Another series of tests that can be done with meta-analysis is MODERATION ANALYSIS
# This seeks to understand how features of studies may produce different effects
# Helpful for REMOVING heterogeneity but also learn more about the effects
# The choice of moderator variables should be done a prior before data collection and theory driven
# Here, I'll just test each other variable in the data

res.cor.mod.controls <- rma(yi, vi, mods=~controls, method="REML", data=dat, slab=paste(authors))
res.cor.mod.design <- rma(yi, vi, mods=~design, method="REML", data=dat, slab=paste(authors))
res.cor.mod.a_measure <- rma(yi, vi, mods=~a_measure, method="REML", data=dat, slab=paste(authors))
res.cor.mod.c_measure <- rma(yi, vi, mods=~c_measure, method="REML", data=dat, slab=paste(authors))
res.cor.mod.meanage <- rma(yi, vi, mods=~meanage, method="REML", data=dat, slab=paste(authors))
res.cor.mod.quality <- rma(yi, vi, mods=~quality, method="REML", data=dat, slab=paste(authors))

# Look at output
# Test of moderators acts as an omnibus test
# can look at each effect as well
# Treat like a regression output! This is more or less a META-REGRESSION
summary.rma(res.cor.mod.controls) #sig
summary.rma(res.cor.mod.design)
summary.rma(res.cor.mod.a_measure)
summary.rma(res.cor.mod.c_measure)
summary.rma(res.cor.mod.meanage)
summary.rma(res.cor.mod.quality)

# look at overall effects
forest(res.cor.mod.controls)

# So can conclude that studies with no controls had no controls has stronger effects than studies with mutltiple 




