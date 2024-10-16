#==============================================================================
#                              Analysis Repository:                           #
#                                Mediation in SEM                             #      
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

dat <- read.csv("data/kasky_dat.csv", header=T)
#dat <- readxl::read_xlsx("fake.data.xlsx")


#===========================================
# Overview                                 #
#==========================================-

# Mediation is a general idea of understanding how one variable influences another by ACTING through a mediator
# Generally speaking, the relationship between X and Y may be mediated by another variable M

# Here, we will look at two mediation effects
# There are many ways to look at INDIRECT effects
# The most advanced is shown here, which involves bootstrapping indirect effects 

# I will quickly go through the CFA and SEM to build the model of interest, then go into the bootstrapping

# The model here that Place Attachment predicted Behavior
# The model also predicts that attachment is partially mediated by anticpated emotions of pride and guilt


#===========================================
# Assess Descriptives                      #
#==========================================-

# Mean, SD, Median, Min, Max, Skewness, and Kurtosis for y
psych::describe(dplyr::select(dat,2:23))
# Check for no extreme skewness or kurtosis, if so, may have to perform other analyses

# Visualizations for distributions
boxplot(dplyr::select(dat,2:23))

# Look at density for all variables, just one shown
ggdensity(dat$PA.a)
ggdensity(dat$AP.a)
ggdensity(dat$PEB.d)

# Look at QQ plots for all variables, just one shown
ggqqplot(dat$PA.a)
ggqqplot(dat$AP.a)
ggqqplot(dat$PEB.d)

# Assess Multivariate Outliers
psych::outlier(dplyr::select(dat,2:23))

# Preliminary Exploration of Associations
pairs.panels(dplyr::select(dat,2:23), scale=TRUE) # TOO MANY, better to look at in smaller pieces
corrplot(cor(dplyr::select(dat,2:23), use="complete.obs"), order = "hclust", tl.col='black', tl.cex=.75) 


#===========================================
# CFA & SEM                                #
#==========================================-

Measurement.model <- '
# Place Attachment
PlaceAttachment =~ PA.a + PA.b + PA.c + PA.d + PA.e + PA.f

# Anticipated Emotions
Pride =~ AP.a + AP.b + AP.c + AP.d + AP.e
Guilt =~ AG.a + AG.b + AG.c + AG.d + AG.e

# Pro-environmental Behavior
PEB =~ PEB.d + PEB.e + PEB.f + PEB.g + PEB.h + PEB.i

# Residual
PA.a ~~ PA.b
PEB.g ~~ PEB.h
AG.a ~~ AG.b
'
Measurement.fit <- cfa(Measurement.model, data=dat, missing = 'fiml')
summary(Measurement.fit, standardized = TRUE, fit.measures = TRUE, rsquare=TRUE)
modindices(Measurement.fit, sort. = T, power = T)

Structural.model <- '
# Regressions
PEB ~ PlaceAttachment + Pride + Guilt
Pride ~ PlaceAttachment
Guilt ~ PlaceAttachment

# Place Attachment
PlaceAttachment =~ PA.a + PA.b + PA.c + PA.d + PA.e + PA.f

# Anticipated Emotions
Pride =~ AP.a + AP.b + AP.c + AP.d + AP.e
Guilt =~ AG.a + AG.b + AG.c + AG.d + AG.e

# Pro-environmental Behavior
PEB =~ PEB.d + PEB.e + PEB.f + PEB.g + PEB.h + PEB.i

# Residual
PA.a ~~ PA.b
PEB.g ~~ PEB.h
AG.a ~~ AG.b
'
Structural.fit <- cfa(Structural.model, data=dat, missing = 'fiml')
summary(Structural.fit, standardized = TRUE, fit.measures = TRUE, rsquare=TRUE)


#===========================================
# Indirect Effects/ Mediation              #
#==========================================-

# Start with the strucutral model from before
# going to add variables to be place holders for the regression coefs
# e.g., adding a a1*PlaceAttachment tells lavaan to hold that regression coef in a varibale a1
# Now we will calculate indirect effects
# The indirect effect between variabels is calcualted by simply muliplying the coefs
# to get the indirect of place on peb through pride, simply multiply the coef between place and pride, and between pride and behaivor

Structural.model.INDIRECT <- '
# Regressions
PEB ~ c1*Pride + c2*Guilt +a1*PlaceAttachment
Pride ~ b1*PlaceAttachment
Guilt ~ b2*PlaceAttachment

# Place Attachment
PlaceAttachment =~ PA.a + PA.b + PA.c + PA.d + PA.e + PA.f

# Anticipated Emotions
Pride =~ AP.a + AP.b + AP.c + AP.d + AP.e
Guilt =~ AG.a + AG.b + AG.c + AG.d + AG.e

# Pro-environmental Behavior
PEB =~ PEB.d + PEB.e + PEB.f + PEB.g + PEB.h + PEB.i

# residual covary
PA.a ~~ PA.b
PEB.g ~~ PEB.h
AG.a ~~ AG.b

# indirect effects
PA_P_PEB:= (b1*c1)
PA_G_PEB:= (b2*c2)
PA_Tot_PEB:= (b1*c1) + (b2*c2)
'

# Can model normally
Structural.fit.INDIRECT <- cfa(Structural.model.INDIRECT, data=dat, missing = 'fiml')
summary(Structural.fit.INDIRECT, standardized = TRUE, fit.measures = TRUE, rsquare=TRUE)

# Can also run bootstrap, takes awhile!!!!
Structural.fit.boot <- cfa(Structural.model.INDIRECT, data=dat, missing = 'fiml', se = "bootstrap", bootstrap = 1000)
summary(Structural.fit.boot, standardized = TRUE, fit.measures = TRUE, rsquare=TRUE)

# Here can see the indirect effect of PA on PEB through Pride is sig
# Through guilt is not! 
# The overall is sig, but that is due to Pride



