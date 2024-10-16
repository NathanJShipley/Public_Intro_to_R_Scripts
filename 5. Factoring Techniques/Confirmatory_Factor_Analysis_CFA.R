#==============================================================================
#                              Analysis Repository:                           #
#                       Confirmatory Factor Analysis (CFA)                    #      
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
# Confirmatory Factor Analysis             #
#==========================================-

# We use the 'lavaan' package for CFA and SEM models, which is short for latent variable analysis 
# The basic flow of a CFA is 
# 1.) Create Model Syntax
# 2.) Use syntax to estimate model fit
# 3.) View output

#====================== =
# Normality & Other Assessments # #
#====================== =

# First check assumption of normality in data
# Can do this as a univariate, but also multi
mshapiro.test(t(dplyr::select(dat,7:15)))
# If non-normal, should refrain from using maximum likelihood as factor extraction

# Assess Multivariate Outliers
psych::outlier(dplyr::select(dat,7:15))


#====================== =
# LAVAAN Syntax       # #
#====================== =

# Unlike in EFA, in CFA we have a theory as to how the data should be organized
# That is, we hypo before collecting data that certain items are measuring the same latent construct
# Latent as in UNOBSERVED, something that can't be measured, but we use items or indicators to reflect the construct

# Here, saying that item 1,2,3 are going to load onto factor 1, called "visual"
HS.model <- ' 
visual  =~ x1 + x2 + x3 
textual =~ x4 + x5 + x6
speed   =~ x7 + x8 + x9 
'

# Some more notes
# =~ is telling lavaan to create a latent construct, measure, that 
# always start and end the model syntax with '

#====================== =
# Fit CFA Model       # #
#====================== =

# Fit model
# After creating model syntax, we need to estimate the model using the "cfa" function
# Here we are saving the output of the "cfa" function to an object cfa.AE.fit
# We are telling the cfa function to use our Kasky.dat object for the data
# Also using the full information maximum likelihood estimator for missing data
HS.fit <- cfa(HS.model, data=dat, missing = 'fiml')

#====================== =
# Model Output        # #
#====================== =

# After creating an object for the model fit, we can use the "summary" function to get our output
summary(HS.fit, standardized = TRUE, fit.measures = TRUE, rsquare=TRUE)
# Note that most summary information needed for a CFA can be found using the above function
# User model chi-square = want non sig (but not very important)
# CFI > .90, good CFI ??? .95
# TLI > .90
# RMSEA <.08, .05 is desired, good ??? .06
# SRMR <.08, 
# all std loading (std is correlation, not covariance) > must be above .4, but really above .7
# Can look at covariances between factors! Note the use of covariances in CFA and SEM
# Intercepts = Means
# Variances and R-squares for each item are also given

# However there are many other measures of fit and other values that are useful for estimating a CFA
# Measures of reliability for CFA are important, there are many metrics all obtainable 
# Check factor reliability 
reliability(HS.fit)
# 1.) alpha = Cronbach's alpha >>> want to be above .7
# 2.) Omega = CR (Composite or construct reliability) >>> want to be above .6
# 3.) avevar = AVE (Average variance extracted) >>> want to be above .5

# Another way to assess the fit of a CFA is to examine the model implied correlation/covariance matrix and residuals
inspect(HS.fit, "implied") # model implied covariance matrix
inspect(HS.fit, "cor.all") # model implied correlation between all observed and latent variables
inspect(HS.fit, "resid") # difference between observed and model-implied covariance matrix
# When doing this, look for EXTREME values as indicator something may be wrong


#====================== =
# Model Re-fitting (as needed)  # #
#====================== =

# Lastly, You can also assess CFA fit by exploring modification indices
# NOTE that CFA and SEM are driven by testing a priori hypotheses
# Modification indices are very powerful, but are data driven and not theoretically driven
# Any changes made in light of modification indices should be made with caution
modindices(HS.fit, sort. = T, power = T)
# Second mod ind offered is a residual correlation (x7 ~~  x8), might be worth adding to the model

# A follow up note 
# A common model refitting is to add residual correlations between items 
# This is generally acceptable as long as items are WITHIN factors
# Now lets re-fit the model

HS.model.v2 <- ' 
visual  =~ x1 + x2 + x3 
textual =~ x4 + x5 + x6
speed   =~ x7 + x8 + x9 
x7 ~~ x8
'

# Above, adding the a double ~ creates a covariance between the two items
# Re-fit the model
HS.fit.v2 <- cfa(HS.model.v2, data=dat, missing = 'fiml')
# Print the new summary info
summary(HS.fit.v2, standardized = TRUE, fit.measures = TRUE, rsquare=TRUE)

# Lastly, can compare model fits directly 
anova(HS.fit, HS.fit.v2)
# If sig, suggests the model is superior


#===========================================
# Factor Creation                          #
#==========================================-

# Unlike in EFA, it is not necessary to create variables using factors ID in a CFA
# That is, these models are seeking to measure latent variables
# These latent variables can then be used in other analyses like SEM.
# BUT if factor scores are still desired, they can be obtained

# Will use regression factorization, other options like bartletts exist
lavPredict(HS.fit.v2) #default is regression
# Can get output directly, also save to 

dat.predict <- lavPredict(HS.fit.v2)
dat <- cbind(dat,dat.predict)

