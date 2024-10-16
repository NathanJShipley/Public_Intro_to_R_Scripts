#==============================================================================
#                              Analysis Repository:                           #
#                               Spatial Regression                            #      
#==============================================================================


update.packages()

#===========================================
# Load Libraries                           #
#==========================================-
# Some of the spatial packages need some careful attention to installation 
##install.packages("Rcpp")
##install.packages("terra")
##install.packages("Rtools")

library(psych) #has nice describe function for descriptive info
library(tidyverse) #has dplyr and ggplot2
library(sf) #Used in much spatial and mapping analyses
library(tmap) # used in tm mapps
library(spData) #has spatial datasets
library(spdep) #tools for spatial regression
library(ggpubr) #has some more GG plots
library(olsrr) #Some good regression functions and plotting tools
library(jtools) #summ function good to get regression output
library(car) #Durbin-Watson test
library(MASS) #Shapiro-Wilk test
library(lmtest) #Breusch-Pagan test
library(pwr) #power analysis
library(spatialreg) #spatial regression 
library(visreg) #good for looking at regression visz


#===========================================
# Simulate Data OR LOAD DATA               #
#==========================================-
set.seed(101)

boston_506 <- st_read(system.file("shapes/boston_tracts.shp", package = "spData")[1])
dat.sf <- boston_506

# Read in the shape file
#dat.shape <- rgdal::readOGR("data/E_C_S_M/Evictions_Census_Spatial_Merged.shp")

# Convert shape file to sf format
#dat.sf <- sf::st_as_sf(dat.shape)

# Lets check and make sure the data looks good!
ggplot() +
  geom_sf(data = dat.sf)


#===========================================
# Overview                                 #
#==========================================-

# Note this code is focused on spatial aspects
# Additional info on regression can be found in linear regression


#===========================================
# Assess Descriptives                      #
#==========================================-

head(dat.sf)
# Medv is median value of owner homes

# Mean, SD, Median, Min, Max, Skewness, and Kurtosis for y
psych::describe(dat.sf$MEDV)
# Check for no extreme skewness or kurtosis, if so, may have to perform other analyses

# Visualizations for distributions
hist(dat.sf$MEDV)
ggdensity(dat.sf$MEDV)
boxplot(dat.sf$MEDV)
ggqqplot(dat.sf$MEDV)

# Preliminary Exploration of Spatial Distributions
ggplot() +
  geom_sf(data = dat.sf,aes(fill = MEDV))

ggplot() +
  geom_sf(data = dat.sf,aes(fill = CRIM))

# Preliminary Exploration of Associations
dat <- as.data.frame(dat.sf)
pairs.panels(dplyr::select(dat,7,9,14,15,17),scale=TRUE) #scale changes the font of correlation to reflect size

# Assess Multivariate Outliers
psych::outlier(dplyr::select(dat,7,9,14,15,17))

# Scatter plot of two variables
ggplot(dat,aes(RM,MEDV)) +
  geom_point() + 
  geom_smooth(method="lm")

#===========================================
# Build LINEAR Model first & Get Output    #
#==========================================-

Reg.fit <- lm(MEDV ~ CRIM + RM + AGE + RAD, data=dat)

jtools::summ(Reg.fit, digits=3)
# First Model Output to get F, p, R-2, est, se, and t
jtools::summ(Reg.fit, part.corr=TRUE, vifs=TRUE, digits=3)
# can get additional information from jtools, partial cors and VIF
olsrr::ols_correlations(Reg.fit)
# Can also get zero-order, partial, and part all at once
# Important to examine 

# Can run a power analysis, pre or post
# Here using post DF, but using an estimated small effect of .02 and sig value of .05
summary(Reg.fit)
# use summary to get DF
pwr.f2.test(u =4, v = 501, f2 = .02, sig.level = .05)

# Some other quick tools to visualize the model
# Visualize coef uncertainty 
jtools::plot_summs(Reg.fit, scale = TRUE, plot.distributions = TRUE, inner_ci_level = .9)
# Quickly plot estimated relationships 
jtools::effect_plot(Reg.fit, pred = RM, interval = TRUE, plot.points = TRUE)

#===========================================
# Check Assumptions                        #
#==========================================-

# 1.) Normality
# Check for normal distribution
ggdensity(dat$MEDV)
# Want data to be normally distributed 

# 2.) Linearity 
plot(Reg.fit,1)
# Fitted line (red), should be roughly zero across
# If there is a pattern, may indicate non-linearity
# If pattern, then may need to run a polynomial regression

# 3.) Normality of Residuals
plot(Reg.fit,2)
# QQ plot, should roughly be a straight line
# can also formally test using a Shapiro-Wilk Normality test
Reg.resid <- studres(Reg.fit)
shapiro.test(Reg.resid)
# If sig, then not normally distributed

# Residuals may not be normal due to OUTLIERS or variables having high leverage 
# Can examine various plots to ID points exerting extreme influence on outcome 
olsrr::ols_plot_resid_stud(Reg.fit)
# Studentized residuals
olsrr::ols_plot_cooksd_bar(Reg.fit)
# Cooks Distances for outliers
plot(Reg.fit,5)
olsrr::ols_plot_resid_lev(Reg.fit)
# Assess Leverage, ID those with extreme values
# For all extreme values, could be worth removing and assessing how relationships change
# sensitivity type analysis

# 4.) Homoscedasticity
plot(Reg.fit,3)
plot(resid(Reg.fit))
# visualize residuals, want straight line, no patterns
# can formally test using Breusch-Pagan test
bptest(Reg.fit) 
# If sig, then determine not homoscedatic, presence of heteroscedasticity 

# 5.) Independent (Multicollinearity)
durbinWatsonTest(Reg.fit)
# First check autocorrelation using Durbin Watson test
# If sig, then may have to account using a lag variable

# Also check for extreme correlations and VIFs
jtools::summ(Reg.fit, vifs=TRUE, digits=3)
# ideal VIF is less than 4, with anything less than 10 considered acceptable (Hair et al., 2010)
# If high VIF or high correlations, may have to remove factors from the model


#===========================================
# Model adjustments if violate assumptions #
#==========================================-

# If the assumption of Homoscedasticity is violated, use robust standard errors
jtools::summ(Reg.fit, robust = "HC3", digits=3)


#===========================================
# Explore Spatial Relationships            #
#==========================================-

# First lets look at a map of the main outcome of interest 
tm_shape(dat.sf, unit = "mi") +
  tm_borders() + 
  tm_fill("MEDV", palette = "cividis", title = "Median House Value", legend.hist=TRUE,
          style = "jenks", n=5) + 
  tm_layout(legend.outside = T, legend.outside.position = "right",
            main.title = "Median House Value, Boston",
            main.title.size = 1.7, main.title.position = c("left", "top")) +
  tm_compass(type = "arrow", position = c("left", "top"))

# Is there any clustering, visually? 
# Now lets formally test if there is clustering! 

#create queen contiguity
Ev_Q <- poly2nb(dat.sf, queen=T)
summary(Ev_Q)

# create spatial weights
Medv.W <- nb2listw(Ev_Q, style="W", zero.policy=TRUE)
summary(Medv.W, zero.policy=TRUE)

# plot moran scatterplot
moran.plot(dat.sf$MEDV, listw=Medv.W, 
           xlab = "Standardized Median Home Value", 
           ylab = "Standardized Lagged Median Home Value")
# Looks like there is spatial auto
# Formal test of autocorrelation
moran.test(dat.sf$MEDV, listw=Medv.W)
# Sig, Moran I is like correlation 

# monte carlo simulation to get more precise p-values
moran.mc(dat.sf$MEDV, listw=Medv.W, nsim=999)
# Double check if still sig

# Keep in mind, important to evaluate the spatial auto-correlation of BOTH 
# the dependent variable AND the residuals of the final models
# Based on this prelim analysis, seems like there is some spatial auto in the DV
# Now lets check if there is spatial dependence in residuals

#===========================================
# Spatial Regression: Residuals            #
#==========================================-

# First model the original regression model from above
Reg.fit.sf <- lm(MEDV ~ CRIM + RM + AGE + RAD, data=dat.sf)

# Can analyze this model in a very similar manner
jtools::summ(Reg.fit.sf, part.corr=TRUE, digits=3)

# Now lets plot the residuals
dat.sf$MEDV_Res <- residuals(Reg.fit.sf)

tm_shape(dat.sf, unit = "mi") +
  tm_borders() + 
  tm_fill("MEDV_Res", palette = "cividis", title = "Evictions per 1,000", legend.hist=TRUE,
          style = "jenks", n=5) + 
  tm_layout(legend.outside = T, legend.outside.position = "right",
            main.title = "Evictions per 1,000 residents, Madison 2020",
            main.title.size = 1.7, main.title.position = c("left", "top")) +
  tm_compass(type = "arrow", position = c("left", "top"))

# is there clustering in the residuals???
# 
# can also formally test the Moran's I of the residuals
lm.morantest(Reg.fit.sf, Medv.W)
# Monte Carlo to get more precise P-values
moran.mc(dat.sf$MEDV_Res, Medv.W, nsim=1000, zero.policy=TRUE) #

# If there is not evidence for spatial autocorrelation
# Then, use the original regression
# If evidence for spatial dependence , then run spatial models

# if evidence of auto correlation, then OLS coefs will be biased, sizes are not close to true value
# SE will be underestimated, result in bias 

# TWO types of spatial regression models 
# 1.) Spatial Lag = models dependency in the outcome
# 2.) Spatial Error = models dependency in the residuals

#====================== =
# Multiple Comparisons# #https://methods.sagepub.com/images/virtual/spatial-lag-bristol-sale-2004/10.4135_9781526473844-fig14.jpg
#====================== =

# Lagrange multiplier test
# compares fit of spatial models relative to OLS model
# sig tests indicate a rejection of the OLS and suggests 
spdep::lm.LMtests(Reg.fit.sf, listw=Medv.W, test="all", zero.policy=TRUE)

# First examine the LMerr and LMlag, if both are sig, then compare the robust forms
# If neither are sig, then reject and keep OLS!!!!!
# Can use this to then examine a lag and error more closely 

# NOTE that after selecting and fitting a model below
# Must check the residuals! 
# Want the model to control for autocorrelation, such that the residuals won't be auto correlated

#====================== =
# Spatial LAG         # #
#====================== =

Reg.fit.sf_lag <- spatialreg::lagsarlm(MEDV ~ CRIM + RM + AGE + RAD, data=dat.sf, listw = Medv.W)
summary(Reg.fit.sf_lag)
# lag parameter = Rho
# if sig, then likely spatial dependency in DV

# NOTE that we can't interpret the coefs directly, because they are lagged
# In other words, an effect in one area influences others, creating a feedback.
# Have direct and indriect effects
# Have to adjust for this

lag.impacts <- impacts(Reg.fit.sf_lag, listw = Medv.W)
print(lag.impacts) 
# Get output for direct, indirect, and total effects

# CHECK RESIDUALS
dat.sf$LAG_Res <- residuals(Reg.fit.sf_lag)

tm_shape(dat.sf, unit = "mi") +
  tm_borders() + 
  tm_fill("LAG_Res", palette = "cividis", title = "Evictions per 1,000", legend.hist=TRUE,
          style = "jenks", n=5) + 
  tm_layout(legend.outside = T, legend.outside.position = "right",
            main.title = "Evictions per 1,000 residents, Madison 2020",
            main.title.size = 1.7, main.title.position = c("left", "top")) +
  tm_compass(type = "arrow", position = c("left", "top"))

moran.mc(dat.sf$LAG_Res, Medv.W, nsim=1000, zero.policy=TRUE) #
# If there is still sig autocorrelation in residuals, test different model

#====================== =
# Spatial ERROR       # #
#====================== =

Reg.fit.sf_error <- spatialreg::errorsarlm(MEDV ~ CRIM + RM + AGE + RAD, data=dat.sf, listw = Medv.W)
summary(Reg.fit.sf_error)

#Lambda is the error parameter
# if sig, likely spatial dependency in residuals
# In otherwords, unexplained variation in DV is correlated 

# Can also perform a spatial Hausman test
Hausman.test(Reg.fit.sf_error)
# If sig, indicates that the spatial error model and OSL model estimates are different
# If sif, this tells us the assumptions of spatial error have been violated, should not be used


# CHECK RESIDUALS
dat.sf$ERROR_Res <- residuals(Reg.fit.sf_error)

tm_shape(dat.sf, unit = "mi") +
  tm_borders() + 
  tm_fill("ERROR_Res", palette = "cividis", title = "Evictions per 1,000", legend.hist=TRUE,
          style = "jenks", n=5) + 
  tm_layout(legend.outside = T, legend.outside.position = "right",
            main.title = "Evictions per 1,000 residents, Madison 2020",
            main.title.size = 1.7, main.title.position = c("left", "top")) +
  tm_compass(type = "arrow", position = c("left", "top"))

moran.mc(dat.sf$ERROR_Res, Medv.W, nsim=1000, zero.policy=TRUE) #


#===========================================
# Plotting                                 #
#==========================================-

theme_set(
  theme_bw()
)

x1.y <- ggplot(dat,aes(RM,MEDV)) +
  geom_point(color="darkblue") + 
  geom_smooth(method="lm",color="blue",size=1) + 
  ggtitle("Y by x1") + theme(plot.title = element_text(hjust = 0.5)) + 
  xlab("x1 variable") + 
  ylab("y variable")

x1.y

# PLOT FITTED VALUES to further understand model
dat$pred.val <- predict(Reg.fit)

ggplot(dat,aes(RM,pred.val)) +
  geom_point(color="darkblue") + 
  geom_smooth(method="lm",color="blue",size=1) + 
  ggtitle("Y by x1") + theme(plot.title = element_text(hjust = 0.5)) + 
  xlab("X1") + 
  ylab("Y")


#===========================================
# When in doubt, GAM!!!!                   #
#==========================================-

# For more complex models, or when auto cor is not solved, might try a GAM! 
# Can see the GAM code for more information on how to run these types of models
# For spatial, will create a spatial weights matrix
# Then create a smoothing function that uses Markov random fields!!!

View(dat.sf)

dat.sf$ID <- 1:506
dat.sf$ID <- as.factor(dat.sf$ID)

nb <- poly2nb(dat.sf, row.names = dat.sf$ID)
names(nb) <- attr(nb, "region.id")
str(nb[1:6])

ctrl <- gam.control(nthreads = 6)

# Create weights matrix as well based on the nb 
GAM.W <- nb2listw(nb, zero.policy = T)

# Play around with K-values to reduce spatial error

Reg.fit.gam.100 <- gam(MEDV ~ s(ID, bs='mrf', xt=list(nb=nb), k=100) + CRIM + RM + AGE + RAD, 
                   data = dat.sf, family=, 
                   method = "REML", control = ctrl)

Reg.fit.gam.75 <- gam(MEDV ~ s(ID, bs='mrf', xt=list(nb=nb), k=75) + CRIM + RM + AGE + RAD, 
                       data = dat.sf, family=, 
                       method = "REML", control = ctrl)

Reg.fit.gam.70 <- gam(MEDV ~ s(ID, bs='mrf', xt=list(nb=nb), k=70) + CRIM + RM + AGE + RAD, 
                      data = dat.sf, family=, 
                      method = "REML", control = ctrl)

Reg.fit.gam.60 <- gam(MEDV ~ s(ID, bs='mrf', xt=list(nb=nb), k=60) + CRIM + RM + AGE + RAD, 
                      data = dat.sf, family=, 
                      method = "REML", control = ctrl)

Reg.fit.gam.50 <- gam(MEDV ~ s(ID, bs='mrf', xt=list(nb=nb), k=50) + CRIM + RM + AGE + RAD, 
                       data = dat.sf, family=, 
                       method = "REML", control = ctrl)

# Save deviance then test 
dat.sf$Res.100 <- residuals(Reg.fit.gam.100, type = "deviance")
dat.sf$Res.75 <- residuals(Reg.fit.gam.75, type = "deviance")
dat.sf$Res.70 <- residuals(Reg.fit.gam.70, type = "deviance")
dat.sf$Res.60 <- residuals(Reg.fit.gam.60, type = "deviance")
dat.sf$Res.50 <- residuals(Reg.fit.gam.50, type = "deviance")

moran.mc(dat.sf$Res.100, GAM.W, nsim=1000, zero.policy=TRUE) #
moran.mc(dat.sf$Res.75, GAM.W, nsim=1000, zero.policy=TRUE) #
moran.mc(dat.sf$Res.70, GAM.W, nsim=1000, zero.policy=TRUE) #
moran.mc(dat.sf$Res.60, GAM.W, nsim=1000, zero.policy=TRUE) #
moran.mc(dat.sf$Res.50, GAM.W, nsim=1000, zero.policy=TRUE) #

summary(Reg.fit.gam.75, digits=6) # 
summary(Reg.fit.gam.70, digits=6) # 
broom::glance(Reg.fit.gam.75) #### A good model
broom::glance(Reg.fit.gam.70)


