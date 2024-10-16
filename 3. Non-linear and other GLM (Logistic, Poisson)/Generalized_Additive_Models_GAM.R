#==============================================================================
#                              Analysis Repository:                           #
#                          Generalized Additive Models                        #      
#==============================================================================

#===========================================
# Load Libraries                           #
#==========================================-

library(psych) #has nice describe function for descriptive info
library(tidyverse) #has dplyr and ggplot2
library(ggpubr) #has some more GG plots
library(corrplot) #nice cor plot
library(mgcv) # used for GAM models
library(car) #Also have component and residual plots
library(visreg) #nice function to view regression
library(caret) #cross validation for ML models
library(vip) #variable importance
library(MASS, exclude = "select") #Shapiro-Wilk test
library(lmtest) #Breusch-Pagan test
library(jtools) #summ function good to get regression output


#===========================================
# Simulate Data OR LOAD DATA               #
#==========================================-

set.seed(101)

dat <- read.csv("insurance.csv", header=T)
#dat <- readxl::read_xlsx("fake.data.xlsx")

glimpse(dat)

dat$smoker <- as.factor(dat$smoker)

#===========================================
# Assess Descriptives                      #
#==========================================-

# Mean, SD, Median, Min, Max, Skewness, and Kurtosis for y
psych::describe(dat)
# Check for no extreme skewness or kurtosis, if so, may have to perform other analyses

# Visualizations for distributions
hist(dat$charges)
ggdensity(dat$charges)
boxplot(dat$charges)
ggqqplot(dat$charges)

# Preliminary Exploration of Associations
pairs.panels(dat, scale=TRUE)
corrplot(cor(dplyr::select(dat,1,3,4,7), use="complete.obs"), order = "hclust", tl.col='black', tl.cex=.75) 

# Scatter plot of two variables
ggplot(dat,aes(bmi, charges)) +
  geom_point() + 
  geom_smooth()

ggplot(dat,aes(age, charges)) +
  geom_point() + 
  geom_smooth()

ggplot(dat,aes(children, charges)) +
  geom_point() + 
  geom_smooth()

ggplot(dat,aes(smoker, charges)) +
  geom_point() + 
  geom_smooth()

ggplot(dat,aes(x=age, y=charges, color=smoker)) +
  geom_point() + 
  geom_smooth()

ggplot(dat,aes(x=bmi, y=charges, color=smoker)) +
  geom_point() + 
  geom_smooth()

# Assess Multivariate Outliers
psych::outlier(dplyr::select(dat,1,3,4,7))

#===========================================
# Build Linear Model for Baseline & Get Output   #
#==========================================-

# Fit both a LM and a GAM
Reg.fit <- lm(charges ~ age + sex + bmi + children + smoker + region, data=dat)
jtools::summ(Reg.fit, part.corr=TRUE, vifs=TRUE, digits=3)
crPlots(Reg.fit)
crPlots(Reg.fit, terms=~age, )
visreg(eg.fit)
plot(Reg.fit, all.terms = TRUE, pages = 1)

#===========================================
# Build GAM & Get Output                   #
#==========================================-

# There are some steps to fitting a GAM
# First, put s in front of variable to apply a smooth effect
# The type of smooth has a default, but this can be changed with bs = 
# NOTE than this model below will produce the SAME results as the linear model above
GAM.linear.fit <- gam(charges ~ age + sex + bmi + children + smoker + region, data=dat)

# Note the similarities with the linear model above
summary(Reg.fit)
summary(GAM.linear.fit)


# Now lets fit a GAM with all factors smoothed
# Note only on numerical values
GAM.smooth.fit <- gam(charges ~ s(age) + sex + s(bmi) + children + smoker + region, data=dat)

summary(GAM.smooth.fit)
coef(GAM.smooth.fit)
# This model controls for non-linear of age and bmi
# IF a edf was 1, we could conclude the effect was a simple linear
# but we can see the edf for age and bmi is greater than 1 and sig
# Also tells us the family and link function
# Next section is the parametric terms - PRE-determined form, linear terms
# Final seciton is the smooth terms --- edf 2 is quadratic, 3 is cuibc, etc


# Look at each variable and relationship with DV
plot(GAM.smooth.fit, all.terms = TRUE, pages = 1)
plot(GAM.smooth.fit, all.terms = TRUE, pages = 1, residuals = T)
plot(GAM.smooth.fit, all.terms = TRUE, pages = 1, shade = T)

visreg(GAM.smooth.fit)

plot(GAM.smooth.fit, residuals = T)
plot(GAM.smooth.fit, residuals = T, pch=1)
# PCH is the size 

plot(GAM.smooth.fit, pages = 1)

# Compare model fit between the GAM and LM
AIC(GAM.linear.fit)
AIC(GAM.smooth.fit)
summary(GAM.linear.fit)$r.sq
summary(GAM.smooth.fit)$r.sq
anova(GAM.linear.fit, GAM.smooth.fit, test="Chisq")

# Fit Residuals and Fitted values
dat$fitted <- GAM.smooth.fit$fitted.values
dat$residual <- GAM.smooth.fit$residuals

ggplot(dat, aes(bmi, fitted)) + 
  geom_point(alpha = .5) +
  geom_smooth(method = "loess") + 
  theme_bw()

ggplot(dat, aes(age, fitted)) + 
  geom_point(alpha = .5) +
  geom_smooth(method = "loess") + 
  theme_bw()


# Also good to check some residuals and fitted values 
gam.check(GAM.smooth.fit, rep=1000)
# First reports on model convergence
# Checking resutls basis, if sig, may need more k! 

concurvity(GAM.smooth.fit, full = T)
concurvity(GAM.smooth.fit, full = F)
# when dealing with GAM, notion of concurvity can be an issue
# 

#===========================================
# Check Residuals                        #
#==========================================-

ggplot(dat, aes(bmi, residual)) + 
  geom_point(alpha = .5) +
  geom_smooth(method = "loess") + 
  theme_bw()

ggplot(dat, aes(age, residual)) + 
  geom_point(alpha = .5) +
  geom_smooth(method = "loess") + 
  theme_bw()


#===========================================
# Changing Smoothing Parameters and Fit     #
#==========================================-

# Can fit GAMS with various adjustments
# Can also change the method for how the model is fitted
GAM.smooth.fit.rmel <- gam(charges ~ s(age) + sex + s(bmi) + children + smoker + region, data=dat, method = "REML")

# Compare Models
anova(GAM.smooth.fit, GAM.smooth.fit.rmel, test="Chisq")

# can change the smoothing parameters
GAM.smooth.fit.v2 <- gam(charges ~ s(age, sp = 0.1) + sex + s(bmi) + children + smoker + region, data=dat)
GAM.smooth.fit.v3 <- gam(charges ~ s(age, sp = 0.3) + sex + s(bmi) + children + smoker + region, data=dat)
GAM.smooth.fit.v4 <- gam(charges ~ s(age, sp = 0.4) + sex + s(bmi) + children + smoker + region, data=dat)

# Compare Models
anova(GAM.smooth.fit, GAM.smooth.fit.v2, test="Chisq")
anova(GAM.smooth.fit, GAM.smooth.fit.v3, test="Chisq")
anova(GAM.smooth.fit, GAM.smooth.fit.v4, test="Chisq")

# Can also change how many k arguments, too low will keep smooth
# Too high k arguments will make the line toooo wiggly
GAM.smooth.fit.v5 <- gam(charges ~ s(age) + sex + s(bmi, k = 3) + children + smoker + region, data=dat)
GAM.smooth.fit.v6 <- gam(charges ~ s(age) + sex + s(bmi, k = 6) + children + smoker + region, data=dat)

# Compare Models
anova(GAM.smooth.fit, GAM.smooth.fit.v5, test="Chisq")
anova(GAM.smooth.fit, GAM.smooth.fit.v6, test="Chisq")

visreg(GAM.smooth.fit.v5, "bmi")
visreg(GAM.smooth.fit.v6, "bmi")

plot(GAM.smooth.fit.v5, residuals = T)
plot(GAM.smooth.fit.v6, residuals = T)


# CAN also change the smoothing function to be based on another variable
#### dat$smoker <- as.factor(dat$smoker)
GAM.smooth.fit.v7 <- gam(charges ~ s(age) + sex + s(bmi, by = smoker) + children + smoker + region, data=dat)

summary(GAM.smooth.fit.v6)

plot(GAM.smooth.fit.v7, all.terms = TRUE, pages = 1)

AIC(GAM.smooth.fit.v7)
anova(GAM.smooth.fit, GAM.smooth.fit.v7, test="Chisq")

gam.check(GAM.smooth.fit.v7, rep=1000)
# suggests higher K
concurvity(GAM.smooth.fit.v7, full = T)
concurvity(GAM.smooth.fit.v7, full = F)


#### Same model as above with dif k

GAM.smooth.fit.v8 <- gam(charges ~ s(age, k=20) + sex + s(bmi, by = smoker, k=20) + children + smoker + region, data=dat)
plot(GAM.smooth.fit.v8, all.terms = TRUE, pages = 1)
anova(GAM.smooth.fit, GAM.smooth.fit.v8, test="Chisq")
anova(GAM.smooth.fit.v7, GAM.smooth.fit.v8, test="Chisq")
gam.check(GAM.smooth.fit.v8, rep=1000)

AIC(GAM.smooth.fit)
AIC(GAM.smooth.fit.v7)
AIC(GAM.smooth.fit.v8)


# Gams can also run binomials, logisitc GAM
GAM.smooth.fit.log <-  gam(charges ~ s(age, k=20) + sex,
    data = dat,
    family = binomial,
    method = "REML")

plogis(coef(GAM.smooth.fit.log)[1])

plot(GAM.smooth.fit.log, pages = 1, trans = plogis)

# transform output to probability scale 
# can add ,shift = coef(GAM.smooth.fit.log) to shit the intercept???


#===========================================
# Plotting                                 #
#==========================================-


theme_set(
  theme_bw()
)

x1.y <- ggplot(dat,aes(x1,y)) +
  geom_point(color="darkblue") + 
  geom_smooth(method="lm",color="blue",size=1) + 
  ggtitle("Y by x1") + theme(plot.title = element_text(hjust = 0.5)) + 
  xlab("x1 variable") + 
  ylab("y variable")

x1.y


x2.y <- ggplot(dat,aes(x2,y)) +
  geom_point(color="darkblue") + 
  geom_smooth(method="lm",color="blue",size=1) + 
  ggtitle("Y by x2") + theme(plot.title = element_text(hjust = 0.5))

x2.y

# PLOT FITTED VALUES to further understand model
dat$pred.val <- predict(Reg.fit)

ggplot(dat,aes(x1,pred.val)) +
  geom_point(color="darkblue") + 
  geom_smooth(method="lm",color="blue",size=1) + 
  ggtitle("Y by x1") + theme(plot.title = element_text(hjust = 0.5)) + 
  xlab("X1") + 
  ylab("Y")



#==================================================================================
# Evaluate Predictive Capacity and compare models                               #
#=================================================================================-

# holdout train and test data split
training.samples <- caret::createDataPartition(dat$charges, p=.8, list=FALSE)

train.dat <- dat[training.samples,]
test.dat <- dat[-training.samples,]
dim(train.dat); dim(test.dat)

Reg.fit.train <- lm(charges ~ age + sex + bmi + children + smoker + region, data=train.dat)
GAM.fit.train <- gam(charges ~ s(age) + sex + s(bmi) + children + smoker + region, data=train.dat)
GAM.fit.best.train <- gam(charges ~ s(age, k=20) + sex + s(bmi, by = smoker, k=50) + children + smoker + region, data=train.dat)
GAM.fit.best.train.better <- gam(charges ~ s(age, k=20) + sex + s(bmi, by = smoker, k=100) + children + smoker + region, data=train.dat)
GAM.fit.best.train.best <- gam(charges ~ s(age, k=20) + sex + s(bmi, by = smoker, k=400) + children + smoker + region, data=train.dat)

# Create a model fit summary function
# MSE = MEAN SQUARED ERROR
# MAE = MEAN ABSOLUTE ERROR
# RMSE = ROOT MEAN SQUARED ERROR
# R-sq = COEFFICIENT OF DETERMINATION

MOD_FIT <- function(Observed,Predicted){
  mse <- mean((Observed-Predicted)^2)
  mae <- caret::MAE(Observed,Predicted)
  rmse <- caret::RMSE(Observed,Predicted)
  r2 <- (cor(Observed,Predicted))^2
  return(cat("MSE: ", mse, "MAE: ", mae, " RMSE: ", rmse, " R2: ", r2))
}

test.dat$LM.pred <- predict(Reg.fit.train, test.dat)
test.dat$GAM.pred <- predict(GAM.fit.train, test.dat)
test.dat$GAM.pred.best <- predict(GAM.fit.best.train, test.dat)
test.dat$GAM.p.bb <- predict(GAM.fit.best.train.better, test.dat)
test.dat$GAM.p.bbb <- predict(GAM.fit.best.train.best, test.dat)

MOD_FIT(test.dat$charges, test.dat$LM.pred)
MOD_FIT(test.dat$charges, test.dat$GAM.pred)
MOD_FIT(test.dat$charges, test.dat$GAM.pred.best)
MOD_FIT(test.dat$charges, test.dat$GAM.p.bb)
MOD_FIT(test.dat$charges, test.dat$GAM.p.bbb)


ggplot(test.dat, aes(x=LM.pred, y=charges)) +
  geom_point() + 
  geom_smooth()

ggplot(test.dat, aes(x=bmi)) +
  geom_smooth(aes(y=charges), color="blue") + 
  geom_smooth(aes(y=LM.pred), color="red") + 
  geom_smooth(aes(y=GAM.pred.best), color="green")


# Can compare random forest to the linear regression
ggplot(test.dat, aes(y=charges)) +
  geom_point(aes(x=LM.pred), color = "red") + 
  geom_smooth(aes(x=LM.pred), color = "red") + 
  geom_point(aes(x=GAM.pred.best), color = "blue") + 
  geom_smooth(aes(x=GAM.pred.best), color = "blue")






