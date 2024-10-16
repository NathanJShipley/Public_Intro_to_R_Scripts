#==============================================================================
#                              Analysis Repository:                           #
#                               Poisson Regression                            #      
#==============================================================================

#===========================================
# Load Libraries                           #
#==========================================-
library(psych) #has nice describe function for descriptive info
library(tidyverse) #has dplyr and ggplot2
library(AER) #Contains dataset used (Ship Accidents)
library(ggpubr) #has some more GG plots
library(MASS) #used to fit negative binomial
library(pscl) #odTest, likelihood ratio test for over-dispersion
library(jtools) #summ function good to get regression output
library(ggstance) #Provides functions that Jtools uses
library(epiDisplay) #overdispersion
library(pscl) # Zero inflation AND model testing
library(mvnormtest) #normality

#===========================================
# Simulate Data OR LOAD DATA               #
#==========================================-
set.seed(101)

data("CreditCard")
dat <- CreditCard

#dat <- read.csv("data.csv", header=T)
#dat <- readxl::read_xlsx("fake.data.xlsx")


#===========================================
# Overview                                 #
#==========================================-

# Poisson regression is used to mostly analyze count data
# It uses the Generalized linear model, which uses maximum likelihood estimation 
# This model transforms the count data into a log
# This model shares some in common with Logistic regression


#===========================================
# Assess Descriptives                      #
#==========================================-

head(dat)
View(dat)

# Mean, SD, Median, Min, Max, Skewness, and Kurtosis for y
psych::describe(dat$active)
# can see mean is 7, 7 active cards is the average per person

summary(dat)

# Visualizations for distributions
hist(dat$active)
ggdensity(dat$active)
ggqqplot(dat$active)
# CLEARLY SEE that this is count data, heavily skewed

# Preliminary Exploration of Associations
pairs.panels(dat, scale=TRUE)

# Assess multivariate normality 
mshapiro.test(t(dat))

# Assess Multivariate Outliers
psych::outlier(dat)

# Scatter plot of two variables
# SCATTER PLOT USING COUNT DATA
ggplot(dat,aes(active,share)) +
  geom_point() + 
  geom_smooth()


#===========================================
# Build Model & Get Output                 #
#==========================================-

Reg.fit <- glm(active ~ age + income + share + expenditure,  data=dat, family = "poisson")

jtools::summ(Reg.fit, digits=3)
# First Model Output to get F, p, R-2, est, se, and t
# note Cragg_Uhler R-squared is also Naglekerke's 
# ALSO Note that coefs are the NATURAL LOGs of the DV
# So as share goes up by 1, the log of active goes down 0.491
# to calculate the effects, will need to calculate the value and exp it
pred.count.low <- 1.36513 + (0*-0.4917447) # low share = 0 
pred.count.avg <- 1.36513 + (.07*-0.4917447) # avg share = avg
pred.count.high <- 1.36513 + (.25*-0.4917447) # high share = +2 SD

# now take the exp to turn this back into the original count data
exp(pred.count.low) # low share, expected to have 3.9 active cards
exp(pred.count.avg) # average have 3.8 cards
exp(pred.count.high) # high have 3.46 cards

# Can also take exp of the coef to get an estimated % change
glm.est <- cbind(Estimate = coef(Reg.fit), confint(Reg.fit))
exp(glm.est)

# Lets look at DEVIANCE
summary(Reg.fit)
# Null deviance is how well outcome is predicted by just the intercept
# Residual deviance is the residual with IV
# These can be helpful for evaluating the model, which will be done in the next section

# Some other quick tools to visualize the model
# Visualize coef uncertainty 
jtools::plot_summs(Reg.fit, scale = TRUE, plot.distributions = TRUE, inner_ci_level = .9)
# Quickly plot estimated relationships 
jtools::effect_plot(Reg.fit, pred = age, interval = TRUE, plot.points = TRUE)

# Lets save the predicted values for later 
dat$pred.val <- predict(Reg.fit,type = "response")


#===========================================
# Check Assumptions                        #
#==========================================-

# Because Poisson uses a link function, it is important to check if the transformed IV is normally distributed
# The code here will calculate some metrics of the fitted values to assess distribution
model.data <- broom::augment(Reg.fit) %>% 
  mutate(index = 1:n()) 

ggplot(model.data, aes(index, .fitted)) + 
  geom_point(aes(color = active), alpha = .5) +
  theme_bw()
# This graph shows the fitted logit values for each indexed value


# 1.) Linearity 
# Are the IV linearly related to the LOGIT values
# Fitted line should be roughly linear
# If there is a pattern, may indicate non-linearity
# If pattern, then may need to make adjustments
# SCATTER PLOTS USING FITTED DATA
ggplot(model.data, aes(age, .fitted)) + 
  geom_point(aes(color = active), alpha = .5) +
  geom_smooth(method = "loess") + 
  theme_bw()

ggplot(model.data, aes(income, .fitted)) + 
  geom_point(aes(color = active), alpha = .5) +
  geom_smooth(method = "loess") + 
  theme_bw()

ggplot(model.data, aes(share, .fitted)) + 
  geom_point(aes(color = active), alpha = .5) +
  geom_smooth(method = "loess") + 
  theme_bw()

# can also Assess linearity with this
plot(Reg.fit,1)



# 2.) Normality of Residuals and no extreme values
plot(Reg.fit,2)
# QQ plot, should roughly be a straight line

# Residuals
ggplot(model.data, aes(index, .std.resid)) + 
  geom_point(aes(color = active), alpha = .5) +
  theme_bw()

# Compare fitted to residual and Deviance residuals
dat$residual <- residuals(Reg.fit, type = "pearson")
dat$res.deviance <- residuals(Reg.fit, type = "deviance")

ggplot(dat, aes(pred.val, res.deviance)) + 
  geom_point(aes(color = active), alpha = .5) +
  theme_bw()

ggplot(dat, aes(pred.val, res.deviance)) + 
  geom_point(aes(color = active), alpha = .5) +
  theme_bw()
# Want both of these graphs to look similar!!!!!!

# Cooks
ggplot(model.data, aes(index, .cooksd)) + 
  geom_point(aes(color = active), alpha = .5) +
  theme_bw()

# 3.) Independent (Multicollinearity)
jtools::summ(Reg.fit, vifs=TRUE, digits=3)
# ideal VIF is less than 4, with anything less than 10 considered acceptable (Hair et al., 2010)
# If high VIF or high correlations, may have to remove factors from the model


#====================== =
# # Overdispersion # #
#====================== =

# data admit more variability than expected based on distributions
# if over dispersion is present, then SE and stats will be distorted
# IF THERE IS overdispersion, then use a negative binomial distribution in GLM
# Can formally test for overdispersion

# IF this ratio is MUCH larger than 1, then indicate overdispersion 
deviance(Reg.fit)/df.residual(Reg.fit) 

# Can also check GOODNESS OF FIT, if SIG, suggests model is not correctly specified and indicates overdispersion
# Can manually estimate goodness of fit 
with(Reg.fit, cbind(res.deviance = deviance, df = df.residual,
                    p = pchisq(deviance, df.residual, lower.tail=FALSE)))
# Also nice code to check it for us
poisgof(Reg.fit)
# If sig, suggests overdispersion

# IF evidence of overdispersion, so lets fit a negative binomial
# First fit a negative binomial model, then compare log-lik to a Poisson
# So fit the negative binomial model
Reg.fit.nb <- glm.nb(active ~ age + income + share + expenditure,  data=dat) #
# then compare, using the function below
odTest(Reg.fit.nb, alpha=.05)
# This is sig, which indicates evidence for overdispersion
# NOTE negative binomial assume conditional mean and variance are not equal
# this differences is captured by a "dispersion" parameter in negative binomial that is otherwise held constant by Poisson
# --- Poisson is a nested model of negative binomial 

# Can also compare the Poisson to the negative binomial model 
anova(Reg.fit,Reg.fit.nb)

# additionally can estimate a
Reg.fit.zi = zeroinfl(active ~ age + income + share + expenditure,  
                      data=dat,
                      dist = "poisson")

# Formally test the three models
vuong(Reg.fit,
      Reg.fit.nb,
      digits = 4)

vuong(Reg.fit.nb,
      Reg.fit.zi,
      digits = 4)
# This script will give an indicate on which model is better
# These in particualr suggest the NB is best

# And can check the ratio for overdispersion as we did previously for the new NB
deviance(Reg.fit.nb)/df.residual(Reg.fit.nb) 

# Formal goodness of fit
with(Reg.fit.nb, cbind(res.deviance = deviance, df = df.residual,
                       p = pchisq(deviance, df.residual, lower.tail=FALSE)))
poisgof(Reg.fit.nb)
# If sig, still suggests the model may not be great
# BUT also some evidence that goodness of fit for Poisson may not be great 
#https://thestatsgeek.com/2014/04/26/deviance-goodness-of-fit-test-for-poisson-regression/

# Lastly, it may be worthwhile to compare the models visually
# Helpful for understanding what the models are estimating
jtools::plot_summs(Reg.fit, Reg.fit.nb, scale = TRUE, plot.distributions = TRUE)
# This just shows the confidence of model coefs


#===========================================
# Model adjustments if violate assumptions #
#==========================================-

# If evidence for overdispersion, then run negative binomial as before
Reg.fit.nb <- glm.nb(active ~ age + income + share + expenditure,  data=dat) #
# Then use this to evaluate model
# also worth running the other previous assumptions (plots)

summary(Reg.fit.nb)
# Can also check 95% CI 
jtools::summ(Reg.fit.nb, digits=3, confint = TRUE)
# Also can check robust SE
jtools::summ(Reg.fit.nb, digits=3, robust = "HC3", confint = TRUE)

# Some other quick tools to visualize the model
# Visualize coef uncertainty 
jtools::plot_summs(Reg.fit.nb, scale = TRUE, plot.distributions = TRUE, inner_ci_level = .9)
# Quickly plot estimated relationships 
jtools::effect_plot(Reg.fit.nb, pred = age, interval = TRUE, plot.points = TRUE)

# NOTE can analyze coef just as in the normal Poisson, sum the intercepts with variables, then take the EXP() to get counts
glm.est.nb <- cbind(Estimate = coef(Reg.fit.nb), confint(Reg.fit.nb))
exp(glm.est.nb)

# Lets save the predicted values for later 
dat$nb.pred.val <- predict(Reg.fit.nb,type = "response")

# AGAIN, check assumptions
# also check goodness of fit! 

#===========================================
# Plotting                                 #
#==========================================-

theme_set(
  theme_bw()
)

x1.y <- ggplot(dat,aes(age,active)) +
  geom_point(color="darkblue") + 
  geom_smooth(method="lm",color="blue",size=1) + 
  ggtitle("Y by x1") + theme(plot.title = element_text(hjust = 0.5)) + 
  xlab("x1 variable") + 
  ylab("y variable")

x1.y

# Can plot fitted values as well
# Measure of accuracy
ggplot(dat,aes(age,nb.pred.val)) +
  geom_point(color="darkblue") + 
  geom_smooth(method="lm",color="blue",size=1) + 
  ggtitle("Y by x1") + theme(plot.title = element_text(hjust = 0.5)) + 
  xlab("Age") + 
  ylab("Active")


#===========================================
# Additional model diagnostics and info    #
#==========================================-

#====================== =
# # Accuracy # #
#====================== =

# Plot actual value by the predicted value
ggplot(dat,aes(pred.val,active)) +
  geom_point(color="darkblue") + 
  geom_smooth(method="lm",color="blue",size=1) + 
  ggtitle("Y by x1") + theme(plot.title = element_text(hjust = 0.5)) + 
  xlab("Predicted") + 
  ylab("Actual")

ggplot(dat,aes(nb.pred.val,active)) +
  geom_point(color="darkblue") + 
  geom_smooth(method="lm",color="blue",size=1) + 
  ggtitle("Y by x1") + theme(plot.title = element_text(hjust = 0.5)) + 
  xlab("Predicted") + 
  ylab("Actual")



