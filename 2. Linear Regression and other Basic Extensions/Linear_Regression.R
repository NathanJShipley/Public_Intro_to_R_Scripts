#==============================================================================
#                              Analysis Repository:                           #
#                               Linear Regression                             #      
#==============================================================================

#===========================================
# Load Libraries                           #
#==========================================-

library(psych) #has nice describe function for descriptive info
library(tidyverse) #has dplyr and ggplot2
library(olsrr) #Some good regression functions and plotting tools
library(jtools) #summ function good to get regression output
library(ggstance) #Provides functions that Jtools uses
library(ggpubr) #has some more GG plots
library(car) #Durbin-Watson test
library(MASS) #Shapiro-Wilk test
library(lmtest) #Breusch-Pagan test
library(pwr) #power analysis
library(Metrics) # used to calculate RMSE and MAE
library(caret) # used for cross validation 
library(mvnormtest) #normality
library(corrplot) #nice cor plot
library(visreg) #Some nice relationship visualizations
library(vip) #variable importance


#===========================================
# Simulate Data OR LOAD DATA               #
#==========================================-

set.seed(101)

#x1 <- rnorm(1000,1,1)
#x2 <- rnorm(1000,1,1)
#x3 <- rnorm(1000,1,1)
#y <- (1.5 * x1) - (.5*x3) + rnorm(1000,1,.8)

#dat <- data.frame(x1,x2,x3,y)

dat <- read.csv("data/insurance.csv", header=T)
#dat <- readxl::read_xlsx("fake.data.xlsx")


#===========================================
# Assess Descriptive                      #
#==========================================-

# View data 
glimpse(dat)
head(dat)
str(dat)
# Mean, SD, Median, Min, Max, Skewness, and Kurtosis for y
psych::describe(dat$charges)
# Check for no extreme skewness or kurtosis, if so, may have to perform other analyses

# Visualizations for distributions
hist(dat$charges)
ggdensity(dat$charges)
boxplot(dat$charges)
ggqqplot(dat$charges)

# Preliminary Exploration of Associations
pairs.panels(dat, scale=TRUE)
corrplot(cor(dplyr::select(dat,1,3,4,7), use="complete.obs"), order = "hclust", tl.col='black', tl.cex=.75) 

# plots 
ggplot(dat,aes(age,charges)) +
  geom_point() + 
  geom_smooth()

boxplot(charges ~ smoker, data = dat)

ggplot(dat,aes(age,charges, color=smoker)) +
  geom_point() + 
  geom_smooth(method="lm", fullrange=T) +
  facet_wrap( ~ smoker)

# Assess Multivariate Outliers
psych::outlier(dplyr::select(dat,1,3,4,7))


#===========================================
# Build Model & Get Output                 #
#==========================================-

Reg.fit <- lm(charges ~ age + sex + bmi + children + smoker + region, data=dat)

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
pwr.f2.test(u =3, v = 996, f2 = .02, sig.level = .05)

# Some other quick tools to visualize the model
# Visualize coef uncertainty 
jtools::plot_summs(Reg.fit, scale = TRUE, plot.distributions = TRUE, inner_ci_level = .9)
# Quickly plot estimated relationships 
jtools::effect_plot(Reg.fit, pred = bmi, interval = TRUE, plot.points = TRUE)

# Some other visualizations of relationships 
crPlots(Reg.fit)
crPlots(Reg.fit, terms=~age, )

visreg(Reg.fit)

plot(Reg.fit, all.terms = TRUE, pages = 1)

# Evaluate variable importance 
var.i.plot <- vip(Reg.fit, geom = "point")

#===========================================
# Check Assumptions                        #
#==========================================-

# 1.) Normality
# Check for normal distribution
ggdensity(dat$charges)
# Want data to be normally distributed 

# Assess multivariate normality 
mshapiro.test(t(dat))

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

# Assess Multivariate Outliers
psych::outlier(dat)

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
jtools::summ(Reg.fit, robust = "HC1", digits=3)
jtools::summ(Reg.fit, robust = "HC2", digits=3)
jtools::summ(Reg.fit, robust = "HC3", digits=3)
# note can adjust the type of robust error uses, default in Stata is HC1, HC3 is recommended 


#===========================================
# Model Comparison/ Selection              #
#==========================================-
Reg.fit <- lm(y ~ x1 + x2 + x3, data=dat)
Reg.fit.reduced <- lm(y ~ x2 + x3, data=dat)

jtools::summ(Reg.fit, digits=3) #base model has much higher R-squared
jtools::summ(Reg.fit.reduced, digits=3)

anova(Reg.fit.reduced,Reg.fit)
# Sig, indicates inclusion of x1 in reg.fit compared to the reduced model is a significant increase in model fit
# if sig, suggests variable should be included 
AIC(Reg.fit.reduced,Reg.fit)
# AIC is lower for Reg.fit than reduced model, indicating Reg.fit is the better model
broom::glance(Reg.fit)
broom::glance(Reg.fit.reduced)
# can further compare other model metrics

# Can also look at differences in coef estimates
jtools::plot_summs(Reg.fit, Reg.fit.reduced, scale = TRUE, plot.distributions = TRUE)

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
# Validation & Testing Predictive Capacity of Model                               #
#=================================================================================-

# NOTE STILL WORKING ON THIS SECTION

#Calculate metrics for original model 
# RMSE = (ROOT MEAN SQUARED ERROR) square root of average squared difference between actual and predicted
#       Desire to have low
# RMSE penalizes if further from zero
# R-squared error = R-squared how much variance in the DV explained by IV
#       Desire to have higher than .7
#       Coefficient of determination
# MAE = mean absolute error, difference between actual and predicted

# RMSE
Metrics::rmse(dat$y, predict(Reg.fit))
# R-squared
summary(Reg.fit)$r.squared
# MAE
Metrics::mae(dat$y, predict(Reg.fit))

# Can also use this code

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


# Now time to cross validate
# defining training control
# repeated cross-validation value of K is 10 and repetition is 3 times
train_control <- trainControl(method = "repeatedcv",
                              number = 10, repeats = 3)

model.fit <- train(y ~ x1 + x2 + x3, data = dat,
               method = "lm",
               trControl = train_control)

# printing model performance metrics
model.fit
predict(model.fit, dat)
dat$valid.pred.val <- predict(model.fit)

# NOTE that these metrics are comparative, not absolute standardized metrics 
# Also note, predicted from the train model is SAME as just normal linear regression
# So actually not really sure what this stuff down here adds..
