#==============================================================================
#                              Analysis Repository:                           #
#                               Linear Regression                             #      
#==============================================================================

#===========================================
# Load Libraries                           #
#==========================================-
library(psych) #has nice describe function for descriptive info
library(dplyr) #pipes and other formatting tools
#library(olsrr) #Some good regression functions and plotting tools
#library(jtools) #summ function good to get regression output
library(ggplot2) #plot funtions 
library(ggpubr) #has some more GG plots
#library(car) #Durbin-Watson test
#library(MASS) #Shapiro-Wilk test
#library(lmtest) #Breusch-Pagan test

#===========================================
# Simulate Data OR LOAD DATA               #
#==========================================-
set.seed(101)

x1 <- rnorm(1000,1,1)
x2 <- rnorm(1000,1,1)
x3 <- I(x1^2)
x4 <- I(x1^3)
# NOTE need to do I(x^2) to include ONLY these variables
# if was just x^2, it would include these variables and ALL interactions up to X
y.quad <- (.2*x1) + (1.5*x3) + rnorm(1000,1,.8)
y.cubic <- (.2*x1) + (.5*x3) + (4*x4) + rnorm(1000,1,.8)

dat <- data.frame(x1,x2,x3,x4,y.quad,y.cubic)

#dat <- read.csv("data.csv", header=T)
#dat <- readxl::read_xlsx("fake.data.xlsx")


#===========================================
# Overview                                 #
#==========================================-

# If the relationship between an X and Y is not linear, then there may exist a polynomial relationship
# Here, I refer to quadratic and cubic relationships
# To analyze these relationships in OLS regression, we simply add a link function
# Really, all we do is model both a linear form and a polynomial form of the independent variable
# This code covers how to conduct such an analysis
# Note that further details on interpreting the model, refer to linear regression 


#===========================================
# Assess Descriptives                      #
#==========================================-

# Mean, SD, Median, Min, Max, Skewness, and Kurtosis for y
psych::describe(dat$y.quad)
psych::describe(dat$y.cubic)
# Check for no extreme skewness or kurtosis, if so, may have to perform other analyses

# Visualizations for distributions
hist(dat$y.quad)
ggdensity(dat$y.quad)
boxplot(dat$y.quad)
ggqqplot(dat$y.quad)

# Preliminary Exploration of Associations
pairs.panels(dat, scale=TRUE)
# Can see that x1 has a quadratic with y.quad
# can also see x1 has cubic with y.cubic

# Assess Multivariate Outliers
psych::outlier(dat)

# Scatter plot of two variables
# but if we look at relationship as linear, not a good fit
ggplot(dat,aes(x1,y.quad)) +
  geom_point() + 
  geom_smooth(method="lm")

#=================================================================
# Build models and test if Polynomial is needed                  #
#================================================================-
Reg.quad.notfit <- lm(y.quad ~ x1 + x2, data=dat)
Reg.quad.fit <- lm(y.quad ~ x1 + x2 + x3, data=dat)

Reg.cubic.notfit <- lm(y.cubic ~ x1 + x2, data=dat)
Reg.cubic.partialfit <- lm(y.cubic ~ x1 + x2 + x3, data=dat)
Reg.cubic.fit <- lm(y.cubic ~ x1 + x2 + x3 + x4, data=dat)

# Can compare model estimates with and without quadratic 
jtools::summ(Reg.quad.notfit, digits=3)
jtools::summ(Reg.quad.fit, digits=3)
# see overall model improvement when include the quadratic variable

# Can plot differences with and without quadratic
plot(Reg.quad.notfit,1)
plot(Reg.quad.fit,1)
# See that when not fitted with polynomial, that fit is not good

# Can formally test if including polynomial improves model fit
anova(Reg.quad.notfit,Reg.quad.fit)
# See that fit is MUCH better with polynomial included
# Have justification for modeling with the polynomial

# Can evaluate similar relationship with CUBIC 

jtools::summ(Reg.cubic.notfit, digits=3)
jtools::summ(Reg.cubic.partialfit, digits=3)
jtools::summ(Reg.cubic.fit, digits=3)
plot(Reg.cubic.notfit,1)
plot(Reg.cubic.partialfit,1)
plot(Reg.cubic.fit,1)

anova(Reg.cubic.notfit,Reg.cubic.partialfit)
anova(Reg.cubic.partialfit,Reg.cubic.fit)
# can see the partial fit is better than no cubic
# also see full cubic is better than partial
# all point to needing to model quadratic relationship 

# If evidence supports including a polynomial, simply include this variable
# Then perform linear regression as normal

# can create quadratic and cubic in data sets like
# dat$x1.2 <- dat$x1^2


#===========================================
# Plotting                                 #
#==========================================-

theme_set(
  theme_bw()
)

x1.y <- ggplot(dat,aes(x1,y.quad)) +
  geom_point(color="darkblue") + 
  geom_smooth(method="gam",color="blue",size=1) + 
  ggtitle("Y by x1") + theme(plot.title = element_text(hjust = 0.5)) + 
  xlab("x1 variable") + 
  ylab("y variable")

x1.y


# Can plot fitted values as well
# Measure of accuracy
dat$pred.val.quad <- predict(Reg.quad.fit)
dat$pred.val.cub <- predict(Reg.cubic.fit)

ggplot(dat,aes(pred.val.quad,y.quad)) +
  geom_point(color="darkblue") + 
  geom_smooth(method="lm",color="blue",size=1) + 
  ggtitle("Quadratic Pred") + theme(plot.title = element_text(hjust = 0.5)) + 
  xlab("Predicted") + 
  ylab("Actual")

ggplot(dat,aes(pred.val.cub,y.cubic)) +
  geom_point(color="darkblue") + 
  geom_smooth(method="lm",color="blue",size=1) + 
  ggtitle("Cubic Pred") + theme(plot.title = element_text(hjust = 0.5)) + 
  xlab("Predicted") + 
  ylab("Actual")
