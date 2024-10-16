#==============================================================================
#                              Analysis Repository:                           #
#                              Logistic Regression                            #      
#==============================================================================

#===========================================
# Load Libraries                           #
#==========================================-
library(psych) #has nice describe function for descriptive info
library(tidyverse) #has dplyr and ggplot2
library(jtools) #summ function good to get regression output
library(ggstance) #Provides functions that Jtools uses
library(ggpubr) #has some more GG plots
library(InformationValue) #Important tools to evalaute GLM
library(mvnormtest) #normality

#===========================================
# Simulate Data OR LOAD DATA               #
#==========================================-
set.seed(101)

x1 <- rnorm(6000,1,.1)
y <- rnorm(6000,0,0)
dat.1 <- data.frame(x1,y)

x1 <- rnorm(4000,1.4,.1)
y <- rnorm(4000,1,0)
dat.2 <- data.frame(x1,y)

# bind the two data sets
dat <- bind_rows(dat.1,dat.2)

# this final data will have 400 ones and 600 zeros as the DV
# Probability of 40%, which we will see in descriptive 

#dat <- read.csv("data.csv", header=T)
#dat <- readxl::read_xlsx("fake.data.xlsx")


#===========================================
# Overview                                 #
#==========================================-

# Logistic regression is used to analyze binomial DVs
# It uses the generalized linear model, so uses maximum liklihood rather than OLS
# Many similarities to linear regression, but some differences
# Most of these differences are covered below
# This code digs deeper into understanding how log reg can be used as a classifier! 

#===========================================
# Assess Descriptives                      #
#==========================================-

# Mean, SD, Median, Min, Max, Skewness, and Kurtosis for y
psych::describe(dat$y)
# can see mean is .4, we have 400

summary(dat)

# Visualizations for distributions
hist(dat$y)
ggdensity(dat$y)
ggqqplot(dat$y)

# Preliminary Exploration of Associations
pairs.panels(dat, scale=TRUE)

# Scatter plot of two variables
ggplot(dat,aes(x1,y)) +
  geom_point() + 
  geom_smooth()

#===========================================
# Build Model & Get Output                 #
#==========================================-
Reg.fit <- glm(y ~ x1,  data=dat, family = "binomial")

jtools::summ(Reg.fit, digits=3)
# First Model Output to get F, p, R-2, est, se, and t
# Note that coefs from logistic are LOGITS
# So as x1 goes up, the logit of y goes up 1.070

# Also note Cragg_Uhler R-squared is also Naglekerke's 

# Can calculate the odds increase as the exponential of the logit
exp(Reg.fit$coefficients)
# returns the ODDS of each coef
# Or get the odd for each variable directly
exp(39.435) # odds = 128.1242
# can now calcualte the probabilty by converting the odds
# Prob = Odds / (1 + Odds)
(exp(39.435)/(1+exp(39.435))) #Prob = 1

# Can create a function that automates the exp to odd to prob
logit2prob <- function(logit){
  odds <- exp(logit)
  prob <- odds / (1 + odds)
  return(prob)
}

logit2prob(Reg.fit$coefficients)
# now can see the probability increase each variable contributes to the DV
# See below in diagnostics to get more information on evaluating if the model is good 

# It should also be mentioned that a linear change in logit DOES NOT lead to a linear change in probabiltiy  
# Have to calculate the logit unit change then calculate prob
# First number in intercept, then add logit based on value of X
logit.low <- -49.733 + (.72*41.064) # Low value of X1 (-2 SD) > x1 = .72
logit.avg <- -49.733 + (1.16*41.064) # Mean value of X1 > x1 = 1.16
logit.high <- -49.733 + (1.6*41.064) # High value of X1 (+2 SD) > x1 = 1.6

# round out what the probabilities area
round(logit2prob(logit.low),3) # 0%
round(logit2prob(logit.avg),3) # 10%
round(logit2prob(logit.high),3) # 99%

# Now calculate the CHANGE in probability from low to average, and average to high values of X1
delta.low.avg <- round(logit2prob(logit.avg),3) - round(logit2prob(logit.low),3)
delta.avg.high <- round(logit2prob(logit.high),3) - round(logit2prob(logit.avg),3)

# Change in % between levels of X1
delta.low.avg # about 10%
delta.avg.high # about 90%

# Some other quick tools to visualize the model
# Visualize coef uncertainty 
jtools::plot_summs(Reg.fit, scale = TRUE, plot.distributions = TRUE, inner_ci_level = .9)
# Quickly plot estimated relationships 
jtools::effect_plot(Reg.fit, pred = x1, interval = TRUE, plot.points = TRUE)

#===========================================
# Check Assumptions                        #
#==========================================-

# Because the regression uses logits, it is helpful to calculate some data to assess assumptions
# The code here will calculate some metrics

model.data <- broom::augment(Reg.fit) %>% 
  mutate(index = 1:n()) 

ggplot(model.data, aes(index, .fitted)) + 
  geom_point(aes(color = y), alpha = .5) +
  theme_bw()
# For example, this graph shows the fitted logit values for each indexed value

# 1.) Linearity 
# Are the IV linearly related to the LOGIT values
ggplot(model.data, aes(x1, .fitted)) + 
  geom_point(aes(color = y), alpha = .5) +
  geom_smooth(method = "loess") + 
  theme_bw()

# Fitted line should be roughly linear
# If there is a pattern, may indicate non-linearity
# If pattern, then may need to run a polynomial regression

# can also look at 
plot(Reg.fit,1)

# Assess multivariate normality 
mshapiro.test(t(dat))


# 2.) Normality of Residuals and no extreme values
plot(Reg.fit,2)
# QQ plot, should roughly be a straight line

# Residuals
ggplot(model.data, aes(index, .std.resid)) + 
  geom_point(aes(color = y), alpha = .5) +
  theme_bw()

# Compare fitted to residual and Deviance residuals
dat$residual <- residuals(Reg.fit, type = "pearson")
dat$res.deviance <- residuals(Reg.fit, type = "deviance")

ggplot(dat, aes(pred.val, res.deviance)) + 
  geom_point(aes(color = x1), alpha = .5) +
  theme_bw()

ggplot(dat, aes(pred.val, res.deviance)) + 
  geom_point(aes(color = x1), alpha = .5) +
  theme_bw()
# Want both of these graphs to look similar!!!!!!

# Cooks
ggplot(model.data, aes(index, .cooksd)) + 
  geom_point(aes(color = y), alpha = .5) +
  theme_bw()

# Assess Multivariate Outliers
psych::outlier(dat)

# 3.) Independent (Multicollinearity)
jtools::summ(Reg.fit, vifs=TRUE, digits=3)
# ideal VIF is less than 4, with anything less than 10 considered acceptable (Hair et al., 2010)
# If high VIF or high correlations, may have to remove factors from the model


#====================== =
# # Overdispersion # #
#====================== =

# data admit more variability than expected based on distributions
# if over dispersion is present, then SE and stats will be distorted
# IF THERE IS overdispersion, then use quasibinomial distrubtion in GLM

# IF this ratio is MUCH larger than 1, then indciate overdispersion 
deviance(Reg.fit)/df.residual(Reg.fit) 

# Can also formally evaluate with a goodness of fit
with(Reg.fit, cbind(res.deviance = deviance, df = df.residual,
                    p = pchisq(deviance, df.residual, lower.tail=FALSE)))
# If sig, indicates not a good fit and evidence for over dispersion

# IF there is overdispersion, then using a quasibinomial may be necessary
# Can also compare the binomial to a quasi directly, if better fit then use quasi
Reg.fit.quasi <- glm(y ~ x1,  data=dat, family = "quasibinomial")
anova(Reg.fit,Reg.fit.quasi)


#===========================================
# Plotting                                 #
#==========================================-

theme_set(
  theme_bw()
)

x1.y <- ggplot(dat,aes(x1,y)) +
  geom_point(color="darkblue") + 
  geom_smooth(method="gam",color="blue",size=1) + 
  ggtitle("Y by x1") + theme(plot.title = element_text(hjust = 0.5)) + 
  xlab("x1 variable") + 
  ylab("y variable")

x1.y


#===========================================
# Additional model diagnostics and info    #
#==========================================-

#====================== =
# # Accuracy # #
#====================== =
# Can also calculate predicted scores, which are PROBABILITES, for each data pointas a 1 or 0
Reg.probs <- predict(Reg.fit,type = "response")
Reg.probs[1:5] #first 5 should be close to zero, which is correct

# Can also save predicted scores for each data point
dat$pred.prob <- predict(Reg.fit,type = "response")
# And can classify probabilities into respective classes
# can use a prob of 50% to indicate a 0 or 1
dat$classified <- ifelse(dat$pred.prob > .5, 1,0)

# Can assess accuracy of the model!!!
misClasificError <- mean(dat$classified != dat$y)
print(paste('Accuracy',1-misClasificError))
# indicates model has a 97.85% accuracy 
mean(dat$classified == dat$y)
# another way to get the same value

# Note that we can identify a optimal cutoff value, rather than 50%
# This reduces misclassifications error

optCutOff <- optimalCutoff(dat$y, dat$pred.prob)[1] 
optCutOff
# This suggests a cut off of 0.41 is more optimal than .5

# lets re-classify and evaluate accuracy
dat$classified.2 <- ifelse(dat$pred.prob > .4, 1,0)
mean(dat$classified.2 == dat$y) # 97.97% accr

# We can also evaluate overall misclassification 
misClassError(dat$y, dat$pred.prob, threshold = optCutOff)
# in this case error is .0203, which is 100% minus accuracy 
# This is the error irrespective of either 1 or 0, lower the better

#====================== =
# #   ROC    # #
#====================== =

# we also have ROC
# Receiver operating charactersitcs curve 
# traces true positives predicted by logit
# Should rise steeply, then cut off, more area under the better
plotROC(dat$y, dat$pred.prob)

#====================== =
# #   Concordance    # #
#====================== =

# Probability of ALL actual 1s should be greater than prob of all 0s
# actual positives compared to ALL negatives
# if so, then model is concordant and highly reliable
Concordance(dat$y, dat$pred.prob)

#====================== =
# #   Confusion Matrix    # #
#====================== =

# Create a confusion matrix-like table
confusionMatrix(dat$y, dat$pred.prob, threshold = optCutOff)
# Columns are actual
# Rows are predicted

#     True Negative    |    False Negative
# -------------------------------------------
#     False Positive   |    True Positive

#====================== =
# #   Sensitivity and Specificity    # #
#====================== =

# Sensitivity (TRUE POSITIVES) 
# Percentage of actual 1s predicted to be 1 by model
sensitivity(dat$y, dat$pred.prob, threshold = optCutOff)
# True Positives

# Specificity is the percentage of actual 0s predicted to be 0 by the model
specificity(dat$y, dat$pred.prob, threshold = optCutOff)
# True Negatives


#===========================================
# Do everything but with TEST and TRAIN data #
#==========================================-

training.samples <- dat$y %>%
  caret::createDataPartition(p=.8, list= F, times = 1)
# Create training sample, stratified across the DV, 80% training, done once
train.dat <- dat[training.samples,]
test.dat <- dat[-training.samples,]

trained.fit <- glm(y ~ x1,  data=train.dat, family = "binomial")
predicted <- predict(trained.fit, test.dat, type="response")  # predicted scores

optCutOff.2 <- optimalCutoff(test.dat$y, predicted)[1] 
optCutOff.2

# mis Classified
misClassError(test.dat$y, predicted, threshold = optCutOff.2)

plotROC(test.dat$y, predicted)
Concordance(test.dat$y, predicted)

confusionMatrix(test.dat$y, predicted, threshold = optCutOff.2)

sensitivity(test.dat$y, predicted, threshold = optCutOff.2)
specificity(test.dat$y, predicted, threshold = optCutOff.2)

