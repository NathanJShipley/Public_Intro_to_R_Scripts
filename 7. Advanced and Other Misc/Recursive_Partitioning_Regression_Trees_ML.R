#==============================================================================
#                              Analysis Repository:                           #
#           Machine Learning: Recursive Partitioning Regression Trees         #      
#==============================================================================

############################################################################## #
############################################################################## #
############################################################################## #
############################################################################## #
############################# In Development ################################# #
############################################################################## #
############################################################################## #
############################################################################## #
############################################################################## #
############################################################################## #

# check these out soon
https://cran.r-project.org/web/views/MachineLearning.html

https://uc-r.github.io/regression_trees

####
 #### 
   #### NOTE TO SELF
          ##### accuracy is must easier to get using categorical data, seems that these types of models are privy towards this type of data

# Some other resources I was using

# https://www.pluralsight.com/guides/explore-r-libraries:-rpart
# https://www.gormanalysis.com/blog/decision-trees-in-r-using-rpart/
# https://www.statmethods.net/advstats/cart.html
# https://www.learnbymarketing.com/tutorials/rpart-decision-trees-in-r/
# https://stackoverflow.com/questions/40080794/calculating-prediction-accuracy-of-a-tree-using-rparts-predict-method
# https://www.datatechnotes.com/2020/10/regression-example-with-rpart-tree.html



#===========================================
# Load Libraries                           #
#==========================================-

library(psych) #has nice describe function for descriptive info
library(tidyverse) #has dplyr and ggplot2
library(ggpubr) #has some more GG plots
library(corrplot) #nice cor plot
library(rpart) # Regression Tree building
library(rpart.plot) #plotting

#library(MASS)

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

#===========================================
# Simulate Data OR LOAD DATA               #
#==========================================-
set.seed(101)

dat <- read.csv("data/insurance.csv", header=T)
#dat <- readxl::read_xlsx("fake.data.xlsx")

glimpse(dat)

#===========================================
# Overview               #
#==========================================-

# Would say that this is mostly exploratory currently
# Playing around with a regression tree function of rpart
# Classification Tree = Categorical outcome
# Regression Tree = continous outcome
# rpart = recursive partitioning for classification 

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
corrplot(cor(dat, use="complete.obs"), order = "hclust", tl.col='black', tl.cex=.75) 


# Scatter plot of two variables
ggplot(dat,aes(bmi, charges)) +
  geom_point() + 
  geom_smooth()

ggplot(dat,aes(age, charges)) +
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


#===========================================
# Pre-processing                           #
#==========================================-

# Developing train and test data
# holdout
set.seed(101)

training.samples <- caret::createDataPartition(dat$charges, p=.8, list=FALSE)

train.dat <- dat[training.samples,]
test.dat <- dat[-training.samples,]
dim(train.dat); dim(test.dat)

# Feature Scaling
# Scale and center numeric features
cols = c('age','children')

pre_proc_val <- caret::preProcess(train.dat[,cols], method = c("center","scale"))

train.dat[,cols] <-  predict(pre_proc_val, train.dat[,cols])
test.dat[,cols] <-  predict(pre_proc_val, test.dat[,cols])

summary(train.dat)


#===========================================
# Build FIRST Regression Trees             #
#==========================================-

# Lets predict medical charges based on age, sex, bmi, # children, smoker, and region
# Would suggest this first pass, good insight into what is important, but not necssarily best
# ALSO Going to build using all data, will look at using the train and test data below
# Method = can be anova, poisson, class, or exp. Class for classification, ANOVA for regression
reg.tree <- rpart(charges ~ age + sex + bmi + children + smoker + region,
                  data = dat,
                  method = "anova")

rpart.plot(reg.tree, type = 4)
# Nice visualization 

print(reg.tree)
# Can get results
# split criteria, #rows in nodes, # misclassified, predictve class, % in

summary(reg.tree)
# CP table
# variable importance
# Description of node and split


# Other diagnostics
printcp(reg.tree)
# dispaly cross validation 
# xerror

plotcp(reg.tree)
# Plot xerror
# improvement in cost complexity
rsq.rpart(reg.tree)
# plot approx R-squared and error


#===========================================
# Regression Trees with different methods  #
#==========================================-
# Lets compare methods 
# Method = can be anova, poisson, class, or exp. Class for classification, ANOVA for regression

reg.tree.a <- rpart(charges ~ age + sex + bmi + children + smoker + region,
                  data = dat,
                  method = "anova")

reg.tree.p <- rpart(charges ~ age + sex + bmi + children + smoker + region,
                  data = dat,
                  method = "poisson")

rpart.plot(reg.tree.a, type = 4)
rpart.plot(reg.tree.p, type = 4)
# Looks like anova is more parsimounous 

#===========================================
# Model tuning  #
#==========================================-

# Can adjust various parameters of the model

reg.tree.v1 <- rpart(charges ~ age + sex + bmi + children + smoker + region,
                    data = dat,
                    maxdepth = 30, minsplit = 20, minbucket = 7,
                    cp = .01)

# Tools for adjusting to overfitting, don't want to be overfitted
# maxdepth = farthest depth/height
# minsplit = smallest # observations that can be split further
# minbucket = smallest number of observations  per bucket

rpart.plot(reg.tree.v1, type = 4)

# Importance of CP
# Complexity Parameter 
# CP is the minimum improvement needed at each node
# A cost complexity function
# Adds the missclassifications at each node, multiple by splits, 
# can get scaled version of CP using printcp

printcp(reg.tree.v1)


#============================ = 
# EXAMPLE OF OVERFITTED MODEL
#============================ =
reg.tree.OVER <- rpart(charges ~ age + sex + bmi + children + smoker + region,
                     data = dat,
                     maxdepth = 5, minsplit = 2, minbucket = 1,
                     cp = .0001)

rpart.plot(reg.tree.OVER, type = 4)


#============================ = 
# EXAMPLE OF OVERFITTED MODEL
#============================ =

prune(fit, cp= )


#













reg.tree.pg <- rpart(charges ~ age + sex + bmi + children + smoker + region,
                    data = dat,
                    method = "class",
                    parms=list(split="gini"))
reg.tree.pi <- rpart(charges ~ age + sex + bmi + children + smoker + region,
                    data = dat,
                    method = "class",
                    parms=list(split="information"))

?split



control=rpart.control(minsplit=30, cp=0.001) 
# minimum number of observations in a node be 30 before split
# split must decrease the overall fit by factor of .001

parms=list(split="information"),
cp = 0.01, minsplit=20, minbucket=7, maxdepth=30)

# pruning 
# Cross validation




#===========================================
# Model Evaluation using train and test    #
#==========================================-

# Lets build a model using the train data set

reg.tree.train <- rpart(charges ~ age + sex + bmi + children + smoker + region,
                  data = train.dat,
                  method = "anova")


predicted_Train <- predict(reg.tree.train, data = train.dat, type = "vector")

table(train.dat$charges, predicted_Train)



test.dat[,cols] <-  predict(pre_proc_val, test.dat[,cols])

summary(train.dat)


















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






############################################################################## #
############################################################################## #
############################################################################## #
############################################################################## #
############################# MUCH MORE TO LEARN ################################# #
############################################################################## #
############################################################################## #
############################################################################## #
############################################################################## #
############################################################################## #

https://cran.r-project.org/web/views/MachineLearning.html

https://uc-r.github.io/regression_trees
