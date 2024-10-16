#==============================================================================
#                              Analysis Repository:                           #
#                       Machine Learning: Random Forests                      #      
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

https://www.google.com/search?q=randomForest+r&rlz=1C1GCEA_enUS966US966&oq=randomforest+r+&aqs=chrome.0.69i59j0i512l4j69i60l3.3264j0j4&sourceid=chrome&ie=UTF-8


#===========================================
# Load Libraries                           #
#==========================================-

library(psych) #has nice describe function for descriptive info
library(tidyverse) #has dplyr and ggplot2
library(ggpubr) #has some more GG plots
library(corrplot) #nice cor plot
library(randomForest) # Randomforest building



#library(rpart) # Regression Tree building
#library(rpart.plot) #plotting

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
# Playing around with RANDOM FORESTS



# Classification Tree = Categorical outcome
# Regression Tree = continuous outcome
# 

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
# Build FIRST Random Forest                #
#==========================================-

# Lets predict medical charges based on age, sex, bmi, # children, smoker, and region
# Would suggest this first pass, good insight into what is important, but not necssarily best

RF_model.fit <- randomForest(charges ~ age + sex + bmi + children + smoker + region,
                             data = train.dat)

summary(RF_model.fit)
plot(RF_model.fit)
legend(colnames(rf_model$err.rate))
# Can see performance of the model

#================ = 
# Get importance
#================ =
importance    <- importance(RF_model.fit)
varImportance <- data.frame(Variables = row.names(importance), 
                            Importance = round(importance[ ,'IncNodePurity'],2))

# Create a rank variable based on importance
rankImportance <- varImportance %>%
  mutate(Rank = paste0('#',dense_rank(desc(Importance))))

# Use ggplot2 to visualize the relative importance of variables
ggplot(rankImportance, aes(x = reorder(Variables, Importance), 
                           y = Importance, fill = Importance)) +
  geom_bar(stat='identity') + 
  geom_text(aes(x = Variables, y = 0.5, label = Rank),
            hjust=0, vjust=0.55, size = 4, colour = 'red') +
  labs(x = 'Variables') +
  coord_flip()


#================ = 
# Predict and Evaluate
#================ =

# Predict using the test set
test.dat$prediction <- predict(RF_model.fit, test.dat)

mse = mean((test.dat$charges - test.dat$prediction)^2)
mae = caret::MAE(test.dat$charges, test.dat$prediction)
rmse = caret::RMSE(test.dat$charges, test.dat$prediction)

cat("MSE: ", mse, "MAE: ", mae, " RMSE: ", rmse)

# Plot predicted to actual charges
ggplot(test.dat, aes(x=charges, y=prediction)) +
  geom_point() + 
  geom_smooth()

# Look how prediction and charges differ
ggplot(test.dat, aes(x=bmi)) +
  geom_smooth(aes(y=charges), color="blue") + 
  geom_smooth(aes(y=prediction), color="red")





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

https://www.google.com/search?q=randomForest+r&rlz=1C1GCEA_enUS966US966&oq=randomforest+r+&aqs=chrome.0.69i59j0i512l4j69i60l3.3264j0j4&sourceid=chrome&ie=UTF-8
