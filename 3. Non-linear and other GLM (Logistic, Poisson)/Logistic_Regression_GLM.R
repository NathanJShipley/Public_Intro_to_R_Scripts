#==============================================================================
#                              Analysis Repository:                           #
#                              Logistic Regression                            #      
#==============================================================================

#===========================================
# Load Libraries                           #
#===========================================

# Load necessary libraries for analysis
library(psych)           # For descriptive statistics
library(tidyverse)      # For data manipulation and visualization
library(jtools)         # For regression output summarization
library(ggstance)       # For horizontal ggplot2 functions
library(ggpubr)         # For additional ggplot2 enhancements
library(InformationValue) # For evaluation of GLMs
library(mvnormtest)     # For normality tests


#===========================================
# Simulate Data OR LOAD DATA               #
#===========================================

# Set seed for reproducibility
set.seed(101)

# Simulate data for two groups
# Group 1: 6000 observations
x1_group1 <- rnorm(6000, 1, 0.1) # Predictor variable with mean 1
y_group1 <- rnorm(6000, 0, 0)     # Response variable centered around 0
dat.1 <- data.frame(x1 = x1_group1, y = y_group1)

# Group 2: 4000 observations
x1_group2 <- rnorm(4000, 1.4, 0.1) # Predictor variable with mean 1.4
y_group2 <- rnorm(4000, 1, 0)      # Response variable centered around 1
dat.2 <- data.frame(x1 = x1_group2, y = y_group2)

# Combine the two datasets into one
dat <- bind_rows(dat.1, dat.2)

# This final dataset will have a binary response variable (DV)
# Probability of 40%, as we will see in the descriptive statistics

# Uncomment to load actual data
# dat <- read.csv("data.csv", header = TRUE)
# dat <- readxl::read_xlsx("fake.data.xlsx")


#===========================================
# Overview                                 #
#===========================================

# Logistic regression is used for analyzing binomial dependent variables (DVs).
# It utilizes a generalized linear model (GLM), employing maximum likelihood estimation instead of ordinary least squares (OLS).
# While similar to linear regression, there are distinct differences highlighted throughout this code.
# This analysis focuses on how logistic regression can serve as a classifier.


#===========================================
# Assess Descriptives                      #
#===========================================

# Calculate descriptive statistics for the response variable 'y'
psych::describe(dat$y)
# Examine mean, SD, median, min, max, skewness, and kurtosis for y

# Summary of the dataset
summary(dat)

# Visualizations for the distribution of y
hist(dat$y, main = "Histogram of y", xlab = "y values", col = "lightblue")
ggdensity(dat$y) + ggtitle("Density Plot of y")
ggqqplot(dat$y) + ggtitle("QQ Plot of y")  # QQ plot to assess normality

# Preliminary exploration of associations among variables
pairs.panels(dat, scale = TRUE, main = "Scatterplot Matrix")  # Correlation matrix

# Scatter plot to visualize the relationship between x1 and y
ggplot(dat, aes(x = x1, y = y)) +
  geom_point(color = "darkblue") + 
  geom_smooth(method = "lm", color = "red") + 
  ggtitle("Scatter Plot of y vs x1") +
  xlab("x1 variable") + 
  ylab("y variable")


#===========================================
# Build Model & Get Output                 #
#===========================================

# Fit logistic regression model
Reg.fit <- glm(y ~ x1, data = dat, family = "binomial")

# Summarize the model output
jtools::summ(Reg.fit, digits = 3)

# Exponentiate coefficients to interpret as odds ratios
odds_ratios <- exp(Reg.fit$coefficients)
odds_ratios

# Example: Calculating probability from logit for a specific coefficient
odds_example <- exp(Reg.fit$coefficients[1]) # First coefficient
prob_example <- logit2prob(odds_example)
prob_example  # Probability for the example odds

# Define function to convert logit to probability
logit2prob <- function(logit) {
  odds <- exp(logit)
  prob <- odds / (1 + odds)
  return(prob)
}

# Apply function to all coefficients
logit_probs <- logit2prob(Reg.fit$coefficients)
logit_probs  # Display the probabilities associated with each coefficient

# Evaluate the impact of changing x1 on probability
# Change in logits for low, average, and high values of x1
logit.low <- -49.733 + (0.72 * 41.064)  # Low value of x1 (-2 SD)
logit.avg <- -49.733 + (1.16 * 41.064)  # Mean value of x1
logit.high <- -49.733 + (1.6 * 41.064)   # High value of x1 (+2 SD)

# Round probabilities
round(logit2prob(logit.low), 3)  # Probability for low x1
round(logit2prob(logit.avg), 3)  # Probability for average x1
round(logit2prob(logit.high), 3)  # Probability for high x1

# Calculate changes in probabilities between levels of x1
delta.low.avg <- round(logit2prob(logit.avg), 3) - round(logit2prob(logit.low), 3)
delta.avg.high <- round(logit2prob(logit.high), 3) - round(logit2prob(logit.avg), 3)

# Display change in probabilities
delta.low.avg  # Change from low to average
delta.avg.high  # Change from average to high

# Visualizations for model diagnostics
# Visualize coefficient uncertainty
jtools::plot_summs(Reg.fit, scale = TRUE, plot.distributions = TRUE, inner_ci_level = .9)

# Plot estimated relationships with confidence intervals
jtools::effect_plot(Reg.fit, pred = x1, interval = TRUE, plot.points = TRUE)


#===========================================
# Check Assumptions                        #
#===========================================

# Assess assumptions of the logistic regression model

# Generate augmented data with fitted values
model.data <- broom::augment(Reg.fit) %>% 
  mutate(index = 1:n())

# Plot fitted values against index
ggplot(model.data, aes(index, .fitted)) + 
  geom_point(aes(color = y), alpha = 0.5) +
  theme_bw() +
  ggtitle("Fitted Values vs. Index")

# 1.) Linearity
# Check if independent variables (IVs) are linearly related to the logit values
ggplot(model.data, aes(x1, .fitted)) + 
  geom_point(aes(color = y), alpha = 0.5) +
  geom_smooth(method = "loess") + 
  theme_bw() +
  ggtitle("Linearity Check: Fitted Values vs. x1")

# Assess normality of residuals and identify potential outliers
plot(Reg.fit, 1)  # Residuals vs. fitted values

# Check multivariate normality
mshapiro.test(t(dat))

# 2.) Normality of Residuals
# QQ plot for residuals
plot(Reg.fit, 2)

# Residuals plot
ggplot(model.data, aes(index, .std.resid)) + 
  geom_point(aes(color = y), alpha = 0.5) +
  theme_bw() +
  ggtitle("Standardized Residuals vs. Index")

# Assess residuals against predicted values
dat$residual <- residuals(Reg.fit, type = "pearson")
dat$res.deviance <- residuals(Reg.fit, type = "deviance")

# Plot deviance residuals against predicted values
ggplot(dat, aes(pred.val, res.deviance)) + 
  geom_point(aes(color = x1), alpha = 0.5) +
  theme_bw() +
  ggtitle("Deviance Residuals vs. Predicted Values")

# Cook's distance plot to identify influential data points
ggplot(model.data, aes(index, .cooksd)) + 
  geom_point(aes(color = y), alpha = 0.5) +
  theme_bw() +
  ggtitle("Cook's Distance")

# Assess multivariate outliers
psych::outlier(dat)

# 3.) Independence (Multicollinearity)
jtools::summ(Reg.fit, vifs = TRUE, digits = 3)  # Check Variance Inflation Factors (VIF)
# VIF should ideally be less than 4; anything below 10 is generally acceptable (Hair et al., 2010)


#====================== =
# # Overdispersion # #
#====================== =
# Check for overdispersion in the model
# Overdispersion indicates more variability than expected based on the distribution
overdispersion_ratio <- deviance(Reg.fit) / df.residual(Reg.fit) 

# Conduct goodness-of-fit test for overdispersion
goodness_of_fit <- with(Reg.fit, cbind(res.deviance = deviance, df = df.residual,
                                       p = pchisq(deviance, df.residual, lower.tail = FALSE)))

# If p-value is significant, indicates a poor fit and evidence of overdispersion
if (goodness_of_fit[3] < 0.05) {
  print("Evidence of overdispersion present!")
} else {
  print("No evidence of overdispersion.")
}

# Evaluate model fit using the AIC criterion
AIC_value <- AIC(Reg.fit)  # Lower values indicate a better fit

# Print AIC value
cat("AIC value for the model is:", AIC_value)

# 4.) Goodness-of-fit
# Evaluate goodness-of-fit with the Hosmer-Lemeshow test
hoslem.test(Reg.fit$y, fitted(Reg.fit))

# Check residuals of the model
plot(Reg.fit)


#===========================================
# Model Predictions                        #
#===========================================

# Make predictions
predictions <- predict(Reg.fit, newdata = dat, type = "response")  # Predicted probabilities

# Convert probabilities to binary classification (threshold of 0.5)
predicted_classes <- ifelse(predictions > 0.5, 1, 0)

# Create confusion matrix
confusion_matrix <- table(Predicted = predicted_classes, Actual = dat$y)

# Calculate accuracy
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
cat("Model Accuracy:", accuracy)

# Precision and recall
precision <- confusion_matrix[2, 2] / sum(confusion_matrix[2, ])
recall <- confusion_matrix[2, 2] / sum(confusion_matrix[, 2])

# Display precision and recall
cat("Precision:", precision, "Recall:", recall)

# AUC-ROC curve for model performance
roc_curve <- pROC::roc(dat$y, predictions)
pROC::plot.roc(roc_curve)
cat("AUC:", roc_curve$auc)


#===========================================
# Visualizations                          #
#===========================================

# Plot ROC curve
ggplot(data = as.data.frame(roc_curve)) +
  geom_line(aes(x = 1 - specificity, y = sensitivity), color = "blue") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
  ggtitle("ROC Curve") +
  xlab("1 - Specificity") +
  ylab("Sensitivity")

