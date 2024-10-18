#==============================================================================
#                              Analysis Repository:                           #
#                               Linear Regression                             #      
#==============================================================================

#===========================================
# Load Required Libraries                 #
#===========================================

library(psych)        # Provides descriptive statistics functions
library(tidyverse)    # Contains dplyr and ggplot2 for data manipulation and visualization
library(olsrr)        # Useful regression functions and plotting tools
library(jtools)       # Functions for summarizing regression output
library(ggstance)     # Provides functions that jtools uses
library(ggpubr)       # Additional ggplot functionalities
library(car)          # Durbin-Watson test for autocorrelation
library(MASS)         # Shapiro-Wilk test for normality
library(lmtest)       # Breusch-Pagan test for homoscedasticity
library(pwr)          # Power analysis functions
library(Metrics)      # For calculating RMSE and MAE
library(caret)        # Cross-validation tools
library(mvnormtest)   # Multivariate normality tests
library(corrplot)     # Correlation plot functions
library(visreg)       # Visualizations of relationships
library(vip)          # Variable importance visualization

#===========================================
# Simulate Data OR LOAD DATA               #
#===========================================

set.seed(101)  # For reproducibility

# Simulate data (uncomment to use)
# x1 <- rnorm(1000, 1, 1)
# x2 <- rnorm(1000, 1, 1)
# x3 <- rnorm(1000, 1, 1)
# y <- (1.5 * x1) - (0.5 * x3) + rnorm(1000, 1, 0.8)
# dat <- data.frame(x1, x2, x3, y)

# Load actual data
dat <- read.csv("data/insurance.csv", header = TRUE)
# Alternatively, load from an Excel file
# dat <- readxl::read_xlsx("fake.data.xlsx")

#===========================================
# Assess Descriptive Statistics            #
#===========================================

# View data structure and first few rows
glimpse(dat)
head(dat)
str(dat)

# Descriptive statistics for the response variable
psych::describe(dat$charges)

# Visualizations for distributions
hist(dat$charges, main = "Distribution of Charges", xlab = "Charges")
ggdensity(dat$charges, title = "Density Plot of Charges")
boxplot(dat$charges, main = "Boxplot of Charges", ylab = "Charges")
ggqqplot(dat$charges, title = "QQ Plot of Charges")

# Preliminary Exploration of Associations
pairs.panels(dat, scale = TRUE)
corrplot(cor(dplyr::select(dat, 1, 3, 4, 7), use = "complete.obs"),
         order = "hclust", tl.col = 'black', tl.cex = 0.75)

# Scatter plot and smoothing line for age vs. charges
ggplot(dat, aes(age, charges)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title = "Charges vs. Age")

# Boxplot for charges by smoker status
boxplot(charges ~ smoker, data = dat, main = "Charges by Smoker Status")

# Scatter plot for age vs. charges by smoker status
ggplot(dat, aes(age, charges, color = smoker)) +
  geom_point() +
  geom_smooth(method = "lm", fullrange = TRUE) +
  facet_wrap(~ smoker) +
  labs(title = "Charges vs. Age by Smoker Status")

# Assess Multivariate Outliers
psych::outlier(dplyr::select(dat, 1, 3, 4, 7))

#===========================================
# Build Model & Get Output                 #
#===========================================

# Fit the linear regression model
Reg.fit <- lm(charges ~ age + sex + bmi + children + smoker + region, data = dat)

# Summary of the regression model
jtools::summ(Reg.fit, digits = 3)

# Additional model diagnostics
olsrr::ols_correlations(Reg.fit)  # Check correlations
summary(Reg.fit)

# Power analysis post-hoc
pwr.f2.test(u = 3, v = 996, f2 = 0.02, sig.level = 0.05)

# Visualization of coefficient uncertainty
jtools::plot_summs(Reg.fit, scale = TRUE, plot.distributions = TRUE)

# Plot estimated relationships
jtools::effect_plot(Reg.fit, pred = bmi, interval = TRUE, plot.points = TRUE)

# Additional relationship visualizations
crPlots(Reg.fit)
visreg(Reg.fit)

# Evaluate variable importance
var.i.plot <- vip(Reg.fit, geom = "point")

#===========================================
# Check Assumptions                        #
#===========================================

# 1.) Normality
ggdensity(dat$charges)  # Check for normal distribution
mshapiro.test(t(dat))   # Assess multivariate normality

# 2.) Linearity 
plot(Reg.fit, 1)  # Residuals vs. Fitted plot

# 3.) Normality of Residuals
plot(Reg.fit, 2)  # QQ plot of residuals
Reg.resid <- studres(Reg.fit)
shapiro.test(Reg.resid)  # Shapiro-Wilk test for residuals

# Assess influence and leverage
olsrr::ols_plot_resid_stud(Reg.fit)  # Studentized residuals
olsrr::ols_plot_cooksd_bar(Reg.fit)  # Cook's distances
olsrr::ols_plot_resid_lev(Reg.fit)    # Leverage vs. residuals

# 4.) Homoscedasticity
plot(Reg.fit, 3)  # Scale-Location plot
bptest(Reg.fit)   # Breusch-Pagan test for homoscedasticity

# 5.) Independence
durbinWatsonTest(Reg.fit)  # Durbin-Watson test for autocorrelation
jtools::summ(Reg.fit, vifs = TRUE, digits = 3)  # Check VIF for multicollinearity

#===========================================
# Model Adjustments if Assumptions Violate #
#===========================================

# If homoscedasticity is violated, use robust standard errors
jtools::summ(Reg.fit, robust = "HC1", digits = 3)
jtools::summ(Reg.fit, robust = "HC2", digits = 3)
jtools::summ(Reg.fit, robust = "HC3", digits = 3)

#===========================================
# Model Comparison/Selection               #
#===========================================

# Fit a reduced model
Reg.fit.reduced <- lm(y ~ x2 + x3, data = dat)

# Compare models
jtools::summ(Reg.fit, digits = 3)
jtools::summ(Reg.fit.reduced, digits = 3)

# ANOVA to compare models
anova(Reg.fit.reduced, Reg.fit)

# AIC comparison
AIC(Reg.fit.reduced, Reg.fit)

# Additional model comparison metrics
broom::glance(Reg.fit)
broom::glance(Reg.fit.reduced)

# Coefficient estimates comparison
jtools::plot_summs(Reg.fit, Reg.fit.reduced, scale = TRUE, plot.distributions = TRUE)

#===========================================
# Plotting                                 #
#===========================================

# Set a theme for plots
theme_set(theme_bw())

# Scatter plots with regression lines
x1.y <- ggplot(dat, aes(x1, y)) +
  geom_point(color = "darkblue") +
  geom_smooth(method = "lm", color = "blue", size = 1) +
  labs(title = "Y by x1", x = "x1 variable", y = "y variable")

x2.y <- ggplot(dat, aes(x2, y)) +
  geom_point(color = "darkblue") +
  geom_smooth(method = "lm", color = "blue", size = 1) +
  labs(title = "Y by x2")

# Plot fitted values to understand the model
dat$pred.val <- predict(Reg.fit)

ggplot(dat, aes(x1, pred.val)) +
  geom_point(color = "darkblue") +
  geom_smooth(method = "lm", color = "blue", size = 1) +
  labs(title = "Fitted Values by x1", x = "X1", y = "Predicted Y")

#==================================================================================
# Validation & Testing Predictive Capacity of Model                               #
#=================================================================================

# Calculate metrics for original model 
# RMSE: Root Mean Squared Error
# R-squared: Coefficient of Determination
# MAE: Mean Absolute Error

# RMSE Calculation
rmse_value <- Metrics::rmse(dat$y, predict(Reg.fit))
cat("RMSE: ", rmse_value, "\n")

# R-squared
r_squared <- summary(Reg.fit)$r.squared
cat("R-squared: ", r_squared, "\n")

# MAE Calculation
mae_value <- Metrics::mae(dat$y, predict(Reg.fit))
cat("MAE: ", mae_value, "\n")

# Create a model fit summary function
MOD_FIT <- function(Observed, Predicted) {
  mse <- mean((Observed - Predicted)^2)
  mae <- caret::MAE(Observed, Predicted)
  rmse <- caret::RMSE(Observed, Predicted)
  r2 <- (cor(Observed, Predicted))^2
  return(cat("MSE: ", mse, "MAE: ", mae, "RMSE: ", rmse, "R-squared: ", r2, "\n"))
}

# Use the fit summary function
MOD_FIT(dat$y, predict(Reg.fit))

# Split the data into training and testing sets
set.seed(101)
trainIndex <- sample(1:nrow(dat), 0.8*nrow(dat))  # 80% for training
train_data <- dat[trainIndex, ]
test_data <- dat[-trainIndex, ]

# Fit the model on the training data
train_model <- lm(y ~ x1 + x2 + x3, data = train_data)

# Validate the model on the test data
predictions <- predict(train_model, newdata = test_data)

# Generate performance metrics for test data
MOD_FIT(test_data$y, predictions)

#===========================================
# Additional Notes:                      #
# - Consider interactions or polynomial terms if needed.  #
# - Cross-validation can be employed for model validation. #
#===========================================