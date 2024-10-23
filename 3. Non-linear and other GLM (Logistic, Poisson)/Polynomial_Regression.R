#==============================================================================
#                              Analysis Repository:                           #
#                               Linear Regression                             #      
#==============================================================================

#===========================================
# Load Required Libraries                 #
#===========================================
# Load libraries essential for data manipulation, statistical analysis, and visualization.
library(psych)        # Contains functions for descriptive statistics (e.g., describe)
library(dplyr)        # Offers data manipulation functions, including piping (%>%)
# library(olsrr)      # (Optional) Contains functions for regression diagnostics and plotting
# library(jtools)     # (Optional) Useful for summarizing regression output
library(ggplot2)      # Provides extensive visualization capabilities
library(ggpubr)       # Contains additional ggplot functions for enhanced plotting
# library(car)        # (Optional) Contains functions for regression diagnostics, including the Durbin-Watson test
# library(MASS)       # (Optional) Contains functions for normality tests like Shapiro-Wilk
# library(lmtest)     # (Optional) Contains functions for hypothesis testing in linear models, including Breusch-Pagan test

#===========================================
# Simulate Data OR LOAD DATA               #
#===========================================
set.seed(101) # Set seed for reproducibility of random numbers

# Generate independent variables
x1 <- rnorm(1000, mean = 1, sd = 1)  # Normal distribution, mean = 1, sd = 1
x2 <- rnorm(1000, mean = 1, sd = 1)  # Another independent variable
x3 <- I(x1^2)                        # Quadratic transformation of x1
x4 <- I(x1^3)                        # Cubic transformation of x1

# Create dependent variables with polynomial relationships and noise
y.quad <- (.2 * x1) + (1.5 * x3) + rnorm(1000, mean = 1, sd = 0.8)  # Quadratic relationship
y.cubic <- (.2 * x1) + (.5 * x3) + (4 * x4) + rnorm(1000, mean = 1, sd = 0.8)  # Cubic relationship

# Combine all variables into a data frame
dat <- data.frame(x1, x2, x3, x4, y.quad, y.cubic)

# Uncomment to load data from external sources
# dat <- read.csv("data.csv", header = TRUE)
# dat <- readxl::read_xlsx("fake.data.xlsx")

#===========================================
# Overview                                 #
#===========================================
# This section discusses the analysis of non-linear relationships using polynomial regression.
# Polynomial relationships can be modeled by adding polynomial terms of independent variables.
# This code demonstrates how to identify and analyze both quadratic and cubic relationships.

# To learn more about linear regression interpretation, refer to relevant literature.

#===========================================
# Assess Descriptive Statistics             #
#===========================================
# Obtain descriptive statistics for the dependent variables
psych::describe(dat$y.quad)  # Descriptive stats for quadratic dependent variable
psych::describe(dat$y.cubic)  # Descriptive stats for cubic dependent variable

# Visualizations for understanding the distributions
hist(dat$y.quad, main = "Histogram of y.quad", xlab = "y.quad")  # Histogram for y.quad
ggdensity(dat$y.quad, main = "Density Plot of y.quad")  # Density plot for y.quad
boxplot(dat$y.quad, main = "Boxplot of y.quad")  # Boxplot for y.quad
ggqqplot(dat$y.quad, main = "QQ Plot of y.quad")  # QQ plot for y.quad to check normality

# Preliminary Exploration of Associations
pairs.panels(dat, scale = TRUE)
# The pairs.panels function visually represents relationships between variables.
# Observe that x1 shows a quadratic relationship with y.quad and cubic with y.cubic.

# Assess Multivariate Outliers
psych::outlier(dat)  # Identify potential outliers in the dataset

# Scatter plot to visualize the relationship between x1 and y.quad
# A linear fit may not adequately describe the relationship.
ggplot(dat, aes(x1, y.quad)) +
  geom_point(color = "darkblue") + 
  geom_smooth(method = "lm", color = "red", size = 1) +
  ggtitle("Scatter plot of y.quad vs. x1") +
  xlab("x1") + 
  ylab("y.quad") 

#=================================================================
# Build Models and Test for Polynomial Relationships             #
#=================================================================
# Fit linear regression models to assess if polynomial terms improve the model.

# Fit a linear model without polynomial terms
Reg.quad.notfit <- lm(y.quad ~ x1 + x2, data = dat)

# Fit a linear model with quadratic term
Reg.quad.fit <- lm(y.quad ~ x1 + x2 + x3, data = dat)

# Fit models for cubic relationship
Reg.cubic.notfit <- lm(y.cubic ~ x1 + x2, data = dat)
Reg.cubic.partialfit <- lm(y.cubic ~ x1 + x2 + x3, data = dat)  # Quadratic term only
Reg.cubic.fit <- lm(y.cubic ~ x1 + x2 + x3 + x4, data = dat)  # Full cubic model

# Compare models to assess the effect of including polynomial terms
jtools::summ(Reg.quad.notfit, digits = 3)  # Summary of the non-fitted quadratic model
jtools::summ(Reg.quad.fit, digits = 3)      # Summary of the fitted quadratic model

# Assess model performance visually
plot(Reg.quad.notfit, which = 1)  # Residuals vs Fitted plot for non-fitted model
plot(Reg.quad.fit, which = 1)      # Residuals vs Fitted plot for fitted model

# Perform ANOVA to formally test if polynomial terms improve model fit
anova(Reg.quad.notfit, Reg.quad.fit)  # Compare models to evaluate fit improvement

# Evaluate cubic relationships similarly
jtools::summ(Reg.cubic.notfit, digits = 3)  
jtools::summ(Reg.cubic.partialfit, digits = 3)
jtools::summ(Reg.cubic.fit, digits = 3)

# Visualize residuals for cubic models
plot(Reg.cubic.notfit, which = 1)
plot(Reg.cubic.partialfit, which = 1)
plot(Reg.cubic.fit, which = 1)

# Conduct ANOVA for cubic models
anova(Reg.cubic.notfit, Reg.cubic.partialfit)
anova(Reg.cubic.partialfit, Reg.cubic.fit)

# Summarize findings
# If model fit improves significantly with polynomial terms, include them in your analysis.

# To create new variables directly in the dataset for further modeling:
# dat$x1.2 <- dat$x1^2  # Create squared term
# dat$x1.3 <- dat$x1^3  # Create cubed term

#===========================================
# Plotting Results                        #
#===========================================
# Set ggplot theme for consistency in plots
theme_set(theme_bw())

# Visualize the relationship between x1 and y.quad using fitted values
x1.y <- ggplot(dat, aes(x1, y.quad)) +
  geom_point(color = "darkblue") + 
  geom_smooth(method = "gam", color = "blue", size = 1) + 
  ggtitle("y.quad vs. x1 with Fitted Curve") + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  xlab("x1 variable") + 
  ylab("y.quad variable")

print(x1.y)  # Display the plot

# Plot predicted values for quadratic and cubic relationships
dat$pred.val.quad <- predict(Reg.quad.fit)  # Predicted values for quadratic model
dat$pred.val.cub <- predict(Reg.cubic.fit)  # Predicted values for cubic model

# Visualization for predicted vs actual values for quadratic model
ggplot(dat, aes(pred.val.quad, y.quad)) +
  geom_point(color = "darkblue") + 
  geom_smooth(method = "lm", color = "blue", size = 1) + 
  ggtitle("Quadratic Model: Predicted vs Actual") + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  xlab("Predicted Values") + 
  ylab("Actual y.quad")

# Visualization for predicted vs actual values for cubic model
ggplot(dat, aes(pred.val.cub, y.cubic)) +
  geom_point(color = "darkblue") + 
  geom_smooth(method = "lm", color = "blue", size = 1) + 
  ggtitle("Cubic Model: Predicted vs Actual") + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  xlab("Predicted Values") + 
  ylab("Actual y.cubic")

#===========================================
# End of Analysis                        #
#===========================================
# This repository serves as a template for performing polynomial regression analysis.
# Future analysis may consider implementing validation techniques or exploring additional non-linear relationships.
