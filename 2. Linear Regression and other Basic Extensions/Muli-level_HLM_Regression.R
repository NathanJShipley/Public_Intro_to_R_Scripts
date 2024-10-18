#==============================================================================
#                              Analysis Repository:                           #
#                               MLM                                            #      
#==============================================================================

#===========================================
# Load Libraries                           #
#===========================================

# Libraries used for data analysis and visualization
library(psych)        # Provides descriptive statistics functions
library(tidyverse)    # A powerful set of packages for data manipulation and visualization (dplyr, ggplot2, etc.)
library(multilevel)   # Tools for multilevel models, including hierarchical modeling
library(corrplot)     # Provides visually appealing correlation matrix plots
library(ggpubr)       # Adds publication-ready plots in ggplot2
library(lme4)         # Used for building multilevel models
library(jtools)       # Simplifies the output of regression models with user-friendly summaries
library(ggstance)     # Extends ggplot2 by adding horizontal variants of common geoms

# Note: Other libraries commented out can be used for further analysis, diagnostics, and cross-validation:
# - olsrr: Functions for OLS regression diagnostics and visualization
# - car: Tools for regression models (e.g., Durbin-Watson test)
# - MASS: Provides additional statistical functions (e.g., Shapiro-Wilk test for normality)
# - lmtest: Tools for heteroscedasticity (e.g., Breusch-Pagan test)
# - pwr: Power analysis tools
# - Metrics: RMSE and MAE calculation
# - caret: Cross-validation and model training package
# - mvnormtest: Tests for multivariate normality

#===========================================
# Simulate Data or Load Data               #
#===========================================

set.seed(101)

# Load a sample dataset
data("klein2000")     # Load dataset used for multilevel analysis from multilevel package
dat <- klein2000      # Assign to a variable
dat$GRPID <- as.factor(dat$GRPID) # Convert group ID to factor

# Alternatively, load custom datasets if needed:
# dat <- read.csv("data.csv", header=T)
# dat <- readxl::read_xlsx("fake.data.xlsx")

#===========================================
# Overview                                 #
#===========================================

# Multilevel modeling (MLM) or hierarchical linear modeling (HLM) is used when analyzing data where
# the relationships vary across different groups or levels.
# 
# Key Concepts:
# - Fixed effects: Effects that do not vary across groups.
# - Random effects: Effects that vary across groups.
# 
# This script covers basic linear regression and extends to multilevel regression. 
# Multilevel regression can model differences between groups by allowing intercepts and slopes to vary.

#===========================================
# Assess Descriptives                      #
#===========================================

# Get summary statistics (mean, SD, median, skewness, kurtosis, etc.)
psych::describe(dat)

# Visualize the distribution of key variables (e.g., Job Satisfaction - JOBSAT)
hist(dat$JOBSAT)                   # Histogram
ggdensity(dat$JOBSAT)              # Density plot
boxplot(dat$JOBSAT)                # Boxplot
ggqqplot(dat$JOBSAT)               # Q-Q plot to assess normality

# Explore relationships between variables
pairs.panels(dat, scale=TRUE)      # Scatterplot matrix with correlations
corrplot(cor(dat, use="complete.obs"), order="hclust", tl.col='black', tl.cex=.75)  # Correlation plot

# Scatter plots to visualize relationships between predictors and the dependent variable (JOBSAT)
ggplot(dat, aes(PAY, JOBSAT)) +
  geom_point() + 
  geom_smooth(method="lm", fullrange=T)

# Faceting by group to see how relationships vary across groups (e.g., GRPID)
ggplot(dat, aes(PAY, JOBSAT)) +
  geom_point() + 
  geom_smooth(method="lm", fullrange=T) +
  facet_wrap(~GRPID)

# Repeat for other predictors:
ggplot(dat, aes(POSAFF, JOBSAT)) + geom_point() + geom_smooth(method="lm", fullrange=T) + facet_wrap(~GRPID)
ggplot(dat, aes(WLOAD, JOBSAT)) + geom_point() + geom_smooth(method="lm", fullrange=T) + facet_wrap(~GRPID)
ggplot(dat, aes(NEGLEAD, JOBSAT)) + geom_point() + geom_smooth(method="lm", fullrange=T) + facet_wrap(~GRPID)

# Overlay groups in a single plot for NEGLEAD
ggplot(dat, aes(x=NEGLEAD, y=JOBSAT, color=GRPID)) +
  geom_point(size=0.5) + 
  geom_smooth(method="lm", se=F, aes(group=GRPID)) +
  theme_classic() + 
  theme(legend.position="none")

#===========================================
# Build Linear Model & Get Output          #
#===========================================

# Fit a standard linear regression model predicting job satisfaction (JOBSAT)
# Using pay (PAY), positive affect (POSAFF), workload (WLOAD), and negative leadership (NEGLEAD)
Reg.fit <- lm(JOBSAT ~ PAY + POSAFF + WLOAD + NEGLEAD, data=dat)

# Display detailed output including partial correlations and Variance Inflation Factors (VIFs)
jtools::summ(Reg.fit, part.corr=TRUE, vifs=TRUE, digits=3)

#===========================================
# Build Multi-level Model & Get Output     #
#===========================================

# With multilevel modeling, you can model both random intercept and random slope models:
# - Random intercept: Relationship between predictors and outcome (e.g., PAY and JOBSAT) is the same,
#                     but predicted values may vary across groups.
# - Random slope: Relationship between predictors and outcome varies across groups.

#====================== =
# # Null Model        # #
#====================== =

# First, fit a null model with random intercepts only
Random.Null.Fit <- lmer(JOBSAT ~ 1 + (1 | GRPID), data=dat)

# Summarize the null model
jtools::summ(Random.Null.Fit, digits=3)
summary(Random.Null.Fit)

# Calculate the Intraclass Correlation Coefficient (ICC), which indicates the proportion of variance 
# between groups (Level 2 variance) vs. within groups (Level 1 variance)
icc_value <- 0.6835/(0.6835 + 5.4741)
# ICC interpretation: About 11% of the variation in job satisfaction is at the group level

#====================== =
# # Random Intercepts # #
#====================== =

# Fit a model with random intercepts, allowing variation in job satisfaction across groups
Random.Intercepts.Fit <- lmer(JOBSAT ~ PAY + POSAFF + WLOAD + NEGLEAD + (1 | GRPID), data=dat)

# Summarize the model with fixed effects and random intercepts
jtools::summ(Random.Intercepts.Fit, digits=3)

# Compare model fit between the null and random intercept model
anova(Random.Null.Fit, Random.Intercepts.Fit)

#====================== =
# # Random Slopes     # #
#====================== =

# Fit a model with random slopes, allowing the relationship between NEGLEAD and JOBSAT to vary across groups
Random.Slopes.Fit <- lmer(JOBSAT ~ PAY + POSAFF + WLOAD + NEGLEAD + (NEGLEAD | GRPID), data=dat)

# Summarize the random slope model
jtools::summ(Random.Slopes.Fit, digits=3)

# Compare the random intercept model with the random slope model
anova(Random.Intercepts.Fit, Random.Slopes.Fit)

#====================== =
# # Mean Centering    # #
#====================== =

# Mean centering predictors is a crucial step in multilevel modeling. Centering can be done in two ways:
# - Grand Mean Centering: Centers variables relative to the overall mean across all groups.
# - Group Mean Centering: Centers variables relative to the mean within each group.
# 
# Grand mean centering controls for variance at Level 1 prior to Level 2 analysis, while group mean 
# centering separates within-group and between-group variation in regression slopes.