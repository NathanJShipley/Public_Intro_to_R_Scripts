#==============================================================================
#                              Analysis Repository:                           #
#                                     T-Test                                  #      
#==============================================================================

#===========================================
# Load Required Libraries                  #
#===========================================
# psych: for descriptive statistics
# tidyverse: for data manipulation (dplyr) and visualization (ggplot2)
# ggpubr: for additional ggplot functions
# car: for Levene's test (homogeneity of variance)
# lsr: for Cohen's d (effect size)
# pwr: for power analysis
# ggsignif: for adding significance annotations to plots
# scales: for formatting salary amounts in plots
# janitor: for cleaner tables

library(psych)
library(tidyverse)
library(ggpubr)
library(car)
library(lsr)
library(pwr)
library(ggsignif)
library(scales)
library(janitor)

#===========================================
# Load and Inspect Data                    #
#===========================================
# Load simulated or real data (replace with actual data path if needed)
dat <- read.csv("data/Sim_Pay_Data.csv", header = TRUE)

# Inspect missing data (if applicable)
# NOTE: Missing data in this dataset should be addressed, if needed.

#===========================================
# Initial Overview                         #
#===========================================
# A t-test is used to compare differences in a dependent variable (DV) between two groups.
# It provides similar information to a simple linear regression when comparing two categories.

#===========================================
# Descriptive Statistics                   #
#===========================================
# Frequency table of gender
tabyl(dat, GENDER)

# Summary statistics of the dependent variable (PAY_NUM)
psych::describe(dat$PAY_NUM)

# Visualizing the distribution of PAY_NUM
hist(dat$PAY_NUM)               # Histogram
ggdensity(dat$PAY_NUM)          # Density plot
boxplot(dat$PAY_NUM)            # Boxplot
ggqqplot(dat$PAY_NUM)           # Q-Q plot for normality check

#===========================================
# Group-wise Descriptives                  #
#===========================================
# Summary statistics of PAY_NUM by GENDER
psych::describeBy(PAY_NUM ~ GENDER, data = dat)

# Boxplot by GENDER
boxplot(PAY_NUM ~ GENDER, data = dat)

# Enhanced boxplot with ggplot2
ggplot(dat, aes(x = GENDER, y = PAY_NUM, color = GENDER)) + 
  geom_boxplot(notch = TRUE) + 
  theme_classic()

# Correlation matrix and scatter plot for selected variables
pairs.panels(select(dat, 3, 8))

#===========================================
# T-Test Analysis                          #
#===========================================
# Performing a t-test to compare PAY_NUM by GENDER
t.test(dat$PAY_NUM ~ dat$GENDER)

# Cohen's D for effect size
cohensD(PAY_NUM ~ GENDER, data = dat)

# Power analysis (post-hoc) for two groups
pwr.t2n.test(n1 = 9092, n2 = 10910, d = .307, sig.level = .001)
# Note: Power analyses should ideally be performed before data collection.
# Adjust the numbers (n1, n2, d) based on your specific sample size and effect size.

#===========================================
# Assumption Checks                        #
#===========================================
# 1. Normality
ggdensity(dat$PAY_NUM)  # Check overall distribution for normality

# Density plot by GENDER
dat %>% 
  tidyr::drop_na(GENDER) %>%
  ggdensity(x = "PAY_NUM", color = "GENDER")

# 2. Outliers and Influence
boxplot(PAY_NUM ~ GENDER, data = dat)  # Check for outliers
ggqqplot(subset(dat, !is.na(GENDER)), x = "PAY_NUM", facet.by = "GENDER")  # Q-Q plot by GENDER

# 3. Homogeneity of Variance (Levene's Test)
leveneTest(PAY_NUM ~ GENDER, data = dat)

# If Levene's Test is significant (p < .05), variances are not equal.
# Perform t-test without assuming equal variances (this is the default in R).
t.test(dat$PAY_NUM ~ dat$GENDER, var.equal = FALSE)

#===========================================
# Visualization of Results                 #
#===========================================
# Set default theme for plots
theme_set(theme_bw())

# Boxplot with significance annotations
dat %>% 
  tidyr::drop_na(GENDER) %>%
  ggplot(aes(x = GENDER, y = PAY_NUM, color = GENDER)) + 
  geom_boxplot() +
  geom_signif(comparisons = list(c("Female", "Male")), map_signif_level = TRUE) + 
  ggtitle("Difference in Salary between Male and Female") +
  theme(plot.title = element_text(hjust = 0.5)) + 
  xlab("Gender") + 
  ylab("Salary") + 
  scale_y_continuous(labels = dollar)

#===========================================
# Final Note: Linear Regression             #
#===========================================
# A t-test is based on Ordinary Least Squares (OLS) regression.
# The same comparison between two groups (GENDER) can be conducted using linear regression.
# Linear regression offers additional flexibility for including more predictors and handling different assumptions.
