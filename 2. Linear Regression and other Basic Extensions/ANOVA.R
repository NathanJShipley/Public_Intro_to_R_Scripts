#==============================================================================
#                              Analysis Repository:                           #
#                                     ANOVA                                   #      
#==============================================================================


#===========================================
# Load Libraries                           #
#===========================================

# Load necessary libraries for data analysis and visualization
library(psych)       # For descriptive statistics and data summaries
library(tidyverse)   # A collection of packages including dplyr for data manipulation and ggplot2 for visualization
library(ggpubr)      # Additional ggplot2 functions for enhanced plotting capabilities
library(car)         # Functions for ANOVA analysis, including the Anova() function
library(lsr)         # For calculating effect sizes, specifically eta-squared
library(multcomp)    # For performing multiple comparisons in ANOVA
library(pwr)         # For conducting power analysis
library(ggsignif)    # To add significance levels to ggplots
library(scales)      # For formatting scales in ggplots, especially for currency
library(janitor)     # Provides functions for cleaning data, such as tabyl for frequency tables
library(mvnormtest)  # For testing multivariate normality of the data


#===========================================
# Simulate Data OR LOAD DATA               #
#===========================================

# Load your data from a CSV file; adjust the file path as necessary
dat <- read.csv("data/Sim_Pay_Data.csv", header = TRUE)
# Alternatively, load data from an Excel file (commented out for now)
# dat <- readxl::read_xlsx("fake.data.xlsx")

# Note: Inspect the data for missing values or unusual entries, which may affect analysis

# Ensure the independent variable is treated as a factor for ANOVA
dat$RACE <- as.factor(dat$RACE)


#===========================================
# Overview                                 #
#===========================================

# Analysis of Variance (ANOVA) is a statistical method used to compare means across different groups
# The dependent variable (DV) should be continuous, and the independent variable (IV) should be categorical.
# Note: The mathematical foundation of ANOVA is similar to that of OLS linear regression.


#===========================================
# Assess Basic Descriptives                #
#===========================================

# Create a basic frequency table for the categorical variable RACE
tabyl(dat, RACE)


#====================== =
# Assessing the Dependent Variable (DV) #
#====================== =
# Descriptive statistics for the numerical variable PAY_NUM
psych::describe(dat$PAY_NUM)


# Visualizing the distribution of PAY_NUM
hist(dat$PAY_NUM)                # Histogram of PAY_NUM
ggdensity(dat$PAY_NUM)           # Density plot of PAY_NUM
boxplot(dat$PAY_NUM)             # Boxplot to visualize the spread and potential outliers
ggqqplot(dat$PAY_NUM)            # QQ plot to assess normality of PAY_NUM


#====================== =
# Assessing DV with IV  #
#====================== =
# Descriptive statistics for PAY_NUM, grouped by RACE
psych::describeBy(PAY_NUM ~ RACE, data = dat)


# Boxplot of PAY_NUM by RACE
boxplot(PAY_NUM ~ RACE, data = dat)


# Boxplot with ggplot2, adding color by GENDER
dat %>% 
  drop_na(RACE) %>%
  ggplot(aes(x = RACE, y = PAY_NUM, color = GENDER)) + 
  geom_boxplot(notch = TRUE) + 
  theme_classic()


# Correlation and scatter plot for selected variables
pairs.panels(dplyr::select(dat, 2, 8)) 


#===========================================
# Build Model & Get Output                 #
#===========================================

# Create ANOVA model
AOV.fit <- aov(PAY_NUM ~ RACE, data = dat)


# Evaluate fit of the model using Type III sums of squares
Anova(AOV.fit, type = "III")

# Summary of the linear model
summary.lm(AOV.fit)

# Obtain estimated means for each group
print(model.tables(AOV.fit, "means"), digits = 3)

# Calculate effect size (eta-squared) for the ANOVA model
etaSquared(AOV.fit, type = 3, anova = FALSE)

# Conduct pairwise comparisons using Tukey's method
summary(glht(AOV.fit, linfct = mcp(RACE = "Tukey")))

# Post-hoc power analysis to assess the power of the ANOVA
# Note: Ideally, power analyses should be conducted prior to data collection
pwr.anova.test(k = 5, n = 876, f = .1, sig.level = .05)


#===========================================
# Check Assumptions & Adjust as needed     #
#===========================================

# 1.) Normality of the DV
# Assessing normal distribution visually
ggdensity(dat$PAY_NUM)

# Density plot of PAY_NUM by RACE
dat %>% 
  tidyr::drop_na(RACE) %>%
  ggdensity(x = "PAY_NUM", color = "RACE")

# Assess multivariate normality using Shapiro-Wilk test
mshapiro.test(t(dat))

# 2.) Linearity
# Plot residuals vs fitted values to check for linearity
plot(AOV.fit, 1)

# 3.) Normality of Residuals
# QQ plot to evaluate the normality of residuals
plot(AOV.fit, 2)

# QQ plots for each category of RACE
ggqqplot(subset(dat, !is.na(RACE)), x = "PAY_NUM", facet.by = "RACE")

# Conduct a Shapiro-Wilk test on residuals to formally assess normality
AOV.resid <- studres(AOV.fit)
shapiro.test(AOV.resid)

# Identify potential outliers using Cook's distance and leverage plots
plot(AOV.fit, 4)  # Cook's distances
plot(AOV.fit, 5)  # Leverage plot

# Check for multivariate outliers using psych package
psych::outlier(dat)

# 4.) Homoscedasticity
# Residuals vs fitted values plot to check homoscedasticity
plot(AOV.fit, 3)
plot(resid(AOV.fit))

# Levene's test for homogeneity of variances
leveneTest(PAY_NUM ~ RACE, data = dat)

# 5.) Independence (Multicollinearity)
# Check for autocorrelation using Durbin-Watson test
durbinWatsonTest(AOV.fit)

# Assess Variance Inflation Factor (VIF) for multicollinearity
jtools::summ(AOV.fit, vifs = TRUE, digits = 3)


#===========================================
# Model adjustments if violate assumptions #
#===========================================

# If assumptions are violated, consider these adjustments
oneway.test(PAY_NUM ~ RACE, data = dat) # ANOVA with more relaxed assumptions
kruskal.test(PAY_NUM ~ RACE, data = dat) # Non-parametric test for comparing medians


#===========================================
# Plotting                                 #
#===========================================

# Set ggplot theme
theme_set(theme_bw())

# Boxplot of PAY_NUM by RACE with significance annotations
dat %>% 
  tidyr::drop_na(RACE) %>%
  ggplot(aes(x = RACE, y = PAY_NUM, color = RACE)) + 
  geom_boxplot() +
  geom_signif(comparisons = list(c("White", "Black")), map_signif_level = TRUE) + 
  ggtitle("Difference in Salary by Race") + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  xlab("Race") + 
  ylab("Salary") + 
  scale_y_continuous(labels = dollar)


#===========================================
# Final Note: Linear Regression            #
#===========================================

# It is important to note that ANOVA is fundamentally based on ordinary least squares (OLS).
# Thus, the same analysis can often be conducted using linear regression methods, 
# allowing for similar insights and interpretations.
