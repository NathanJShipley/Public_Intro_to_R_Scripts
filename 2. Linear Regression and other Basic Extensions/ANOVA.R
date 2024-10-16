#==============================================================================
#                              Analysis Repository:                           #
#                                     ANOVA                                   #      
#==============================================================================

#===========================================
# Load Libraries                           #
#==========================================-

library(psych) #has nice describe function for descriptive info
library(tidyverse) #has dplyr and ggplot2
library(ggpubr) #has some more GG plots
library(car) #ANOVSA
library(lsr) #eta-squared
library(multcomp) #needed for multiple comparrisons in ANOVA
library(pwr) #power analysis
library(ggsignif) #add Sig to ggplots
library(scales) #used in ggplot to show salary amounts
library(janitor) #Tabyl function
library(mvnormtest) #normality


#===========================================
# Simulate Data OR LOAD DATA               #
#==========================================-

dat <- read.csv("data/Sim_Pay_Data.csv", header=T)
#dat <- readxl::read_xlsx("fake.data.xlsx")

# Note that this data has missing data in many places which may need to be examined

# ANOVA works with factors
# important to set as factor now
dat$RACE <- as.factor(dat$RACE)


#===========================================
# Overview                                 #
#==========================================-

# Analysis of Variance, used to examine a continous DV across categorical IV
# Note that the math is idential to OLS linear regression, which can also be used

#===========================================
# Assess Basic Descriptives                #
#==========================================-

tabyl(dat, RACE)
# A basic frequency table

#====================== =
# DV # #
#====================== =
psych::describe(dat$PAY_NUM)

hist(dat$PAY_NUM)
ggdensity(dat$PAY_NUM)
boxplot(dat$PAY_NUM)
ggqqplot(dat$PAY_NUM)

#====================== =
# DV with IV # #
#====================== =

psych::describeBy(PAY_NUM ~ RACE, data = dat)
# Means, sd, etc by IV

boxplot(PAY_NUM ~ RACE, data = dat)

dat %>% 
  drop_na(RACE) %>%
  ggplot(aes(x=RACE, y=PAY_NUM, color = GENDER)) + 
  geom_boxplot(notch=TRUE) + 
  theme_classic()

pairs.panels(dplyr::select(dat, 2,8)) 
# Correlation and scatter plot


#===========================================
# Build Model & Get Output                 #
#==========================================-

# Create Model
AOV.fit <- aov(PAY_NUM ~ RACE, data = dat)

# Evaluate fit
Anova(AOV.fit, type = "III")
# This summary shows the omnibus results for the DV
# Type should be set to III
summary.lm(AOV.fit)
# get specific estimates for each class
print(model.tables(AOV.fit, "means"), digits=3)
# get estimated values per group

# Effect size
etaSquared(AOV.fit, type = 3, anova = FALSE) #

# Pairwise comparisons
summary(glht(AOV.fit, linfct = mcp(RACE = "Tukey"))) # 
# a few differences, most notable is between Caucasian and Black
# 4035.6 difference
# Most other non sig, small samples 

# Can do post-hoc power analysis 
pwr.anova.test(k = 5, n = 876, f = .1, sig.level = .05)
# but note that power analyses should normally be done prior to data collection
# NOTE will have to manually adjust the numbers above


#===========================================
# Check Assumptions & Adjust as needed     #
#==========================================-

# 1.) Normality of DV
# Check for normal distribution
ggdensity(dat$PAY_NUM)
# Want data to be normally distributed 

dat %>% 
  tidyr::drop_na(RACE) %>%
  ggdensity(x = "PAY_NUM", color = "RACE")
# Look at density broken down by cateogry 

# Assess multivariate normality 
mshapiro.test(t(dat))

# 2.) Linearity 
plot(AOV.fit,1)
# Fitted line (red), should be roughly zero across
# If there is a pattern, may indicate non-linearity
# If pattern, then may need to run a polynomial regression


# 3.) Normality of Residuals
plot(AOV.fit,2)
# QQ plot, should roughly be a straight (diagonal) line

# QQ plot for each subset
ggqqplot(subset(dat, !is.na(RACE)), x = "PAY_NUM", facet.by = "RACE") #

# can also formally test using a Shapiro-Wilk Normality test
AOV.resid <- studres(AOV.fit)
shapiro.test(AOV.resid)
# If sig, then not normally distributed

# Residuals may not be normal due to OUTLIERS or variables having high leverage 
# Can examine various plots to ID points exerting extreme influence on outcome 
plot(AOV.fit,4)
# Cooks Distances for outliers
plot(AOV.fit,5)
# Assess Leverage, ID those with extreme values
# For all extreme values, could be worth removing and assessing how relationships change
# sensitivity type analysis

# Assess Multivariate Outliers
psych::outlier(dat)

# 4.) Homoscedasticity
plot(AOV.fit,3)
plot(resid(AOV.fit))
# visualize residuals, want straight line, no patterns

# can formally test using Levene test
leveneTest(PAY_NUM ~ RACE, data = dat)
# If sig, then determine not homoscedatic, presence of heteroscedasticity 
# If sig, then variances are NOT assumed to be equal, need adjustments 


# 5.) Independent (Multicollinearity)
durbinWatsonTest(AOV.fit)
# First check autocorrelation using Durbin Watson test
# If sig, then may have to account using a lag variable

# Also check for extreme correlations and VIFs
jtools::summ(AOV.fit, vifs=TRUE, digits=3)
# ideal VIF is less than 4, with anything less than 10 considered acceptable (Hair et al., 2010)
# If high VIF or high correlations, may have to remove factors from the model


#===========================================
# Model adjustments if violate assumptions #
#==========================================-

# If violate above, then need adjustments 
# For ANOVA, R has a few different adjustments, each with more relaxed assumptions

oneway.test(PAY_NUM ~ RACE, data = dat) # more relaxed assumptions
kruskal.test(PAY_NUM ~ RACE, data = dat) # non-parametric


#===========================================
# Plotting                                 #
#==========================================-

theme_set(
  theme_bw()
)

dat %>% 
  tidyr::drop_na(RACE) %>%
  ggplot(aes(x=RACE, y=PAY_NUM, color=RACE)) + 
  geom_boxplot() +
  geom_signif(comparisons = list(c("White", "Black")),  map_signif_level=TRUE) + 
  ggtitle("Difference in Salary between Male and Female") + theme(plot.title = element_text(hjust = 0.5)) + 
  xlab("Gender") + 
  ylab("Salary") + 
  scale_y_continuous(labels = dollar)


#===========================================
# Final Note: Linear Regression            #
#==========================================-

# It should be noted that an ANOVA is based on OLS
# Given this simple principle, generally speaking the same calculations can be done using regression
# Generally all models and tools used in linear regression can be done with the data analyzed via ANOVA
