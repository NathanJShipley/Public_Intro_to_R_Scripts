#==============================================================================
#                              Analysis Repository:                           #
#                                     T-Test                                  #      
#==============================================================================

#===========================================
# Load Libraries                           #
#==========================================-

library(psych) #has nice describe function for descriptive info
library(tidyverse) #has dplyr and ggplot2
library(ggpubr) #has some more GG plots
library(car) #Levene Test
library(lsr) #Cohen's D function
library(pwr) #power analysis
library(ggsignif) #add Sig to ggplots
library(scales) #used in ggplot to show salary amounts
library(janitor) #Tabyl function


#===========================================
# Simulate Data OR LOAD DATA               #
#==========================================-

dat <- read.csv("data/Sim_Pay_Data.csv", header=T)
#dat <- readxl::read_xlsx("fake.data.xlsx")

# Note that this data has missing data in many places which may need to be examined


#===========================================
# Overview                                 #
#==========================================-

# Note a t.test is used to compare differences in a dependent variable between two groups
# The stats are similar to linear regression


#===========================================
# Assess Basic Descriptives                #
#==========================================-

tabyl(dat,GENDER)
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

psych::describeBy(PAY_NUM ~ GENDER, data = dat)
# Means, sd, etc by IV

boxplot(PAY_NUM ~ GENDER, data = dat)

ggplot(dat, aes(x=GENDER, y=PAY_NUM, color = GENDER)) + 
  geom_boxplot(notch=TRUE) + 
  theme_classic()
# Another nicer boxplot

pairs.panels(select(dat, 3,8)) 
# Correlation and scatter plot


#===========================================
# Build Model & Get Output                 #
#==========================================-

# Run test
t.test(dat$PAY_NUM ~ dat$GENDER)
# Output shows differences and statistical info

# can pull additional information using some other functions
cohensD(PAY_NUM ~ GENDER, data = dat)
# Measure of effect size

# Can post-hoc power
pwr.t2n.test(n1 = 9092, n2= 10910, d = .307, sig.level = .001)
# but note that power analyses should normally be done prior to data collection
# NOTE will have to manually adjust the numbers above


#===========================================
# Check Assumptions & Adjust as needed     #
#==========================================-

# Check for normal distribution
ggdensity(dat$PAY_NUM)
# Want data to be normally distributed 

dat %>% 
  tidyr::drop_na(GENDER) %>%
  ggdensity(x = "PAY_NUM", color = "GENDER")
# Look at density broken down by cateogry 

# Check for outliers or other overly influential cases
boxplot(PAY_NUM ~ GENDER, data = dat)
ggqqplot(subset(dat, !is.na(GENDER)), x = "PAY_NUM", facet.by = "GENDER") #

# Assumption of Homogeneity of Variance
leveneTest(PAY_NUM ~ GENDER, data = dat)
# If sig, then variances are NOT assumed to be equal, need adjustments 

t.test(dat$PAY_NUM ~ dat$GENDER, var.equal=F)
# NOTE that in R, the default is to assume variances are not equal
# So in practice, not adjustments to the model may be needed


#===========================================
# Plotting                                 #
#==========================================-

theme_set(
  theme_bw()
)

dat %>% 
  tidyr::drop_na(GENDER) %>%
  ggplot(aes(x=GENDER, y=PAY_NUM, color=GENDER)) + 
  geom_boxplot() +
  geom_signif(comparisons = list(c("Female", "Male")),  map_signif_level=TRUE) + 
  ggtitle("Difference in Salary between Male and Female") + theme(plot.title = element_text(hjust = 0.5)) + 
  xlab("Gender") + 
  ylab("Salary") + 
  scale_y_continuous(labels = dollar)


#===========================================
# Final Note: Linear Regression            #
#==========================================-

# It should be noted that t.test is based on OLS
# Given this simple principle, the same calculations can be done using regression
# All models and tools used in linear regression can be done with the data analyzed here
