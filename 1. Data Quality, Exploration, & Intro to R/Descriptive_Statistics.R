#==============================================================================
#                              Analysis Repository:                           #
#                             Descriptive Statistics                          #      
#==============================================================================

#===========================================
# Load Libraries                           #
#==========================================-
library(psych) # For Descriptive and other useful functions 
library(tidyverse) #has dplyr and ggplot2
library(ggpubr) #some more visualizations 
library(janitor) #Tabyl function
library(finalfit) # Missing Functions
library(corrplot) #nice cor plot

#===========================================
# Simulate Data OR LOAD DATA               #
#==========================================-

data("iris")
dat <- iris
# Load built in dataset

dat.pay <- read.csv("data/Sim_Pay_Data.csv", header=T)
#dat <- readxl::read_xlsx("fake.data.xlsx")


#===========================================
# Some brief data manipulation             #
#==========================================-

dat$Sepal.size <- ifelse(dat$Sepal.Length < median(dat$Sepal.Length),
                   "small", "big")

dat$Sepal.wide <- ifelse(dat$Sepal.Width < median(dat$Sepal.Width),
                         "small", "big")
#===========================================
# Assess Missing Data                      #
#==========================================-

# Heat Map of Missing values in Dataset
missing_plot(dat.pay)
missing_plot(dat.pay, dependent = "RACE")
missing_plot(dat.pay, dependent = "PAY_NUM")
# Can examine if things differ based on other factors
missing_plot(dat.pay, dependent = "PAY_NUM", explanatory = "RACE")
missing_plot(dat.pay, dependent = "RACE", explanatory = "GENDER")

# A matrix of missing data patterns
# 1 indicates observed, 0 is missing
# Rows and columns sorted in increasing amount of missing information
missing_pattern(dat.pay)
# Numbers on right indicate # of observations in each pattern
# Numbers on bottom indicate # of data missing per pattern
missing_pattern(dat.pay,dependent = "PAY_NUM", explanatory = c("RACE","GENDER"))
# Can select a few specific variables 

missing_compare(dat.pay, dependent = "PAY_NUM", explanatory = c("RACE","GENDER"))
# Can compare patterns of missing data in a specific variable based on explanatory variables 


#===========================================
# Check Structure of Data                  #
#==========================================-

head(dat)
# can see column headers

View(dat)
# can see data directly

dat[1,]
# Data for first row

dat[,1]
# Data for first column


#===========================================
# Univariate Descriptive Data              #
#==========================================-

summary(dat)
# gives min, mean, max, quartiles for each variable 

psych::describe(dat)
psych::describe(dat$Sepal.Length)
# Both of these provide additional stats such as mean, SD, median, skewness, and kurtosis

tabyl(dat,Species)
# A basic frequency table

dat %>%
  tabyl(Species) %>%
  adorn_pct_formatting(digits = 0, affix_sign = T)


#===========================================
# Univariate Data Visualizations           #
#==========================================-

hist(dat$Sepal.Length)
# Histogram 

ggdensity(dat$Sepal.Length)
# Density plot, similar to histogram

boxplot(dat$Sepal.Length)
# Box plot

ggqqplot(dat$Sepal.Length)
# QQ plot for univariate 

ggplot(dat,aes(y=Sepal.Length)) + 
  geom_boxplot(notch=T)
# Boxplot of quantiles and mean

ggplot(dat,aes(Sepal.Length)) + 
  geom_histogram(bins = 10)
# Distribution histogram 

ggplot(dat, aes(x=Species)) + 
  geom_bar()
# Counts of categorical 


#===========================================
# Multivariate Descriptive Data            #
#==========================================-

psych::describeBy(Sepal.Length ~ Species, data = dat)
# Get mean, SD, etc for variable by category 

tabyl(dat,Species,Sepal.size)
# Frequency for multiple categorical & continuous 


#===========================================
# Multivariate Data Visualizations         #
#==========================================-

pairs.panels(dat, scale=TRUE)
# Show correlations and scatter plots for all data

pairs.panels(select(dat, 1:5)) 
# Selected first 5 variables, removed scale

# Another visualization
corrplot(cor(dplyr::select(dat,1:4), use="complete.obs"), order = "hclust", tl.col='black', tl.cex=.75) 

ggplot(dat,aes(Petal.Length,Petal.Width)) + 
  geom_point(color="darkblue") + 
  geom_smooth(color="blue",size=1.5, method='lm') + 
  ggtitle("Relationship between Petal Length and Width") + theme(plot.title = element_text(hjust = 0.5))

ggplot(dat, aes(x=Species, y=Sepal.Length, color = Species)) + 
  geom_boxplot(notch=TRUE) + 
  theme_classic()

ggplot(dat, aes(x=Sepal.Length, color = Species)) + 
  geom_histogram(aes(y = ..density..),
                 colour = 1, fill = "white",
                 bins = 10) +
  geom_density(size = 1) + 
  facet_wrap(~ Species) + 
  theme_classic()


ggplot(dat, aes(x=Sepal.size, y = Sepal.Length, fill = Sepal.wide)) + 
  geom_violin(trim = FALSE) + 
  theme_classic()
  
ggplot(dat, aes(x=Sepal.size, y = Petal.Length, fill = Species)) + 
  geom_violin(trim = FALSE) +  
  theme_classic()

ggscatterhist(iris, x = "Sepal.Length", y = "Sepal.Width",
              margin.params = list(fill = "lightgray"))

ggscatterhist(iris, x = "Sepal.Length", y = "Sepal.Width",
              color = "Species", size = 3, alpha = 0.6,
              palette = c("#00AFBB", "#E7B800", "#FC4E07"),
              margin.params = list(fill = "Species", color = "black", size = 0.2))


# Assess Multivariate Outliers
psych::outlier(dplyr::select(dat,1:4))
