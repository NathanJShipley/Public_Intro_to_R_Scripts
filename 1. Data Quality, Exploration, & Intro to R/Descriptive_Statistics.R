#==============================================================================
#                              Analysis Repository:                           #
#                             Descriptive Statistics                          #      
#==============================================================================

#===========================================
# Load Libraries                           #
#==========================================
# Import necessary libraries for data manipulation, visualization, and analysis.
library(psych)        # For descriptive statistics and useful functions
library(tidyverse)    # A collection of packages including dplyr and ggplot2 for data manipulation and visualization
library(ggpubr)       # Provides additional ggplot2 functionalities for visualizations
library(janitor)      # Contains functions for cleaning and inspecting data, including the 'tabyl' function
library(finalfit)     # Functions for dealing with missing data
library(corrplot)     # Functions for creating correlation plots


#===========================================
# Simulate Data OR LOAD DATA               #
#==========================================
# Load the iris dataset, a well-known dataset for flower species classification
data("iris")
dat <- iris # Assign the iris dataset to the variable 'dat'

# Load a simulated pay dataset from a CSV file
dat.pay <- read.csv("data/Sim_Pay_Data.csv", header=T)
# Uncomment the following line to load data from an Excel file
# dat <- readxl::read_xlsx("fake.data.xlsx")


#===========================================
# Some brief data manipulation             #
#==========================================
# Create a new categorical variable 'Sepal.size' based on the median of Sepal.Length
dat$Sepal.size <- ifelse(dat$Sepal.Length < median(dat$Sepal.Length), "small", "big")

# Create another categorical variable 'Sepal.wide' based on the median of Sepal.Width
dat$Sepal.wide <- ifelse(dat$Sepal.Width < median(dat$Sepal.Width), "small", "big")


#===========================================
# Assess Missing Data                      #
#==========================================
# Visualize missing data in the 'dat.pay' dataset using heat maps
missing_plot(dat.pay)                              # General heatmap of missing values
missing_plot(dat.pay, dependent = "RACE")         # Heatmap of missing values dependent on 'RACE'
missing_plot(dat.pay, dependent = "PAY_NUM")      # Heatmap of missing values dependent on 'PAY_NUM'
missing_plot(dat.pay, dependent = "PAY_NUM", explanatory = "RACE") # Explore missing patterns by 'RACE'
missing_plot(dat.pay, dependent = "RACE", explanatory = "GENDER")   # Explore missing patterns by 'GENDER'

# Generate a matrix showing missing data patterns
# 1 indicates observed values, 0 indicates missing values
# This allows assessment of the structure of missing data
missing_pattern(dat.pay)

# Display counts of observations per missing pattern
missing_pattern(dat.pay, dependent = "PAY_NUM", explanatory = c("RACE", "GENDER"))

# Compare missing data patterns based on explanatory variables
missing_compare(dat.pay, dependent = "PAY_NUM", explanatory = c("RACE", "GENDER"))


#===========================================
# Check Structure of Data                  #
#==========================================
# View the first few rows of the dataset to understand its structure
head(dat)                # Displays the first six rows
View(dat)               # Opens the dataset in a viewer window
dat[1,]                 # Access the first row of the dataset
dat[,1]                 # Access the first column of the dataset


#===========================================
# Univariate Descriptive Data              #
#==========================================
# Generate summary statistics for the dataset
summary(dat)            # Provides min, mean, max, and quartiles for each variable

# Use psych package to provide a detailed description of the dataset
psych::describe(dat)                     # Provides additional stats like mean, SD, skewness, kurtosis
psych::describe(dat$Sepal.Length)       # Summary statistics specifically for 'Sepal.Length'

# Create a basic frequency table for the 'Species' variable
tabyl(dat, Species)

# Format the frequency table to include percentage formatting
dat %>%
  tabyl(Species) %>%
  adorn_pct_formatting(digits = 0, affix_sign = T)


#===========================================
# Univariate Data Visualizations           #
#==========================================
# Generate various plots to visualize the distribution of 'Sepal.Length'
hist(dat$Sepal.Length)                         # Histogram
ggdensity(dat$Sepal.Length)                    # Density plot
boxplot(dat$Sepal.Length)                      # Box plot
ggqqplot(dat$Sepal.Length)                     # QQ plot for checking normality

# Create a box plot with notches to indicate confidence intervals around the median
ggplot(dat, aes(y = Sepal.Length)) + 
  geom_boxplot(notch = TRUE)

# Histogram of 'Sepal.Length' with specified number of bins
ggplot(dat, aes(Sepal.Length)) + 
  geom_histogram(bins = 10)

# Bar plot showing counts of each species
ggplot(dat, aes(x = Species)) + 
  geom_bar()


#===========================================
# Multivariate Descriptive Data            #
#==========================================
# Get descriptive statistics for 'Sepal.Length' by species category
psych::describeBy(Sepal.Length ~ Species, data = dat)

# Create a frequency table for 'Species' and the new 'Sepal.size' variable
tabyl(dat, Species, Sepal.size)


#===========================================
# Multivariate Data Visualizations         #
#==========================================
# Create pairwise scatter plots and correlation coefficients
pairs.panels(dat, scale = TRUE)  # All variables with scaling
pairs.panels(select(dat, 1:5))   # First five variables without scaling

# Create a correlation plot for the first four numerical variables
corrplot(cor(dplyr::select(dat, 1:4), use = "complete.obs"), order = "hclust", tl.col = 'black', tl.cex = .75) 

# Scatter plot with a linear regression line for Petal.Length and Petal.Width
ggplot(dat, aes(Petal.Length, Petal.Width)) + 
  geom_point(color = "darkblue") + 
  geom_smooth(color = "blue", size = 1.5, method = 'lm') + 
  ggtitle("Relationship between Petal Length and Width") + 
  theme(plot.title = element_text(hjust = 0.5))

# Box plot of Sepal.Length by Species with notches
ggplot(dat, aes(x = Species, y = Sepal.Length, color = Species)) + 
  geom_boxplot(notch = TRUE) + 
  theme_classic()

# Histogram of Sepal.Length with density overlay, faceted by Species
ggplot(dat, aes(x = Sepal.Length, color = Species)) + 
  geom_histogram(aes(y = ..density..), colour = 1, fill = "white", bins = 10) +
  geom_density(size = 1) + 
  facet_wrap(~ Species) + 
  theme_classic()

# Violin plot showing the distribution of Sepal.Length by Sepal.size and colored by Sepal.wide
ggplot(dat, aes(x = Sepal.size, y = Sepal.Length, fill = Sepal.wide)) + 
  geom_violin(trim = FALSE) + 
  theme_classic()

# Violin plot of Petal.Length by Sepal.size and colored by Species
ggplot(dat, aes(x = Sepal.size, y = Petal.Length, fill = Species)) + 
  geom_violin(trim = FALSE) +  
  theme_classic()

# Scatter plot with marginal histograms for Sepal.Length and Sepal.Width
ggscatterhist(iris, x = "Sepal.Length", y = "Sepal.Width",
              margin.params = list(fill = "lightgray"))

# Scatter plot with marginal histograms for Sepal.Length and Sepal.Width, colored by Species
ggscatterhist(iris, x = "Sepal.Length", y = "Sepal.Width",
              color = "Species", size = 3, alpha = 0.6,
              palette = c("#00AFBB", "#E7B800", "#FC4E07"),
              margin.params = list(fill = "Species", color = "black", size = 0.2))

# Assess Multivariate Outliers
psych::outlier(dplyr::select(dat, 1:4))  # Check for outliers in the first four variables



