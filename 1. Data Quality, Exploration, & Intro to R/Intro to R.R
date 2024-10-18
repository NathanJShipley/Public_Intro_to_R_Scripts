#==============================================================================
#                              Analysis Repository:                           #
#                                  Intro to R                                 #      
#==============================================================================

# This script assumes you have no prior knowledge of R.
# Note that anything starting with a # (octothorpe, pound sign) is a comment, 
# and R won't run what follows. 
# To run a line of code, press "Ctrl" and "Enter" simultaneously. 
# You can also highlight multiple lines and press "Ctrl" + "Enter" to run them all.

# R relies on libraries that provide the algorithms for various functions. 
# You must specify which libraries to use with the "library" function. 
# If you don't have a library installed, use "install.packages('example_library_name')" to install it. 
# You only need to install a library once, but you have to load it each time you run your script.

# Load necessary libraries
library(psych)       # For descriptive statistics and other useful functions
library(tidyverse)   # Contains dplyr for data manipulation and ggplot2 for plotting

# First, we need to read in our data. 
# The function "read.csv" opens CSV files.
# It's good practice to use "<-" to assign the result of a function to an object.
# For keyboard shortcuts, you can create "<-" by pressing "Alt" + "-" (minus key) together.
# Below, we're loading the pay data CSV and assigning it to the object name "pay.dat".
pay.dat <- read.csv("data/Sim_Pay_Data.csv", header=TRUE)  # Adjust the filename as necessary

# You can view the object by running the line of code below.
pay.dat

# The output might be overwhelming, so you can use the "View" function to see the table of data in a spreadsheet format.
# Note that R is generally case-sensitive; I suggest being consistent with case sensitivity in all code to avoid issues.
View(pay.dat)

# We will focus on analyzing employee pay data. 
# This dataset includes various demographic information along with their regular pay.
# First, we will gather some basic descriptive statistics.

#===========================================
# Example of running descriptive stats     #
#==========================================

# The 'psych' package has a convenient function called "describe".
# We will use the 'dplyr' package to select the relevant columns for our analysis.
# I will focus on the 'REGULAR_PAY' column for this example.
# The code below will provide descriptive information for the 'REGULAR_PAY' variable.

describe(pay.dat$PAY_NUM)  # Descriptive stats for regular pay

# You can also retrieve descriptive statistics for multiple variables.
# Let's get a summary of pay by gender using the 'dplyr' package.

# First, summarizing the mean regular pay by gender.
pay_summary <- pay.dat %>%
  group_by(GENDER) %>%
  summarize(Mean_Pay = mean(PAY_NUM, na.rm = TRUE),  # Calculating mean pay while ignoring NAs
            Count = n(),                               # Counting the number of employees in each gender group
            .groups = "drop")                         # Dropping the group structure for further analysis

print(pay_summary)  # Displaying the summary statistics

# To visualize the results, we can create a bar plot of average pay by gender.
# This helps to illustrate potential pay disparities.

ggplot(pay_summary, aes(x=GENDER, y=Mean_Pay, fill=GENDER)) +
  geom_bar(stat="identity", position=position_dodge()) +
  labs(title="Average Regular Pay by Gender", x="Gender", y="Average Regular Pay") +
  theme_minimal()

# Finally, let's explore the correlation between regular pay and log pay.
# The pairs.panels function creates a visual representation showing correlations and distributions.
pairs.panels(pay.dat[, c("PAY_NUM", "LOG_PAY")], scale=TRUE)  # Scatter plots and correlation matrix for pay variables

# This code creates:
# 1.) A histogram and distribution of each variable on the diagonal.
# 2.) The correlation coefficients between variables above the diagonal.
# 3.) Scatter plots of the relationships between variables below the diagonal.

