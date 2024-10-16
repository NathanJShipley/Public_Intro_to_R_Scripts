#==============================================================================
#                              Analysis Repository:                           #
#                       Exploratory Factor Analysis (EFA)                     #      
#==============================================================================

#===========================================
# Load Libraries                           #
#==========================================-
library(psych) #has nice describe function for descriptive info
library(tidyverse) #has dplyr and ggplot2
library(lavaan) #CFA and dataset
library(ggpubr) #has some more GG plots
library(nFactors) #parallel analysis
library(GPArotation) #Used in factor creation
library(mvnormtest) #normality
library(corrplot) #nice cor plot


#===========================================
# Notes                                    #
#==========================================-
# One common technique similar to EFA
# PCA - principle components analysis
# PCA represents a reduction of dimensional 
# The final outcome is a summed variable that includes both shared and unshared variance 
# EFA represents the reduction of variance into a single factor, that factor reflects a latent relationship to the DVS
# In other words, a factor reflects the shared variance of the items, rather than the shared/unshared of PCA
# PCA is more common in machine learning and dimension reduction of unknown relationships 
# EFA would be more common in psych research, hypo relationships in data

# See this for a graphical depiction https://community.jmp.com/t5/image/serverpage/image-id/5945i6C9C25C89DA06AA7/image-dimensions/697x303?v=v2


#===========================================
# Simulate Data OR LOAD DATA               #
#==========================================-
set.seed(101)

data("HolzingerSwineford1939")
dat <- HolzingerSwineford1939

#dat <- read.csv("fake.data.csv", header=T)
#dat <- readxl::read_xlsx("fake.data.xlsx")


#===========================================
# Assess Descriptives                      #
#==========================================-

# Mean, SD, Median, Min, Max, Skewness, and Kurtosis for y
psych::describe(dplyr::select(dat,7:15))
# Check for no extreme skewness or kurtosis, if so, may have to perform other analyses

# Visualizations for distributions
boxplot(dplyr::select(dat,7:15))

# Look at density for all variables, just one shown
ggdensity(dat$x1)

# Look at QQ plots for all variables, just one shown
ggqqplot(dat$x1)

# Preliminary Exploration of Associations
pairs.panels(dplyr::select(dat,7:15), scale=TRUE)
corrplot(cor(dplyr::select(dat,7:15), use="complete.obs"), order = "hclust", tl.col='black', tl.cex=.75) 


#===========================================
# Exploratory Factor Analysis              #
#==========================================-

#====================== =
# Normality           # #
#====================== =

# First check assumption of normality in data
# Can do this univariatly, but also multi
mshapiro.test(t(dplyr::select(dat,7:15)))
# If non-normal, should refrain from using maximum likelihood as factor extraction

# Assess Multivariate Outliers
psych::outlier(dplyr::select(dat,7:15))

#====================== =
# Sphericity          # #
#====================== =

# Want to test Sphericity, that is, are there redundancies in data that can summarize with a smaller # of factors
# First create a correlation table
cor.table <- cor(dplyr::select(dat,7:15), use = "pairwise.complete.obs")

# Use Bartlett's test to determine possible redundancy
# Null hypo is that variables are ORTHOGONAL (not correlated)
cortest.bartlett(cor.table, n = nrow(dat)) # If sig, suggests this data can be factored well

# Also test Kaiser-Meyer-Olkin measure
KMO(cor.table) #KMO test
# Overall KMA above .7, with each item being close or above .7 

# Both Bart and KMO indicate this data can be factored

#====================== =
# Eigenvalues         # #
#====================== =

# Next step is to examine the possible number of factors
# Eigenvalues are used here

ev.cor <-eigen(cor.table) # Note we have 9 items in this scale, add to the next couple lines
ev.cor$values # How many factors have eigenvalues greater than 1???
ev.cor$values/9 # Divide by # of items, how many factors explain more than 10% of variance? 

# There are 3 factors with eigenvalues greater than 1
# also 3 factors with eigenvalues that explain more than 10% of variance

#====================== =
# Parallel Analysis   # #
#====================== =

# Now run an parallel analysis
# Looks at eigenvalues predicted compared to eigenvalues of a null-like model (monte-carlo matrix random data, same size)
PA.test <- parallel(subject = nrow(dat), var = ncol(dat[,7:15]), rep = 100, cent = .95, model = "components")
PA.test
PA.test$eigen$qevpea
which(ev.cor$values > PA.test$eigen$qevpea) #This suggests the number based on parallel

#Now look at scree plot
plotnScree(nScree(x = ev.cor$values, aparallel = PA.test$eigen$qevpea, model = "components")) 
# Shows eigenvalues and parallel analysis
# further evidence on the number of factors 


#====================== =
# How many factors?   # #
#====================== =

# This is where EFA goes from being data driven to more of an art
# There really is no one optimal #, so we will look at what everything tells us
# Then make an educated decision

# How many factors does everything suggest?
#theory = 3 (the data collected was theorized to reflect 3 factors)
#scree = 3
#parallel = 3
#Kaiser = 3 (eigenvalues)

# Together, all of these metrics suggest a 3 factor solution is ideal


#===========================================
# Create & Examine Factors                 #
#==========================================-

# Evidence for 3 factors
# EFA uses rotations, recommended to do oblimin, others include promax, varimax, etc.
efa3 <- fa(dat[,7:15], nfactors = 3, rotate = "oblimin")
print(efa3 , cut = .3, sort = T)

# Here is where we examine factor loadings
# Can see good factor loadings on factor 1
# Some cross loading for item 9
# Some generally low loadings for factor 3
# Many other metrics can be found here (RMSR, Tucker Lewis, RMSEA)

efa3.scores <- factor.scores(dat[,7:15], f=efa3, method = "Bartlett")
efa3.scores.pred <- as.data.frame(as.array(efa3.scores$scores))
dat <- cbind(dat,efa3.scores.pred)

# This has created std scores for each row for each factor
# These can be viewed 
psych::describe(dplyr::select(dat,16:18))

# Preliminary Exploration of Associations
pairs.panels(dplyr::select(dat,16:18), scale=TRUE)

