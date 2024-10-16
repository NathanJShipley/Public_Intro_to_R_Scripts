#==============================================================================
#                              Analysis Repository:                           #
#                          Latent Profile Anlysis (LPA)                       #      
#==============================================================================

#===========================================
# Load Libraries                           #
#==========================================-
library(psych) #has nice describe function for descriptive info
library(tidyverse) #has dplyr and ggplot2
library(mclust) #primary package for LPA
library(ggpubr) #has some more GG plots
library(reshape2) #used for graphs
library(car) #ANOVSA
library(lsr) #eta-squared
library(multcomp) #needed for multiple comparisons in ANOVA

#===========================================
# Simulate Data OR LOAD DATA               #
#==========================================-

dat <- read.csv("data/farmer_type.csv", header=T)
#dat <- readxl::read_xlsx("fake.data.xlsx")


#===========================================
# Assess Descriptives                      #
#==========================================-

# Mean, SD, Median, Min, Max, Skewness, and Kurtosis for y
psych::describe(dat)
# Check for no extreme skewness or kurtosis, if so, may have to perform other analyses

# Visualizations for distributions
boxplot(dplyr::select(dat,2:4))

# Look at density for all 3 variables 
ggdensity(dat$Livestock)
ggdensity(dat$Rec)
ggdensity(dat$Row)

# Look at QQ plots for all 3 variables 
ggqqplot(dat$Livestock)
ggqqplot(dat$Rec)
ggqqplot(dat$Row)

# Preliminary Exploration of Associations
pairs.panels(dplyr::select(dat,2:4), scale=TRUE)

# Assess Multivariate Outliers
psych::outlier(dplyr::select(dat,2:4))

#===========================================
# Overview                                 #
#==========================================-

# Estimating latent profiles using MCLUST is a series of integrated steps
# This is part data informed, exploration, and overall feel/ finesse

# Will first create multiple models, plot their BIC and ICL to help selection
# Want model with lowest BIC and ICL
# ICL is a slight variation, tends to refine BIC estimates
# ICL favors well-seperated groups

# NOTE that mclust creates multiple groupings 
# e.g., EII, VII, EEI, etc
# Each differ in their Distribution, Volume, Shape, and Orientation
# EII is the most restrictive, with VVV being the most flexible
# These are all shown in the BIC plot output
# Read more here https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5096736/

#====================== =
# BIC                 # #
#====================== =

BIC.farmer <- mclustBIC(na.omit(dplyr::select(dat,2:4)))
plot(BIC.farmer)
# Seems to be too many groups estimated
# 5 groups is likely the most helpful
# This will 

# Note can select specific models and max groups as desired
BIC.farmer.v2 <- mclustBIC(na.omit(dplyr::select(dat,2:4)),G=1:5)
plot(BIC.farmer.v2)
# Appears that EVI 3, VVI 3, and VEI 3 are good models
summary(BIC.farmer.v2)
# Confirms, but again many groups, lets further refine

# A further note is that LPAs can be done in other programs
# Those familair with software like MPLUS will be used to having only ONE model
# Common models used generally are EEI, VVI, EEE, VVV

BIC.farmer.v3 <- mclustBIC(na.omit(dplyr::select(dat,2:4)),G=1:4,modelName = c("EEI","VVI","EVI","VEI","EEE","VVV"))
plot(BIC.farmer.v3)
summary(BIC.farmer.v3)
# Looks like EVI 3, VVI 3, and EEI 4 are good fit!
# Now see how they fit in ICL

#====================== =
# ICL                 # #
#====================== =

ICL.farmer.v3 <- mclustICL(na.omit(dplyr::select(dat,2:4)),G=1:4,modelName = c("EEI","VVI","EVI","VEI","EEE","VVV"))
plot(ICL.farmer.v3)
summary(ICL.farmer.v3)
# Favors VVI, EVI models
# helps refine selection

#====================== =
# Bootstrap           # #
#====================== =

# Now can examine each series of models to ID those where the number of components is optimized
farmer.EEI <- mclustBootstrapLRT(dplyr::select(dat,2:4), modelName = "EEI") 
farmer.VVI <- mclustBootstrapLRT(dplyr::select(dat,2:4), modelName = "VVI") 
farmer.EVI <- mclustBootstrapLRT(dplyr::select(dat,2:4), modelName = "EVI") 
farmer.EEI # shows improvement up to 5, but big jump to 3 groups
farmer.VVI # Only fit up to 3 groups
farmer.EVI # Only fit up to 3 groups
# All suggest that 3 groups is ideal

#====================== =
# Model creation      # #
#====================== =

# Now create models, one for each of the above using 3 groups as suggested be optimal
mod_EEI_3 <- Mclust(dplyr::select(dat,2:4), modelNames ="EEI", G = 3, x = BIC.farmer) 
mod_VVI_3 <- Mclust(dplyr::select(dat,2:4), modelNames ="VVI", G = 3, x = BIC.farmer) 
mod_EVI_3 <- Mclust(dplyr::select(dat,2:4), modelNames ="EVI", G = 3, x = BIC.farmer) 

# Assess the mixing probabilteis and other fit info
summary(mod_EEI_3)
summary(mod_EEI_3,parameters = TRUE) # Has good mixing probabilities, each group over 20%
summary(mod_VVI_3)
summary(mod_VVI_3,parameters = TRUE) # also good mixing probs
summary(mod_EVI_3)
summary(mod_EVI_3,parameters = TRUE)

#====================== =
# Model visualziation # #
#====================== =

# Now will plot these models to visualize 
# EEI model
means.EEI.3 <- data.frame(mod_EEI_3$parameters$mean, stringsAsFactors = FALSE) %>%
  rownames_to_column() %>%
  rename(Meaning = rowname) %>%
  melt(id.vars = "Meaning", variable.name = "Profile", value.name = "Mean")



means.EEI.3  %>%
  ggplot(aes(Meaning, Mean, group = Profile, color = Profile)) +
  geom_point(size = 2.25) +
  geom_line(size = 1.25) +
  labs(x = NULL, y = "RAW mean Importance") +
  theme_bw(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "top")

# VVI Model
means.VVI.3 <- data.frame(mod_VVI_3$parameters$mean, stringsAsFactors = FALSE) %>%
  rownames_to_column() %>%
  rename(Meaning = rowname) %>%
  melt(id.vars = "Meaning", variable.name = "Profile", value.name = "Mean")

means.VVI.3  %>%
  ggplot(aes(Meaning, Mean, group = Profile, color = Profile)) +
  geom_point(size = 2.25) +
  geom_line(size = 1.25) +
  labs(x = NULL, y = "RAW mean Importance") +
  theme_bw(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "top")

# EVI model
means.EVI.3 <- data.frame(mod_EVI_3$parameters$mean, stringsAsFactors = FALSE) %>%
  rownames_to_column() %>%
  rename(Meaning = rowname) %>%
  melt(id.vars = "Meaning", variable.name = "Profile", value.name = "Mean")

means.EVI.3  %>%
  ggplot(aes(Meaning, Mean, group = Profile, color = Profile)) +
  geom_point(size = 2.25) +
  geom_line(size = 1.25) +
  labs(x = NULL, y = "RAW mean Importance") +
  theme_bw(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "top")

# VVI was indicated by BIC and ICL as being a good fit
# it has good mixing probabilities and is more of a "normal" model 
# Will use this grouping for now

#====================== =
# Entropy             # #
#====================== =

# Create multiple models, each with different group #
mod_VVI_1 <- Mclust(dplyr::select(dat,2:4), modelNames ="VVI", G = 1, x = BIC.farmer) 
mod_VVI_2 <- Mclust(dplyr::select(dat,2:4), modelNames ="VVI", G = 2, x = BIC.farmer) 
mod_VVI_3 <- Mclust(dplyr::select(dat,2:4), modelNames ="VVI", G = 3, x = BIC.farmer) 
mod_VVI_4 <- Mclust(dplyr::select(dat,2:4), modelNames ="VVI", G = 4, x = BIC.farmer) 

## Assess Entropy for models ##
# Note must set the # of individuals/rows and the number of groups
# 126 is the number in this data
numerator1 <- -sum(mod_VVI_1$z * log(mod_VVI_1$z))
denomenator1 <- 126*log(1)
entropy1 = 1-(numerator1/denomenator1)

numerator2 <- -sum(mod_VVI_2$z * log(mod_VVI_2$z))
denomenator2 <- 126*log(2)
entropy2 = 1-(numerator2/denomenator2)

numerator3 <- -sum(mod_VVI_3$z * log(mod_VVI_3$z))
denomenator3 <- 126*log(3)
entropy3 = 1-(numerator3/denomenator3)

numerator4 <- -sum(mod_VVI_4$z * log(mod_VVI_4$z))
denomenator4 <- 126*log(4)
entropy4 = 1-(numerator4/denomenator4)

entropy1
entropy2
entropy3 # highest value, above .8
entropy4 # no convergence

# This information all indicates that a 3 cluster solution best fits this data
# Can use this information to cluster the data into 3 groups
 
#====================== =
# Extract Groupings   # #
#====================== =

# Extract Classes
farmer.VVI.3.Classified <- as.array(mod_VVI_3$classification)

# Merge into data
dat$VVI.3 <- as.factor(as.array(farmer.VVI.3.Classified))


#====================== =
# Validate Groups     # #
#====================== =

# Will use the groups to compare their difference using an ANOVA
# Create Model
AOV.fit.Livestock <- aov(Livestock ~ VVI.3, data = dat)
AOV.fit.Rec <- aov(Rec ~ VVI.3, data = dat)
AOV.fit.Row <- aov(Row ~ VVI.3, data = dat)

# Evaluate fit
Anova(AOV.fit.Livestock, type = "III")
Anova(AOV.fit.Rec, type = "III")
Anova(AOV.fit.Row, type = "III")
# Are there sig differences in scores between groups?

# Pairwise comparisons
summary(glht(AOV.fit.Livestock, linfct = mcp(VVI.3 = "Tukey"))) # 
summary(glht(AOV.fit.Rec, linfct = mcp(VVI.3 = "Tukey"))) # 
summary(glht(AOV.fit.Row, linfct = mcp(VVI.3 = "Tukey"))) # 

# Effect size
etaSquared(AOV.fit.Livestock, type = 3, anova = FALSE) #
etaSquared(AOV.fit.Rec, type = 3, anova = FALSE) #
etaSquared(AOV.fit.Row, type = 3, anova = FALSE) #

# Visualize
boxplot(Livestock ~ VVI.3, data=dat)
boxplot(Rec ~ VVI.3, data = dat)
boxplot(Row ~ VVI.3, data = dat)

# Get values
psych::describeBy(dat$Livestock, dat$VVI.3)
psych::describeBy(dat$Rec, dat$VVI.3)
psych::describeBy(dat$Row, dat$VVI.3)