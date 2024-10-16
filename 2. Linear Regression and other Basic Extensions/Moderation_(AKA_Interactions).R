#==============================================================================
#                              Analysis Repository:                           #
#                               Linear Regression                             #      
#==============================================================================

#===========================================
# Load Libraries                           #
#==========================================-

library(psych) #has nice describe function for descriptive info
library(tidyverse) #has dplyr and ggplot2
library(olsrr) #Some good regression functions and plotting tools
library(jtools) #summ function good to get regression output
library(ggpubr) #has some more GG plots
library(corrplot) #nice cor plot


#===========================================
# Overview                                 #
#==========================================-

# Going to assume that if running moderation
# then all other normal tests for linear regression have been done
# Therefore, very little additional code will be described here

# AN INTERESTING NOTE ON CENTERING
# Recommended to center when using interaction effects 
# Recommended because it lessens the correlation when using interaction terms
# Makes interpretation easier

# DON'T mean center when
# If any continuous predictors have meaningful values at 0
# If not interaction terms are being used

# Found this very easy to understand code from here
# https://ademos.people.uic.edu/Chapter13.html

#===========================================
# Simulate Data OR LOAD DATA               #
#==========================================-

set.seed(101)

n <- 1000

X <- rnorm(n, 2.75, .75)   
Z <- rnorm(n, 15, 15)   
Y <- .7*X + .3*Z + 2.5*X*Z + rnorm(n, sd = 5)

dat <- data.frame(DV=Y, IV=X, Mod=Z)

#dat <- read.csv("data.csv", header=T)
#dat <- readxl::read_xlsx("fake.data.xlsx")

#===========================================
# Assess Descriptives                      #
#==========================================-

# Mean, SD, Median, Min, Max, Skewness, and Kurtosis for y
psych::describe(dat)
# Check for no extreme skewness or kurtosis, if so, may have to perform other analyses

# Visualizations for distributions
hist(dat$DV)
ggdensity(dat$DV)
boxplot(dat$DV)
ggqqplot(dat$DV)

# Preliminary Exploration of Associations
pairs.panels(dat, scale=TRUE)
corrplot(cor(dat, use="complete.obs"), order = "hclust", tl.col='black', tl.cex=.75) 

# Scatter plot of two variables
ggplot(dat,aes(x1,y)) +
  geom_point() + 
  geom_smooth()

#===========================================
# Build Model & Get Output                 #
#==========================================-

# Going to build a simple model, one with and one without the interaction
Reg.fit <- lm(DV ~ IV + Mod, data=dat)
Reg.fit.INT <- lm(DV ~ IV * Mod, data=dat)

# Compare the fit and reg coefs
jtools::summ(Reg.fit, digits=3)
jtools::summ(Reg.fit, part.corr=TRUE, vifs=TRUE, digits=3)
olsrr::ols_correlations(Reg.fit.INT)

jtools::summ(Reg.fit.INT, digits=3)
jtools::summ(Reg.fit.INT, part.corr=TRUE, vifs=TRUE, digits=3)
olsrr::ols_correlations(Reg.fit.INT)
# The stats indicate the interaction between IV and Mod is SIG!!!
# Interpret that for every one unit in MOD, the effect of IV on the DV goes up 2.5


# Quickly plot estimated relationships 
jtools::effect_plot(Reg.fit, pred = IV, interval = TRUE, plot.points = TRUE)
jtools::effect_plot(Reg.fit, pred = Mod, interval = TRUE, plot.points = TRUE)

jtools::effect_plot(Reg.fit.INT, pred = IV, interval = TRUE, plot.points = TRUE)
jtools::effect_plot(Reg.fit.INT, pred = Mod, interval = TRUE, plot.points = TRUE)


#===========================================
# Mean Centering Model                 #
#==========================================-

# As discussed above, it is common to also mean center when using interactions
# Lets go ahead and run a mean centered model as well

dat$IV.C <- scale(dat$IV, center = TRUE, scale = FALSE)
dat$Mod.C <- scale(dat$Mod, center = TRUE, scale = FALSE)

# Run model
Reg.fit.INT.C <- lm(DV ~ IV.C * Mod.C, data=dat)
jtools::summ(Reg.fit.INT.C, part.corr=TRUE, vifs=TRUE, digits=3)

# Can tell that this is EXTREMLY helpful for interpreting the coefs, as the intercept is now not negative! 


#===========================================
# Plotting                                 #
#==========================================-

theme_set(
  theme_bw()
)

# Best way to interpret an interaction is through visualziations! 
# This can be done a number of ways, through hand selection to SD
# Lets do both a hand selected version and one that uses SD

#====================== =
# #   PLot by hand    # #
#====================== =
# First - pick values by hand
# Going to just pick values of mod and dv

Inter.effects.hand <- effect('IV.C*Mod.C', Reg.fit.INT.C,
                             xlevels = list(Mod.C = c(-10,0,10),
                                            IV.C = c(-.5,0,.5)),
                             se = T, confidence.level=.95, typical = mean)

Inter.effects.hand <- as.data.frame(Inter.effects.hand)

head(Inter.effects.hand)

# Now will create factors for the two variables

Inter.effects.hand$IV.C <- factor(Inter.effects.hand$IV.C,
                                  levels = c(-.5,0,.5),
                                  labels = c(".5 below", "Mean", ".5 Above"))

Inter.effects.hand$Mod.C <- factor(Inter.effects.hand$Mod.C,
                                   levels = c(-10,0,10),
                                   labels = c("10 below", "Mean", "10 Above"))

ggplot(data=Inter.effects.hand, aes(x=IV.C, y=fit, group=Mod.C))+
  geom_line(size=2, aes(color=Mod.C))+
  ylab("Y")+
  xlab("X")+
  ggtitle("Hand Picked Plot")


#====================== =
# #   PLot using SD    # #
#====================== =

# We can do something very similar as before, but now using SD! 
# Make sure to use the mean-centered values 
psych::describe(dat$IV.C)
psych::describe(dat$Mod.C)

Inter.effects.sd <- effect('IV.C*Mod.C', Reg.fit.INT.C,
                             xlevels = list(Mod.C = c(-31.24,-15.62,0,15.62,31.24),
                                            IV.C = c(-1.44,-.72,0,.72,1.44)),
                             se = T, confidence.level=.95, typical = mean)

Inter.effects.sd <- as.data.frame(Inter.effects.sd)

head(Inter.effects.sd)

# Now will create factors for the two variables

Inter.effects.sd$IV.C <- factor(Inter.effects.sd$IV.C,
                                  levels = c(-1.44,-.72,0,.72,1.44),
                                  labels = c("2 SD Below", "1 SD Below", "Mean", "1 SD Above", "2 SD Above"))

Inter.effects.sd$Mod.C <- factor(Inter.effects.sd$Mod.C,
                                   levels = c(-31.24,-15.62,0,15.62,31.24),
                                   labels = c("2 SD Below", "1 SD Below", "Mean", "1 SD Above", "2 SD Above"))

ggplot(data = Inter.effects.sd, aes(x=IV.C, y=fit, group=Mod.C))+
  geom_line(size=2, aes(color=Mod.C)) +
  ylab("Y") +
  xlab("X") +
  ggtitle("SD Plot")

  
ggplot(data = Inter.effects.sd, aes(x=Mod.C, y=fit, group=IV.C))+
  geom_line(size=2, aes(color=IV.C)) +
  geom_ribbon(aes(ymin=fit-se, ymax=fit+se,fill=IV.C),alpha=.2) +
  ylab("Y") +
  xlab("Moderator") +
  ggtitle("SD Plot") + 
  theme_bw()+
  theme(text = element_text(size=12),
        legend.text = element_text(size=12),
        legend.direction = "horizontal",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position="top")


#====================== =
# #   Categorical Moderators    # #
#====================== =

# Do something VERY similar but make sure the categorical factor is factorized
# Lets simulate data to get a view for this

N <- 250
# Create linear normal IV
X <- rnorm(n, 2.75, .75)
# Create cagetgocial binary variable, gender
G <- sample(rep(c(0,1),N),N,replace = FALSE)    
Y <- .7*X + .3*G + 2*X*G + rnorm(n, sd = 5)     
Y = (Y - min(Y)) / (max(Y) - min(Y))*4
GPA.Data<-data.frame(GPA=Y, Work.Ethic=X, Gender=G)   

#Don't forget to center continuous variables 
GPA.Data.2$Work.Ethic.C <- scale(GPA.Data$Work.Ethic, center = TRUE, scale = FALSE)[,]

# Now create factor/dummy code for male and female
GPA.Data.2$Gender.F <- factor(GPA.Data.2$Gender,    
                              level=c(0,1),    
                              labels=c("Male","Female"))

# Now run model
GPA.2.Model.2 <- lm(GPA~Work.Ethic.C*Gender.F, GPA.Data.2)


# Create effects
Inter.GPA.2 <- effect('Work.Ethic.C*Gender.F', GPA.2.Model.2,
                      xlevels=list(Work.Ethic.C = c(-1.1, 0, 1.1)),
                      se=TRUE, confidence.level=.95, typical=mean)

Inter.GPA.2<-as.data.frame(Inter.GPA.2)

Inter.GPA.2$Work.Ethic<-factor(Inter.GPA.2$Work.Ethic.C,
                               levels=c(-1.1, 0, 1.1),
                               labels=c("Poor Worker", "Average Worker", "Hard Worker"))

Inter.GPA.2$Gender<-factor(Inter.GPA.2$Gender.F,
                           levels=c("Male", "Female"))
# NOTE creating a new varible, identical to Gender.F, but calling it just gender for plot

ggplot(data=Inter.GPA.2, aes(x=Work.Ethic, y=fit, group=Gender))+
  geom_line(size=2, aes(color=Gender))+
  ylab("GPA")+
  xlab("Work Ethic")+
  ggtitle("Work Ethic and Gender as Predictors of GPA")+
  theme_bw()+ 
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank())+
  scale_fill_grey()

