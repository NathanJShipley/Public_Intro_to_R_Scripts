#==============================================================================
#                              Analysis Repository:                           #
#                       Structural Equation Modeling (SEM)                    #      
#==============================================================================

#===========================================
# Load Libraries                           #
#==========================================-
library(lavaan) #CFA and dataset
library(simsem) #
library(quantreg) #
library(devtools) #


#===========================================
# Overview                                 #
#==========================================-

#If you are doing anything even slightly complicated in SEM, standard power calculations are not appropriate
#Reporting something like "we have 80% power for a correlation of .3" tells you nothing about the power of parameters in your model
#Luckily, Monte Carlo simulation can give you estimates of power for the exact model you intend to test

#To accomplish this, you can follow these steps-

#Step 1: Specify a population model with fixed parameters
#Step 2: Generate a large number of datasets from this population
#Step 3: Test your analytic model on each dataset
#Step 4: Calculate average parameter estimates, and proportion of significant effects (i.e., power)
#Step 5: Repeat the above steps in order to see the effect of varying population parameters or sample size


#===========================================
# Step 1 #
#==========================================-

# Lets run a simple mediation model
# we have 3 latent variables, X, Y, M
# Each latent variable has 3 indicators
# nothing is freely estimated, fix everything!
# Including regression paths, variances, loadings

#What is the power for the indirect effect of X on Y via M?
# let's assume standardized pathways of .3 for our regressions and also assume factor loadings of .7

med.sim.pop<-'
# Factor loadings(loadings set to .7, can adjust)
X =~ .7*x1 + .7*x2 + .7*x3
Y =~ .7*y1 + .7*y2 + .7*y3
M =~ .7*m1 + .7*m2 + .7*m3

# Regression paths set to .3 for all regressions (can adjust)
Y ~ .3*X + .3*M
M ~ .3*X

# Variances (note that variance for factors are unexplained variance, need to subtract the explained paths above)
# Thus for factor variances, need to subtract explained from total variance (1 - explained)
X ~~ 1*X 
Y ~~ (1-(.3^2)-(.3^2))*Y 
M ~~ (1-(.3^2))*M 

# For indicators, left over is what is not accounted for by loadings
# In this case loadings of .7, variance left over for each item would be (.7 * .7)
x1 ~~ .51*x1
x2 ~~ .51*x2
x3 ~~ .51*x3
y1 ~~ .51*y1
y2 ~~ .51*y2
y3 ~~ .51*y3
m1 ~~ .51*m1
m2 ~~ .51*m2
m3 ~~ .51*m3
'

# AGAIN NOTE this is a hypo population!
# We use this model below
# Again, this is treated as the IMPLIED data set

# Now we are going to build the model we want to actually test! 
med.sim.ana<-'
# Factor Loadings
X =~ x1 + x2 + x3
Y =~ y1 + y2 + y3
M =~ m1 + m2 + m3

# Regressions
Y ~ X + b*M
M ~ a*X

# Indirect effefct
ind:=a*b
'

#===========================================
# Step 2, 3, 4, and 5 #
#==========================================-

# Now time to simulate the model
# med.sim.ana = analytic model we are evaluating
# med.sim.pop = implied dataset
# We are going to assess power for different sample sizes ranging from 100 to 500, in 50 point increments, drawing this 100 times

# Note this will take some time to run
med.sim<-sim(NULL, med.sim.ana, n = rep(seq(100,500,50),100), generate = med.sim.pop, lavaanfun = "cfa", std.lv = T)

summary(med.sim)
# Useful, but better to look down at FindPower Output
# fit indices = different indices at different cut off points
# we care about parameter estimates and standard error part of the table
# MOSTLY care about that power is not equal to zero column
# lets plot the different fit statistic cut offs

plotCutoff(med.sim, .05)
findPower(getPower(med.sim), "N",.8)
# find power for each sample size where we get 80% power
# INF = all models were over 80% powe
# if 100 = means with small sample size, 100% over 80% sample
# INF again always 100% power, 100 means that at 100 people, atleast over 80% with 100 people
# find the largest number will tell you the minimum sample size needed to get atleast 80% for the weakest parameter
# SO we can see that in order to achieve 80% power for indirect effect we would need a sample of 261

plotPower(med.sim, powerParam = c("a",'b',"ind"))
# indirect is the product of two terms, less 

Cpower<-continuousPower(med.sim)
Cpower
# gets power for each sample size for each parameter


#===========================================
# Additional analyses    #
#==========================================-

# what if we think this is a little bit too optimistic for our effect sizes... maybe we will only get closer to .2 for the indirect pathways
# reduce parameters to .2 rather than .3
# thus indirect pathway produce will be smaller 

med.sim.pop.small<-'
X =~ .7*x1 + .7*x2 + .7*x3
Y =~ .7*y1 + .7*y2 + .7*y3
M =~ .7*m1 + .7*m2 + .7*m3

Y ~ .3*X + .2*M
M ~ .2*X

X ~~ 1*X
Y ~~ (1-(.3^2)-(.2^2))*Y
M ~~ (1-(.2^2))*M

x1 ~~ .51*x1
x2 ~~ .51*x2
x3 ~~ .51*x3
y1 ~~ .51*y1
y2 ~~ .51*y2
y3 ~~ .51*y3
m1 ~~ .51*m1
m2 ~~ .51*m2
m3 ~~ .51*m3
'

# replaced generate population with our small population but kept our original exploratory model
med.sim.small<-sim(NULL, med.sim.ana, n = rep(seq(100,500,50),100), generate = med.sim.pop.small, lavaanfun = "cfa", std.lv = T)

summary(med.sim.small)
plotCutoff(med.sim.small, .05)
findPower(getPower(med.sim.small), "N",.8)
# NOW we can see that indirect is NA
plotPower(med.sim.small, powerParam = c("a",'b',"ind"))
# can see that we never appraoch 80% for the indirect effect
# ouch! pretty small change, leads to a big drop in power

# lets run one final simulation, increasing our sample sizes to 1000
# THIS WILL TAKE AWHILE TO RUN! 
med.sim.small.1000<-sim(NULL, med.sim.ana, n = rep(seq(100,1000,50),100), generate = med.sim.pop.small, lavaanfun = "cfa", std.lv = T)

summary(med.sim.small.1000)
plotCutoff(med.sim.small.1000, .05)
findPower(getPower(med.sim.small.1000), "N",.8)
# NOW we can see that in order to acheive 80% power for our small indirect effect we would need a sample size of 558
plotPower(med.sim.small.1000, powerParam = c("a",'b',"ind"))







