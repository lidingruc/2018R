##########################################################################
###
### SPATIAL AUTOCORRELATION EXPLORATION
### KENTUCKY DEMO 3
### LAST UPDATED APRIL 28, 2011
### RUN SUCCESSFULLY IN R VERSION 2.13.0 ON 5/4/11
###
###########################################################################
###
### SPATIAL AUTOCORRELATION IN R
### IN THIS DEMO WE:
###     1. Remind ourselves of the outcome of Demos 1 & 2
###     2. Fit an OLS linear regression model
###     3. Examine the results
###     4. In particular, examine the OLS residuals
###     5. Try to correct for non-normality & heteroskedasticity
###        a. Formal tests for normal residuals
###        b. Formal tests for homoskedasdistic residuals
###     6. Try EGLS, a variance stabilizing approach
###     7. Check residuals for spatial autocorrelation
###     8. Demonstrate Moran Scatterplot in R, and in doing so...
###     9. Demonstrate one of the advantages of working in an *open
###        source* environment
###
###########################################################################

# Set working directory:

# setwd("C:/Teaching/Shape Files/Southern Counties Shape file")

# Load the necessary packages:

library(classInt)
library(car)
library(graphicsQC)
library(lmtest)
library(sp)
library(spdep)
library(spatstat)
library(RColorBrewer)
library(nortest)

############################################################################
###
### READ IN THE SHAPE FILE
###

soco <- readShapePoly("south00.shp")
summary(soco)    # Nice 6-number summary of each variable

# READ IN THE .txt FILE

socotxt <- read.table('south00.txt', header=T)

# Convert our SpatialPolygonsDataFrame object to a data.frame object
# needed for some of our analysis:

soco.df <- data.frame(soco)

# Add the nice county names from socotxt to soco.df, and rename
# the column "NAME".

soco.df[,1] <- socotxt[,1]
names(soco.df)[1] <- "NAME"
names(soco.df)

### Demos 1 & 2 helped us choose the transformations we wish to use.
### By way of example, let's be somewhat simple minded about this and
### decide that a good model to predict child poverty would be to use
### the county unemployment rate, the proportion of female headed
### families with kids but no spouse, and the proportion of persons age
### 18+ with more than a HS education (with all variables transformed
### as determined in Demos 1 & 2)  And let's pretend we know nothing at
### this point about spatial modeling.  The regression specification
### notation in R is commonly referred to as Wilkinson-Rogers notation.

### We use the square root transformation of PPOV as arrived at in Demo 1.
### Attach soco.df as the default data set.

attach(soco.df)
class(soco.df)     # data.frame
names(soco.df)

# Just to see how much we've improved the variables, let's check some
# correlations (we look for stronger correlations for the transformed
# variables):

cor(PPOV, PFHH)               # Nothing transformed;  0.558
cor(PPOV, PUNEM)              # Nothing transformed;  0.723
cor(PPOV, (1-PHSLS))          # Nothing transformed; -0.530
cor(PFHH, PUNEM)              # Nothing transformed;  0.516
cor(PFHH, (1-PHSLS))          # Nothing transformed; -0.086
cor(PUNEM, (1-PHSLS))         # Nothing transformed; -0.306
cor(sqrt(PPOV), PFHH)         # PPOV transformed;  0.551; weaker
cor(sqrt(PPOV), PUNEM)        # PPOV transformed;  0.707; weaker
cor(sqrt(PPOV), (1-PHSLS))    # PPOV transformed; -0.557; stronger
cor(sqrt(PPOV), sqrt(PFHH))   # Both transformed;  0.549; weaker
cor(sqrt(PPOV), sqrt(PUNEM))  # Both transformed;  0.709; weaker
cor(sqrt(PPOV), log(1-PHSLS)) # Both transformed; -0.563; stronger

# Mixed results; but generally disappointing.  Failure to show big
# improvement likely results from the fact that our untransformed
# DV was reasonably symmetric, and relationships with the untransformed
# covariates were reasonably linear.  Moreover the transformations
# don't help us much with the potential outliers.

# Now, we create some new variable names so that R doesn't have to do
# the transformation afresh every time a variable is called upon:

cpov <- sqrt(PPOV)
femhh <- sqrt(PFHH)
unem <- sqrt(PUNEM)
hied <- log(1-PHSLS)

#####################################################################
###
### TO BEGIN, FIT AND TEST AN OLS LINEAR REGRESSION MODEL
###
#####################################################################

reg1 <- lm(cpov ~ femhh + unem + hied)
summary(reg1)

# These should be exactly the same results as we get in GeoDa
# Let's see how things look.  More EDA:

###
### OLS DIAGNOSTICS
###

# Begin with an examination of the residuals and test for constant
# residual variance and possible nonlinearities (see Faraway 2005:Ch.4)

names(reg1)  # Check to see how to recover the y-hat values

yhat <- reg1$fitted.values
y <- sqrt(PPOV)
summary(y)    # Did this to recover the min and max values of y for plot
summary(yhat) # Find plot range

row.names(soco.df) <- soco.df$NAME
plot(yhat, y, ylim=c(0,0.8))
lines(lowess(y ~ yhat), lwd=2, col=4)
abline(lm(y ~ yhat), lwd=2, col=2)
identify(yhat, y, row.names(soco.df), cex=0.8)

# Maybe not so bad.  Reasonably tight except for maybe 25-30 points.
# Many Texas counties are problematic.  Outliers?

### Cook & Weisberg have a preference for a plot of yhat against y.  See
### R. Dennis Cook & Sanford Weisberg, "Transforming a Response
### Variable for Linearity." Biometrika (1994) 81(4):731-737.  Especially
### see pp. 732-3.  They call it the "inverse response plot":

plot(y, yhat, xlab="y", ylab="yhat", xlim=c(0.1,0.8), ylim=c(0,0.8))
lines(lowess(yhat ~ y), lwd=2, col=4)
abline(lm(yhat ~ y), lwd=2, col=2)
identify(y, yhat, row.names(soco.df), cex=0.8)

# Pretty much the same story.  Texas!

# Generate the pseudo R-square statistic.  It's the same as the OLS
# R-square, but we'll hold on to this for when we do MLE.

cor(yhat,y)^2    # 0.6769
summary(reg1)    # Yup.  It's the same.

######################################################################
###
### OLS RESIDUAL DIAGNOSTICS
###
######################################################################

# Let's begin by taking a look at the symmetry of the residuals:

me1 <- mean(residuals(reg1))
me1    # Effectively zero
sd1 <- sd(residuals(reg1))
sd1    # 0.055
summary(residuals(reg1))  # reasonably symmetric; that's what we want.

hist(residuals(reg1), breaks=seq(-.3, .4, .01), col=8, probability=T,
	ylab='Density', main='Histogram of Residuals(reg1)',
	xlab='Residuals(reg1)')
box()
curve(dnorm(x, mean=me1, sd=sd1), from=-.2, to=.2, add=T, col='red', lwd=2)
leg.txt <- c("Residuals(reg1)", "Min. =        -0.224",
	"Max.=         0.366",
	"Mean =       0.000", "Median =   -0.002", "Std. dev. =  0.055")
legend (x=0.12, y=8.7, leg.txt)

# Histogram not too bad, but clear signs of some serious outliers.
# Let's look at QQ-Plot:

qqPlot(residuals(reg1), distribution="norm",
	xlab='', main='Quantile Comparison Plot reg1 esiduals',
	envelope=.95, labels=FALSE, las=0, pch=NA, lwd=2, col="red",
	line="quartiles")
par(new=TRUE)
qqPlot(residuals(reg1), distribution="norm", envelope=FALSE, labels=FALSE,
	pch=1, cex=1, col="black")
par(new=FALSE)

# Yes... serious overdispersion in right tail.  Not good.
# There are a number of formal tests for normality.  Many can be found in
# the R package nortest.  The shapiro.test (Shapiro & Wilks) is in the
# stats package:

ad.test(residuals(reg1))       # reject normality
lillie.test(residuals(reg1))   # reject normality
pearson.test(residuals(reg1))  # reject normality
cvm.test(residuals(reg1))      # reject normaility
sf.test(residuals(reg1))       # reject normality
shapiro.test(residuals(reg1))  # Okay, we get it!  Reject normal residuals

# The next plot clearly informs us that the problems in the right tail
# can be traced to the small Texas counties.  For now, let's continue
# with an examination of the residuals and test first for constant
# residual variance (homoskedasticity).  The following is a very common
# visual plot:

plot(fitted(reg1), residuals(reg1), xlab="Fitted y", ylab= "Residuals",
	main="Plot of Residuals against Fitted y")
abline(h=0)
identify(fitted(reg1), residuals(reg1), row.names(soco.df), cex=0.8)

# Those Texas counties are messing things up.  With the exception of a
# few real klinkers, however, the plot suggests that the residuals
# may not be so bad. 

# When it's difficult to tell, some analysts suggest looking at the
# absolute value of the residuals:

plot(fitted(reg1), abs(residuals(reg1)), xlab="Fitted y",
	ylab= "|Residuals|")
identify(fitted(reg1), abs(residuals(reg1)), row.names(soco.df))

# Some "residual vs carrier" plots:

plot(femhh, residuals(reg1), xlab="SQRT(PFEMHH)",
	ylab= "Residuals", main="Residuals vs SQRT(PFEMHH)")
abline(h=0)
identify(femhh, residuals(reg1), row.names(soco.df), cex=0.8)

# Same outliers identified; but the main visual message is that this
# independent variable is responsible for some of the heteroskedasticity
# in the residuals.

plot(unem, residuals(reg1), xlab="SQRT(PUNEM)",
	ylab= "Residuals", main="Residuals vs SQRT(PUNEM)")
abline(h=0)
identify(unem, residuals(reg1), row.names(soco.df), cex=0.8)

# Except for the outliers, maybe heteroskedasticity not too bad on this
# variable.

plot(hied, residuals(reg1), xlab="LOG(1-PHSLS)",
	ylab= "Residuals", main="Residuals vs LOG(PHSPLUS)")
abline(h=0)
identify(hied, residuals(reg1), row.names(soco.df), cex=0.8)

# Again, maybe variable isn't contributing so much to heteroskedasticity.
# Perhps we should do some more forml testing.

# We can get a plot and transformation diagnostic using the
# spread-level plot {car} suggested by Tukey (1977):

spreadLevelPlot(reg1)

# Don't be concerned about the power transformation diagnostic.  Basically
# what this plot tells us is that the "spread" of the (Studentized) residuals
# is a function of the "level" of the yhat.  We saw this in an earlier plot,
# and it's a troublesome diagnostic outcome.

# As with tests for normality of residuals, also check for homoskedastic
# residuals

bptest(reg1)       # We also reject homoskedastic errors

### Okay.  Again we get the message.  We must reject homoskedastic errors.
### Heteroskedasticity doesn't bias the regression estimates, but it causes
### the parameter standard errors to be larger than they should be under the
### assumption of homoskedasticity.  Thus our parameter estimates are
### inefficient.  So, what to do???  Let's esimate the variances using a
### form of Weighted Least Squares called Estimated Generalized Least
### squares (EGLS)

############################################################################
#
# EGLS:
# The EGLS steps are:
# 1. Save the residuals from the OLS run.  Square them and take natural logs.
# 2. Use the log of the squared residuals as a new dependent variable, and
#    rerun the regression with this DV.  Save the predicted values.
# 3. Create a new variable by exponentiating the predicted values.
# 4. Take the reciprocal of the exponentiated predicted values.
# 5. Run the original regression equation, but use the new variable from
#    step 4 as a regression weight.  The regression analysis has now been
#    approximately corrected for violation of the homoskedasticity assumption.
#
# So let's do it.
#
############################################################################

lnsqresid <- log(residuals(reg1)^2)
reg2 <- lm(lnsqresid ~ femhh + unem + hied)
exppred <- exp(fitted(reg2))
wt <- 1/exppred

# Here's the new regression from step 5:

reg3 <- lm(cpov ~ femhh + unem + hied, weight=wt)

#### LET'S CHECK THE EGLS MODEL FOR NORMALITY OF RESIDUALS

me3 <- mean(residuals(reg3))
me3
sd3 <- sd(residuals(reg3))
sd3
summary(residuals(reg3))
hist(residuals(reg3), breaks=seq(-.3, .4, .01), col=8, probability=T,
	ylab='Density', main='Histogram of Residuals(reg3)',
	xlab='Residuals(reg3)')
box()
curve(dnorm(x, mean=me3, sd=sd3), from=-.2, to=.2, add=T, col='red', lwd=2)
leg.txt <- c("Residuals(reg3)", "Min. =        -0.224",
	"Max.=          0.380",
	"Mean =       0.000", "Median =   -0.002", "Std. dev. =  0.055")
legend (x=0.12, y=8.7, leg.txt)

# Clearly still an outlier problem

ad.test(residuals(reg3))       # reject normality
lillie.test(residuals(reg3))   # reject normality
pearson.test(residuals(reg3))  # reject normality
cvm.test(residuals(reg3))      # reject normality
sf.test(residuals(reg3))       # reject normality
shapiro.test(residuals(reg3))  # reject normality

# In general, the EGLS residuals are slightly more normal, but we
# still must reject the null of normality.

# LET'S ALSO CHECK THE EGLS RESIDUALS FOR CONSTANT VARIANCE.

plot(fitted(reg3), residuals(reg3), xlab="Fitted y", ylab= "Residuals")
abline(h=0)
identify(fitted(reg3), residuals(reg3), row.names(soco.df), cex=0.8)

# Maybe looks a bit better, but we still likey still have a problem
# regarding the null of homoskedasticity.
#
# Again, using the nonconstant variance test we can check for an
# association of residual spread with fitted values:

ncvTest(reg3)      # Seems okay?  That would be nice.
bptest(reg3)       # Same results as reg1; reject homoskedastic errors

# Might also want to check the loglikelihood statistic and the AIC

logLik(reg1)
AIC(reg1)
logLik(reg3)  # EGLS a bit better
AIC(reg3)     # EGLS a bit better

# Other regression approaches are possible.  But at this point let's
# move on.  It could be that our main problem is with the specification
# used in this example.  Clearly there are other variables we should 
# entertain for the right-hand-side.  And then there's the issue of
# troublesome outliers.

# Exclude several Texas Counties and a couple more from the test

shapiro.test(residuals(reg1)[-c(275,774,1048,1066,1114,1130,1135,1143)])
shapiro.test(residuals(reg3)[-c(275,774,1048,1066,1114,1130,1135,1143)])

# Still some problems; Must reject normality, and it's not just because of
# a few outliers.

# Some interesting plots can be obtained from these regression results.
# Among them is the "component-plus-residual" (partial residual) plots {car}
# for the predictors in the two regressions.  Each plot includes the least
# squares line (representing the plane viewed edge-on in the direction of
# the corresponding predictor while controlling for the other covariates)
# and a nonparametric regression smoother (See Fox & Weisberg 2011).

crPlots(reg1)   # OLS model
crPlots(reg3)   # EGLS model; almost identical to the OLS model

# Try a visual comparison:

par(mfrow=c(1,2))
qqnorm(residuals(reg1),ylab="Residuals(reg1)")
qqline(residuals(reg1))
qqnorm(residuals(reg3),ylab="Residuals(reg3)")
qqline(residuals(reg3))

# Not much difference.

# Exclude several Texas Counties and a couple more from the test:

qqnorm(residuals(reg1)[-c(275,774,1048,1066,1114,1130,1135,1143)],
	ylab="Residuals(reg1)")
qqline(residuals(reg1))
qqnorm(residuals(reg3)[-c(275,774,1048,1066,1114,1130,1135,1143)],
	ylab="Residuals(reg3)")
qqline(residuals(reg3))
par(mfrow=c(1,1))

row.names(soco.df)[c(275,774,1048,1066,1114,1130,1135,1143)]

# Some familiar Texas bad boys there.  Yet, it doesn't seem to make
# much difference when they're excluded.  Still some problems in the
# upper tail

##########################################################################
###
### CHECK FOR INDEPENDENCE OF THE RESIDUALS
###
##########################################################################

# Durbin-Watson test for correlated errors is available in package lmtest.
# Tests for autocorrelation, but it's not a spatial test:

dwtest(reg1)
dwtest(reg3)

# Reject independent residuals on the basis of this test for both models.
# But, we should also look at the standard Moran test for residual
# dependence.  Bring in the Q1 weights matrix generated in GeoDa and
# convert it to a listw class object for R to work with:

socogal <- read.gal("socoQueen1.GAL", override.id=TRUE)
socoQ1.gal <- nb2listw(socogal)

# BTW, here are some interesting queries about the weights matrix in R:

names(attributes(socoQ1.gal))  # Attribute names associated with socoQ1.gal
card(socoQ1.gal$neighbours)    # Shows the no. of neighbors for each obs.
range(card(socoQ1.gal$neighbours))  # Range of the no. of neighbors
1/rev(range(card(socoQ1.gal$neighbours))) # Range of the weights

moran.test(reg1$residuals, socoQ1.gal, alternative="two.sided")
moran.test(reg3$residuals, socoQ1.gal, alternative="two.sided")

# Might as well try the Geary autocorrelation statistic as well.  This is
# called up by the geary function in spdep; it returns the value of the
# Geary autocorrelation coefficient (null=1; perfect autocorrelation=0) and
# the sample kurtosis of the 1st argument):

summary(socoQ1.gal)
geary(reg1$residuals, socoQ1.gal, n=length(cpov), n1=length(cpov)-1,
	S0=length(cpov))

# Plenty of spatial autocorrelation still lurking in the residuals.
# What does a map of the residuals look like?


min(reg1$residuals)      # -0.22434
max(reg1$residuals)      #  0.36587

(max(reg1$residuals)-min(reg1$residuals))/7   # target bin width = 0.0843

# Recall from Demo 1, we would like roughly 200 counties per color bin for
# a choropleth quantile map it with 7 color categories 1387/7 = 198.1

res1.sort <- sort(reg1$residuals)
class(res1.sort)   # numeric
res1.sort[1]       # min
res1.sort[1]       # max
res1.sort[198]     # -0.052  Approximate 1st cut point
res1.sort[198*2]   # -0.030  Approximate 2nd cut point
res1.sort[198*3]   # -0.012  Approximate 3nd cut point
res1.sort[198*4]   #  0.006  Approximate 4th cut point
res1.sort[198*5]   #  0.026  Approximate 5th cut point
res1.sort[198*6]   #  0.051  Approximate 6th cut point
res1.sort[1387]    #  0.366  Approximate final cut point

# Choose a palette

colors <- brewer.pal(7, "YlOrBr")  # Stores our colors in object color

color.category.reg <- findInterval(reg1$residuals,
	c(min(reg1$residuals)-.01, res1.sort[198], res1.sort[198*2],
	res1.sort[198*3], res1.sort[198*4], res1.sort[198*5],
	res1.sort[198*6], max(reg1$residuals)+.01),all.inside=TRUE)

def.par <- par(no.readonly = TRUE) # Save default, for resetting...
par(mar=c(3,3,1,3))

plot(soco, axes=FALSE)
title(main="Plot of Residuals(reg1)", line=-1)
plot(soco, col=colors[color.category.reg], border="black", add=TRUE)

labels <- c("-0.224 to -0.052", "-0.052 to -0.030", "-0.030 to -0.012",
	"-0.012 to  0.006", " 0.006 to  0.026", " 0.026 to  0.051",
	" 0.051 to  0.366")
legend(-990000, 500000, legend=labels, fill=colors, cex=0.8,
	y.intersp = 0.9, bty="n")
box()

par(def.par)   # Reset to default

##########################################################################
###
### SPATIAL CORRELOGRAM
###
### A nice plot available in spdep package is the sp.correlogram function.
### But it takes quite a while to do all the calculating.  This function
### calculates the Moran statistic for each lag.  Note:  As coded, the 
### higher-order lags do NOT include the lower-order lags as well.  The
### confidence interval is 95%, calculated as I +/- 2*(sqrt(variance)),
### with the variance calculated under the theoretical randomization
### assumption (default) or normalization assumption when specified
### (randomisation=FALSE).
###
### R packages ncf and pgirmess (and perhaps others) also have functions
### for generating a spatial correlogram
###
##########################################################################

par(mfrow=c(1,1))
spcorr <- sp.correlogram(socogal, sqrt(PPOV), order = 8, method = "I",
	style = "W", randomisation = TRUE)
plot(spcorr)

##########################################################################

###
### CLEARLY WE NEED TO MOVE TO A SPATIAL REGRESSION MODEL
###

##########################################################################
###
### BUT LET'S DO A BIT MORE EXPLORATION OF GLOBAL SPATIAL AUTOCORRELATION
### IN R.  IT'S HARDER IN R THAN IN GEODA, BUT R IS ABLE TO TAKE US WELL
### BEYOND GEODA.
###
##########################################################################

### Recall, we read a first-order queen weights matrix into R that had
### been created in GeoDa.  We called the nb2listW object socoQ1.gal.
### Just as in GeoDa, we can look at the characteristics of this weights
### matrix:

socoQ1.gal
summary(socoQ1.gal)  # A little more information here

# Which are the counties with only 1 neighbor?

row.names(soco.df)[c(1050,1296,1347)]

# El Paso, TX:  Several neighbors but none in South.
# Northampton, VA: Most southern VA county on the Eastern Shore.
# Hancock, WV:  Five neighbors but none in South

# Some general comments about this weights matrix.  We need to be a bit
# cautious.  A weights matrix produced in GeoDa is based on our data set
# with 1,387 observations.  The information regarding counties with one
# neighbor reminds us that our data set does have some artifactual edge
# effects.  Also, recall that we deemed Loving and King counties in Texas
# to be sufficiently troublesome that we, at least momentarily, removed
# them from some of our analysis, above.  For now, they're back in the
# data set because our weights matrix came from GeoDa.  It's n x n, which
# means that any vector we multiply by it must be n x 1.  This seems like
# a show-stopper, but it isn't.  We have two options:  (1) create our own
# weights matrix directly in R (there are lots of options!), and (2) we
# can subset the GeoDa weights matrix in R.

###
### TESTS FOR GLOBAL SPATIAL AUTOCORRELATION
###

# Here, for illustration purposes, we check for autocorrelation in the
# SQRT(PPOV)variable [renamed above to cpov] using the function
# moran.test {spdep} and the weights matrix generated in GeoDa.

moran.test(cpov, socoQ1.gal)   # 0.58258

# The default variance for the Moran statistic using the moran.test function
# is based on the exact variance calculation (Cliff & Ord, 1981) under the
# randomization assumption (the non-free sampling assumption).  The
# function permits the alternative (free sampling) calculation by selecting
# FALSE in the "randomisation" argument.  The (very slight!) difference will
# appear only in the variance of the Moran statistic.  Let's try it:

moran.test(cpov, socoQ1.gal, randomisation=FALSE)

# Note that every thing is the same except the variance calculation
# which is only nominally different.

# There are other Moran tests in R.  But the moran.test function is fine.
# Note that we can also perform a test under a permutation simulation
# exercise -- exactly the kind of test GeoDa gives us.

nsim <- 999
set.seed(5240)
mcsim1 <- moran.mc(cpov, socoQ1.gal, nsim=nsim)

# Note, it's calculated but not printed.  Now ask for the results:

mcsim1

# Let's plot it.  It should look something like the GeoDa histogram
# from the random permutation process:

mean(mcsim1$res[1:nsim])  # somewhat different from the exact expectation
var(mcsim1$res[1:nsim])   # somewhat different from the exact variance
sd(mcsim1$res[1:nsim])
summary(mcsim1$res[1:nsim])
hist(mcsim1$res, nclass=50, xlim=c(-0.1,0.6),
	ylim=c(0,275), col="purple", xlab="Moran's I")
segments(max(mcsim1$res), 1.0, max(mcsim1$res), 250, col="yellow1", lwd=5)

# Note that there are 999 simulation results.  At the end of the results
# the actual observed Moran coefficient is given.  We also see this in
# the following graph.  I darkened the background to bring out the single
# yellow symbol (the observed Moran statistic):

sim <- sort(mcsim1$res)
par(bg="gray80")
plot(sim, pch="+", col="purple")
points(nsim+1, max(mcsim1$res), pch="+", cex=1.5, col="yellow1")
par(bg="white")

##########################################################################
#####
##################### MORAN SCATTERPLOT IN R #############################
#####
##########################################################################
#
# The Moran Scatterplot sometimes gets messed up when there are outliers
# in the data set.  We can eliminate those observations, as we did before
# (but that doesn't work if we want to use the GeoDa-generated weights
# matrix.  I suppose another option would be to simply recode the outliers
# to something else (e.g., by re-setting outlier values equal to the mean of
# the univariate distribution (but this hardly is defensible or recommended
# for serious work).  This isn't a particular problem for our dependent
# variable, cpov, but the issue may come up again.  When it does, we can
# drop the outliers and restructure the weights matrix accordingly.  Or,
# once again, we can create the weights matrix using R.
#
# The Moran Scatterplot in R requires some effort.  If we simply use the
# moran.plot function in spdep we probably won't like what we get:

moran.plot(cpov, socoQ1.gal)

# Not very pretty!  What's going on here?  The unattractive nature of the
# plot results from too much information for a dataset as large as ours.
# The plotting symbol uses a grey diamond shape for observations with strong
# leverage on the regression line.  To make matters worse, they are even
# identified by observation number.  But, most unfamiliar of all, the
# variable (x-axis) and its spatial lag are not standardized.  Let's take
# care of this last issue by standardizing our variables.  So...

mean(cpov)
sd(cpov)
Scpov=(cpov - mean(cpov))/sd(cpov)   # Here's the standardized cpov
hist(Scpov, nclass=50, probability=TRUE)
curve(dnorm(x, mean=0, sd=1), from=-3.1, to=3.1, add=T, col='red', lwd=2)
 
# We now can get the R version of the Moran scatterplot with the
# standardized variable

moran.plot(Scpov, socoQ1.gal, labels=NULL, xlab=NULL, ylab=NULL, pch=1)

# Not very nice, indeed!  To make matters worse, we can even get the
# leverage points identified by county name:

moran.plot(Scpov, socoQ1.gal, labels=as.character(socotxt$NAME),
	xlab=NULL, ylab=NULL, pch=1)

# But this is ridiculous for a data set of this size.  We can turn off
# the labels of influential observations with argument labels=FALSE; can
# suppress the listing of influential observations with quiet=TRUE. 


moran.plot(Scpov, socoQ1.gal, labels=FALSE, spChk=FALSE,
	xlab=NULL, ylab=NULL, quiet=NULL, pch=1)

# This is still rather ugly.  The plotting symbol for influential
# observations is hard-coded into the script for the function
# moran.plot.  What to do????

########################################################################
###
### NOW COMES A BRIEF DIGRESSION TO HIGHLIGHT THE STRENGTH OF
### WORKING IN AN **OPEN SOURCE ENVIRONMENT** LIKE R.

# I simply typed moran.plot in the R Console.  The R console gave me
# the full R script (the code) for the function as found in package
# spdep.  I copied the code and pasted it into a text editor like
# Notepad or Wordpad (not MSWord!).  I found the offending code, and
# changed some other things to make the function output look more like
# the Moran Scatter Plot in GeoDa, and called the slightly revised
# script "my.moran.plot".  I brought this into the R script editor
# (You'll find it at the very bottom of this Demo), ran it, and now(!) I
# can create what I think is a more pleasing, GeoDa-like, Moran
# Scatterplot by just using the function my.moran.plot (and all the
# original arguments in the moran.plot function):
#
########################################################################

my.moran.plot(Scpov, socoQ1.gal, labels=FALSE, spChk = FALSE,
	xlab=NULL, ylab=NULL, quiet=TRUE)

# Check that slope matches our calculated Moran statistic

lagScpov <- lag.listw(socoQ1.gal, Scpov)
scatterslope <- lm(lagScpov ~ Scpov)
names(scatterslope)
Mstat <- scatterslope$coefficients[2]
Mstatr <- round(Mstat, digits=4)
Mstatc <- as.character(Mstatr)
Mstatc

# Add Moran's I to the plot:

title(main = paste("Moran's I =", Mstatc), cex=0.9)

########### SO... THERE IT IS; A RELATIVELY FAMILIAR GRAPH ##########

#####################################################################
#####
##### END OF DISCUSSION REGARDING SPATIAL AUTOREGRESSION.
##### CLEARLY WE NEED TO MOVE TO A SPATIAL REGRESSION MODEL.
##### THIS TOPIC WE TAKE UP IN DEMO 4
#####
######################################################################

######################################################################
#####
##### SHOWN BELOW IS HOW I CREATED THE MODIFIED VERSION OF moran.plot.
##### I COPIED THE ORIGINAL FUNCTION CODE, MODIFIED IT TO SUIT MY
##### PURPOSES AND NAMED IT my.moran.plot
#####
######################################################################

# Steps:
# 1.  Get the code of the function by simply typing the function name at
#     the R prompt.  The full code will appear in the R console.
# 2.  Swipe the full code, copy it, and read it into Notepad (or WordPad).
# 3.  While in Notepad, locate the offending code and change it.
# 4.  Give the revised function a new name like "my.function"
# 5.  Swipe the new code in the Notepad file, copy, and paste into your
#     script file.
# 6.  Run the new function so that R will recognize what to do when you call
#     your my.function.

# I took the R function moran.plot [spdep}, changed it to look a bit more
# like GeoDa's Moran Scatterplot, renamed the function my.moran.plot.
# What follows is the script of the new function.  Must run this before R
# can recognize what to do when the my.moran.plot is called.

############################################################################
###
### Create function my.moran.plot
###
############################################################################

my.moran.plot <- function (x, listw, zero.policy = FALSE, spChk = NULL,
    labels = NULL, xlab = NULL, ylab = NULL, quiet = FALSE, ...) 
{
    if (!inherits(listw, "listw")) 
        stop(paste(deparse(substitute(listw)), "is not a listw object"))
    xname <- deparse(substitute(x))
    if (!is.numeric(x)) 
        stop(paste(xname, "is not a numeric vector"))
    if (any(is.na(x))) 
        stop("NA in X")
    n <- length(listw$neighbours)
    if (n != length(x)) 
        stop("objects of different length")
    if (is.null(spChk)) 
        spChk <- get.spChkOption()
    if (spChk && !chkIDs(x, listw)) 
        stop("Check of data and weights ID integrity failed")
    labs <- TRUE
    if (is.logical(labels) && !labels) 
        labs <- FALSE
    if (is.null(labels) || length(labels) != n) 
        labels <- as.character(attr(listw, "region.id"))
    wx <- lag.listw(listw, x, zero.policy = zero.policy)
    if (is.null(xlab))
        xlab <- xname
    if (is.null(ylab)) 
        ylab <- paste("spatially lagged", xname)
    plot(x, wx, xlab = xlab, ylab = ylab,
	  type="p", col="#AE017E", cex=0.8, pch=18)
    if (zero.policy) {
        n0 <- wx == 0
        symbols(x[n0], wx[n0], inches = FALSE,
            circles = rep(diff(range(x))/50, 
            length(which(n0))),
        fg="#AE017E", bg ="white", add = FALSE)
    }
    xwx.lm <- lm(wx ~ x)
    abline(xwx.lm, col="blue", lwd=1.5)
    abline(h = mean(wx), lty = 1)
    abline(v = mean(x), lty = 1)
    infl.xwx <- influence.measures(xwx.lm)
    is.inf <- which(apply(infl.xwx$is.inf, 1, any))
    points(x[is.inf],
        wx[is.inf], pch=18, cex=0.8, col="#AE017E") # CHANGED THIS
    if (labs) 
        text(x[is.inf], wx[is.inf], labels = labels[is.inf], 
            pos = 2, cex = 1.0)
    rownames(infl.xwx$infmat) <- labels
    if (!quiet) 
        summary(infl.xwx)
    invisible(infl.xwx)
}

#######################################################################

# Housekeeping:

ls()
rm(list=ls())
ls()

#######################################################################
#######################################################################
#######################################################################
###
### END OF DEMO 3
###
#######################################################################
#######################################################################
#######################################################################

