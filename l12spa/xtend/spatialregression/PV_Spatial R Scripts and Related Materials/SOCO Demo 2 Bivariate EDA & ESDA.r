##########################################################################
###
### EXPLORATORY DATA ANALYSIS
### KENTUCKY DEMO 2
### LAST UPDATED APRIL 27, 2011
### RUN SUCCESSFULLY IN R VERSION 2.13.0 ON 5/4/11
###
###########################################################################
###
### BIVARIATE EDA/ESDA
### IN THIS DEMO WE:
###     1. Remind ourselves of the outcome of Demo 1
###     2. Begin selecting independent variables (PUNEM, PFHH, & PHSPLUS)
###     3. Introduce scatterplots in R
###     4. Transformation exploration
###     5. Use of interactive identify function
###     6. Illustrate scatterplot matrix
###
###########################################################################
#
# Set working directory:

# setwd("C:/Teaching/Shape Files/Southern Counties Shape file")

# Load the necessary packages:

library(classInt)
library(car)
library(graphicsQC)
library(spdep)
library(RColorBrewer)
library(StatDA)

############################################################################
#
# READ IN THE SHAPE FILE

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

#########################################################################
###
### We will use the square root transformation of PPOV as arrived at in
### Demo 1.
### To get started, attach soco.def as the default data set.
### Then plot the transformed dependent variable.
### Finally, plot the map of the dependent variable.
###
#########################################################################

attach(soco.df)

me <- mean(sqrt(PPOV))
sd <- sd(sqrt(PPOV))
hist(sqrt(PPOV), nclass=74, probability=T, xlim=c(0.1, 0.81),
	ylim=c(0, 4.9), ylab='Density', col="wheat")
lines(density(sqrt(PPOV)), lwd=2, col='blue')
points(sqrt(PPOV), rep(0, length(sqrt(PPOV))), pch="|")
curve(dnorm(x, mean=me, sd=sd), from=0.1, to=0.81, add=T, col='red', lwd=3)
box()
leg.txt <- c("   sqrt(PPOV)", "Min. =        0.1687",
	"Max.=         0.7716", "Median =    0.4620",
	"Mean =       0.4641", "Std. dev. =  0.0966")
legend (x=0.05, y=5.1, bty="n", cex=1.1, leg.txt)

### R has many interesting packages.  Many packages do similar things.
### Here's a similar histogram with added features from package StatDA:

edaplot(sqrt(PPOV), scatter=TRUE, H.freq=FALSE, box=TRUE, H.breaks=74,
	H.col="wheat", H.border=TRUE, H.labels=FALSE,
	S.pch=1, S.col="blue", S.cex=0.5,
	D.lwd=2, D.lty=1, D.plot=FALSE,
	P.xlim=c(0.1, 0.81), P.cex.lab =1.2,
	P.log=FALSE, P.main="Histogram, Density Plot, Scatterplot,
	and Boxplot of sqrt(PPOV)",
	P.xlab="sqrt(PPOV)", P.plot=TRUE,
	P.ylab="Density",
	B.pch=1,B.cex=0.5, B.col="red")
lines(density(sqrt(PPOV)), lwd=2, col='blue')
curve(dnorm(x, mean=me, sd=sd), from=0.1, to=0.83, add=T, col='red', lwd=3)
leg.txt <- c("   sqrt(PPOV)", "Min. =        0.1687",
	"Max.=         0.7716", "Median =    0.4620",
	"Mean =       0.4641", "Std. dev. =  0.0966")
legend (x=0.05, y=4.7, bty="n", cex=1.1, leg.txt)

############# AGAIN, MAP THE DEPENDENT VARIABLE:

def.par <- par(no.readonly = TRUE) # Save default, for resetting...
par(mar=c(3,3,1,3))

colors <- brewer.pal(7, "YlOrBr")
color.category.POV <- findInterval(sqrt(PPOV),
	c(0.16, 0.37, 0.41, 0.45, 0.48, 0.52, 0.57, 0.78), all.inside=TRUE)
plot(soco, axes=FALSE)
title(main="Plot of sqrt(PPOV)", line=-1)
plot(soco, col=colors[color.category.POV], border="black", add=TRUE)
labels <- c("0.16 to 0.37", "0.37 to 0.41", "0.41 to 0.45", "0.45 to 0.48",
	"0.48 to 0.52", "0.52 to 0.57", "0.57 to 0.78")
legend(-900000, 700000, legend=labels, fill=colors, cex=1.0, bty="n")
box()

Northarrow(1480000, 400000, 1480000, 600000, 1480000, 700000,
	Alength=0.15, Aangle=015, Alwd=2.0, Tcex=1.4)

par(def.par)   # Reset to default

#########################################################################
###
### HAVING ARGEED THAT THE SQUARE ROOT TRANSFORMATION OF PPOV IS A DECENT
### DEPENDENT VARIABLE, LET'S COMMENCE BIIVARIATE EDA/ESDA.  THIS USUALLY
### INVOLVES BIVARIATE SCATTERPLOTS, FURTHER TRANSFORMATIONS, AND
### CAUTIOUS EXAMINATION OF POSSIBLE OUTLIERS
###
###########################################################################

### Let's look at some potential independent variables:  PUNEM, PFHH, &
### PHSPLUS (or, if necessary, a proper transformation of each)

#### First: PUNEM (percent of labor force unemployed)

plot(PUNEM, sqrt(PPOV), xlim=c(-0.05,0.24), ylim=c(0,0.9),
	main="sqrt(PPOV) by PUNEM")

# Maybe a hint of curvilinearity?  We'll need to check on that.
# Also check for zero values in the event we would prefer a log transform:

row.names(soco.df) <- soco.df$NAME
which(PUNEM == 0)     # Two observations have zero values.  Careful of logs!
row.names(soco.df)[c(1114,1130)]  # King & Loving counties in Texas

# Checks for linearity:

plot(PUNEM, sqrt(PPOV), main="sqrt(PPOV) by PUNEM")
abline(lm(sqrt(PPOV) ~ PUNEM), lty=2, lwd=2, col=2)
lines(lowess(sqrt(PPOV) ~ PUNEM), col=4, lwd=2)

# Another way to do this is interactively.  This is a highly useful
# R function provided in the graphics package.  Just use your cursor,
# click on a point in the scatterplot to identify it.  Continue
# until you're ready to move on.  At this point, right click on your mouse
# and choose "stop" option.  Useful hint:  when you're ready to identify a
# point, place your cursor where you would like the identifying text to
# appear (e.g., bottom, top, left, etc.)

identify(PUNEM, sqrt(PPOV), row.names(soco.df), cex=0.8)

# What do we see?  Certainly it's in the hypothesized direction: as
# unemployment goes up poverty goes up.
# Nonlinearity?  Probably.  (The lowess function -- for "locally weighted
# regression" -- is very useful for this.)  So, what to do?  There are
# some zeros, so let's avoid a log transformation for now.
# Let's see what a square root transformation buys us:

plot(sqrt(PUNEM), sqrt(PPOV))

# Linearity seems to be improved; but those outliers!
# Put 'em next to one another for comparison:

par(mfrow=c(1,2))
plot(PUNEM, sqrt(PPOV))
abline(lm(sqrt(PPOV) ~ PUNEM), lty=2, lwd=2, col=2)
lines(lowess(sqrt(PPOV) ~ PUNEM), col=4)
plot(sqrt(PUNEM), sqrt(PPOV))
abline(lm(sqrt(PPOV) ~ sqrt(PUNEM)), lty=2, lwd=2, col=2)
lines(lowess(sqrt(PPOV) ~ sqrt(PUNEM)), col=4)
par(mfrow=c(1,1))

# Square root transformation of PUNEM improves things, but it really
# highlights those two outliers.  Let's drop them for now and try both a
# square root and log transform.  The set up looks something like this:

which(PUNEM == 0)
row.names(soco.df)[c(1114,1130)]
socoexcl <- soco.df[-c(1114,1130),]
detach(soco.df)
attach(socoexcl)
length(PPOV)       # 1385 looks right
which(PUNEM == 0)  # also looks right

# Now let's go head-to-head between the sqrt & log transformations
# First the square root transformation of PUNEM:
#
par(mfrow=c(1,2))
plot(sqrt(PUNEM), sqrt(PPOV))
abline(lm(sqrt(PPOV) ~ sqrt(PUNEM)), lty=2, lwd=2, col=2)
lines(lowess(sqrt(PPOV) ~ sqrt(PUNEM)), col=4)
#
# And now the log transformation:
#
plot(log(PUNEM), sqrt(PPOV))
abline(lm(sqrt(PPOV) ~ log(PUNEM)), lty=2, lwd=2, col=2)
lines(lowess(sqrt(PPOV) ~ log(PUNEM)), col=4)
par(mfrow=c(1,1))

# Just looking at them, I'm inclined to prefer the sqrt transformation,
# but it's a tough call.  Let's check the R-square term in a simple
# linear regression set up:

regsqrt <- lm(sqrt(PPOV) ~ sqrt(PUNEM))
reglog <- lm(sqrt(PPOV) ~ log(PUNEM))
summary(regsqrt)
summary(reglog)

# sqrt appears to win in a photo finish?  Are the estimated paramter
# t-values relevant? Let's go with the square root transformation, but
# let's also stay with the data set excluding King and Loving counties
# for now.  Here's where R makes life considerably easier than GeoDa.

############################################################################
#####
##### Now we should do the same sort of test with a couple other potential
##### independent variables.  I did this for variables PFHH & PHSLS.  The
##### details for what I did are shown below at the end of this tutorial.
##### But you should try this on your own to see if you agree.
##### When the dust settled, my choice for independent variables are:
##### sqrt(PUNEM), sqrt(PFHH) and log(PHSPLUS). Let's stay with these.
#####
############################################################################
#####
##### A SCATTERPLOT MATRIX USING THE pairs FUNCTION IS A GOOD WAY TO
##### SEE HOW THINGS LOOK AFTER THE TRANSFORMATIONS:
#####
############################################################################

# Rename the variables so that the names will be short enough to fit the
# plot windows

SqrtCPov <- sqrt(PPOV)
SqrtPUnemp <- sqrt(PUNEM)
SqrtPFemHH <- sqrt(PFHH)
LogPHiEd <- log(PHSPLUS)
pairs(cbind(SqrtCPov, SqrtPUnemp, SqrtPFemHH, LogPHiEd), cex.labels=1.6,
	panel=function(x,y){
		points(x,y, cex=0.3)
		abline(lm(y~x), lty=2, lwd=2, col=2)
		lines(lowess(x,y), lwd=2, col=4)
		},
	diag.panel=function(x){
		par(new=T)
		hist(x, main="", axes=F, col=7, nclass=20)
		}
	)

##########################################################################
####                                                                  ####
#### END OF BIVARIATE EDA/ESDA LOOKING AT PUNEM                       ####
####                                                                  ####
#### BUT HERE'S HOW I TRANSFORMED PFHH and PHSPLUS:                   ####
##########################################################################

detach(socoexcl)
attach(soco.df)

summary(PFHH)    # No zero values; slight right skew
summary(PHSPLUS) # No zero values; slight right skew

# First, let's see where we might want to go with PFHH:

par(mfrow=c(1,2))
plot(sqrt(PFHH), sqrt(PPOV))
abline(lm(sqrt(PPOV) ~ sqrt(PFHH)), lty=2, lwd=2, col=2)
lines(lowess(sqrt(PPOV) ~ sqrt(PFHH)), col=4)

# And now the log transformation:

plot(log(PFHH), sqrt(PPOV))
abline(lm(sqrt(PPOV) ~ log(PFHH)), lty=2, lwd=2, col=2)
lines(lowess(sqrt(PPOV) ~ log(PFHH)), col=4)
par(mfrow=c(1,1))

# There's probably less linearity in the log(PFHH) plot.
# But also some troublesome outliers that ought to be pursued:

which(log(PFHH) < -2.8)
row.names(socoexcl) <- socoexcl$NAME
row.names(socoexcl)[c(1066,1114)]

# Two Texas counties, but not Loving & King.  Wikipedia Time:
# Glasscock Co., TX, not far from Loving Co., (at least in Texas terms).
# 2000 pop. = 1,406.  Kinney Co. in south TX along Rio Grande. 2000 pop. =
# 3,379.  Let's check the R-square term in a simple linear regression:

reg2sqrt <- lm(sqrt(PPOV) ~ sqrt(PFHH))
reg2log <- lm(sqrt(PPOV) ~ log(PFHH))
summary(reg2sqrt)
summary(reg2log)

# Again, not much to write home about.  It's a tough call.  If this were
# my dissertation, I'd probably drop Glasscock & Kinney from the analysis.
# Since it's not, I'm going to leave the outliers in for now, and go
# with the sqrt(PFHH) transformation.

# And now let's check PHSPLUS:
# First, the square root transformation:

par(mfrow=c(1,2))
plot(sqrt(PHSPLUS), sqrt(PPOV))
abline(lm(sqrt(PPOV) ~ sqrt(PHSPLUS)), lty=2, lwd=2, col=2)
lines(lowess(sqrt(PPOV) ~ sqrt(PHSPLUS)), col=4)

# And now the log transformation:

plot(log(PHSPLUS), sqrt(PPOV))
abline(lm(sqrt(PPOV) ~ log(PHSPLUS)), lty=2, lwd=2, col=2)
lines(lowess(sqrt(PPOV) ~ log(PHSPLUS)), col=4)
par(mfrow=c(1,1))

# My first impression is to prefer the log transformation

which(log(PHSPLUS) == 0)  # No zero values, so we're okay with log

# Let's check the R-square term in a simple linear regression set up:

reg3sqrt <- lm(sqrt(PPOV) ~ sqrt(PHSPLUS))
reg3log <- lm(sqrt(PPOV) ~ log(PHSPLUS))
summary(reg3sqrt)
summary(reg3log)

# Okay, not great, but I'm going with the log(PHSPLUS) transformation

########################################################################
###                                                                  ###
### END OF DISCUSSION REGARDING TRANSFORMATION FOR PFHH and PHSPLUS  ###
###                                                                  ###
########################################################################

# Housekeeping:

ls()
rm(list=ls())
ls()

########################################################################
########################################################################
###                                                                  ###
### END DEMO FOR BIVARIATE EDA/ESDA                                  ###
###                                                                  ###
########################################################################
########################################################################

