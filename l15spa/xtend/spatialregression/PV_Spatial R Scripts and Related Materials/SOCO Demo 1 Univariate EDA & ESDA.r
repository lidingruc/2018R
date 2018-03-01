##########################################################################
###
### EXPLORATORY DATA ANALYSIS EDA/ESDA
### KENTUCKY DEMO 1
### LAST UPDATED APRIL 27, 2011
### RUN SUCCESSFULLY IN R VERSION 2.13.0 ON 5/4/11
###
###########################################################################
###
### UNIVARIATE EDA/ESDA
### IN THIS DEMO WE:
###     1. Load the necessary packages
###     2. Input the shape file
###     3. Improve the format for county name in the shape file
###     4. Illustrate univariate EDA for selecting dependent variable
###        a. Histograms
###        b. Transformation exploration
###        c. QQ-Plots & more
###        d. Introduce layout function
###     5. Map the transformed dependent variable
###
###########################################################################
#
# Set working directory (note forward slash):

# setwd("C:/Teaching/Shape Files/Southern Counties Shape file")

# Load the necessary packages:

library(classInt)
library(car)
library(graphicsQC)
library(spdep)
library(RColorBrewer)
library(StatDA)

# Learn more about packages and what functionality they provide:

help(package=car)            # General info, including function names
packageDescription("car")    # Similar, but less the function detail

###########################################################################
#
# Note: The car package was recently revised to accompany the 2nd edition
# of John Fox's book, "An R Companion to Applied Regression" (Sage 2011).
# In the process some of the nice functionality from the old package has
# been lost.  In particular the earlier Box-Cox interactive function: Ask
#
############################################################################

######################### READ IN THE SHAPE FILE

# We will bring the shape file (generated in ArcGIS) into R and examine the
# data:

soco <- readShapePoly("south00.shp")
summary(soco)    # Nice 6-number summary of each variable

# It looks fine, except I don't like the format of the county name.  So
# I'm also going to read in a .txt file (created from the .dbf file of the
# shape file).  I do this only because I like the way the county names were
# edited.  So... read the .txt file into R and check out its attributes

######################### READ IN THE TEXT FILE

socotxt <- read.table('south00.txt', header=T)

socotxt[c(1:10),c(1:8)]   # Examine the first 10 rows and first 8 columns
head(socotxt)             # Another way to look at first few records
tail(socotxt)             # Similarly, look at the last few records
names(socotxt)            # Shows variable names
class(socotxt)            # Class = data.frame
str(socotxt)              # Nice summary of the structure of the data frame
summary(socotxt)          # Again, a nice 6-number summary of each variable

# Now, we convert our SpatialPolygonsDataFrame object to a data.frame
# object needed for some of our analysis:

soco.df <- data.frame(soco)

# How's it looking?

par(mfrow=c(1,1))
plot(soco)   # Looking good!

# Now let's add the nice county names from socotxt to soco.df, and rename
# the column "NAME".  It will become apparent why this was done when we
# use the identify function {graphics} in subsequent Demos.

soco.df[,1] <- socotxt[,1]
names(soco.df)[1] <- "NAME"
summary(soco.df)
names(soco.df)

attach(soco.df)  # The wisdom of this is a matter of debate
length(PPOV)     # 1,387 observations

# File seems to be working just fine.

###########################################################################
###
### COMMENCE UNIVARIATE EDA/ESDA.  THIS USUALLY INVOLVES CHECKS FOR
### SYMMETRY (NORMALITY) IN OUR POTENTIAL DEPENDENT VARIABLE AND
### BEGINS WITH SIMPLE HISTOGRAMS & Q-Q PLOTS
###
### See R. Dennis Cook & Sanford Weisberg, "Transforming a Response
### Variable for Linearity." Biometrika (1994) 81(4):731-737.
###
###########################################################################

# Examine PPOV for "correctness" as a dependent variable

hist(PPOV, border=2) # It looks slightly right skewed; border color is red

palette()            # Short list of standard colors

# But these aren't our only color choices in R

colors()

hist(PPOV, col="peachpuff2")

# The function hist {graphicsQC} makes a lot of plotting decisions for us.
# While the plot looks okay, we actually have enormous control over
# almost all aspects of the plot.  Check out ?hist.

# Generally, for a variable with a positive (right) skew, we would look to
# a log transform, logodds transformation, or a square root transform to
# make the variable more "normal", more symmetric.

hist(log(PPOV), col="tomato4")      # Seriously left skewed
hist(logit(PPOV), col="lightblue3") # Slight left skew
hist(sqrt(PPOV), col="peachpuff3")  # Maybe not so bad

# But we probably should request more bins.  While there's a large
# literature on the topic, there's no clear consensus on the
# optimal number of bins.  A common rule of thumb for small data sets
# is that the number of bins for a variable should be no more than
# about 2*sqrt(n) (See Fox, 1997:37). 

2*sqrt(length(PPOV))         # 74.4849

hist(PPOV, nclass=74)        # Slight right skew shows up pretty clearly
hist(log(PPOV), nclass=74)   # Badly left skewed
hist(logit(PPOV), nclass=74) # Slight left skew
hist(sqrt(PPOV), nclass=74)  # Still looks pretty good

# Note: We don't always get exactly the number of bins requested by the
# nclass argument.  R makes some internal decisions regarding cutpoints,
# but it's close.

# A frequently cited option for the number of bins comes from a 1981
# paper by David Freedman & Persi Diaconis.  Their calculation uses
# both the min. & max. of the observations as well as the interquartile
# range (see Fox & Weisberg, 2011:109).   Here I calculate the
# Freedman/Diaconis optimum for the PPOV variable:

FDbins <- (length(PPOV)^(1/3))*(max(PPOV)-min(PPOV))/
	(2*(IQR(PPOV)))
FDbins  # 26.8; much smaller number than 74 from the above rule of thumb

# But it's a pain to calculate this option each time we produce a
# histogram.  Fortunately, this choice is built into the car package
# as an option to the breaks="FD" argument to the function hist.  so...

hist(PPOV, nclass=26.82256)    # Using my calculated value
hist(PPOV, breaks="FD")        # Using the FD choice for breaks argument
hist(log(PPOV), breaks="FD")   # For log transform
hist(logit(PPOV), breaks="FD") # For logit transform
hist(sqrt(PPOV), breaks="FD")  # For sqrt transform

# Yet one more convention (for choosing the *width* of bins) is attributed
# to David W. Scott, "On Optimal and Data-Based Histograms," Biometrika
# 66(3)(Dec. 1979):605-610:  W = 3.49*sigma*n^(-1/3).  It also is a
# breaks= selection option in the function hist.

Wbins <- 3.49*sd(PPOV)*(length(PPOV))^(-1/3)
Wbins  # Suggests a bin width of approximately 0.029 for PPOV

hist(PPOV, breaks="Scott")        # R seems to have chosen 0.05
hist(log(PPOV), breaks="Scott")   # For log transform
hist(logit(PPOV), breaks="Scott") # For logit transform
hist(sqrt(PPOV), breaks="Scott")  # For sqrt transform

# I still like the sqrt transformation just based on an examination of the
# histograms.  We also can set the endpoints of each bin in a histogram using
# the breaks argument; the bins need not be of equal width, although
# histograms with unequal width bins are difficult to interpret.  Here I
# use this feature with sqrt(PPOV) -- producing around 70 bins located
# between 0.1 & 0.8).  I then overlay on the histogram a normal density
# curve (using the lines function) with the same mean & sd as the variable
# plotted in the histogram:

############# SO... HISTOGRAM OF SQRT(PPOV) WITH NORMAL DENSITY:

# Note: color="lightskyblue1" yields a nice Tar Heel blue
# Also have a nice Kentucky royal blue with color="blue" (despite
# several royal blue options [1-4] -- which aren't all that great)

mean(sqrt(PPOV))
sd(sqrt(PPOV))
median(sqrt(PPOV))
min(sqrt(PPOV))
max(sqrt(PPOV))
summary(sqrt(PPOV))  # Provides everything except s.d.

hist(sqrt(PPOV), breaks=seq(0.10, 0.80, 0.01), col='blue',
	probability=T, ylab='Density',
	main='Histogram of Transformed Child
	Poverty Rate (sqrt(PPOV))',
	xlab='sqrt(PPOV)')
box()

x <- seq(0.10,0.80,0.01)
y <- dnorm(x, mean=0.4640952, sd=0.09657176, log=FALSE)
lines(x,y, lty='solid', col='red', lwd=2)
leg.txt <- c("   sqrt(PPOV)", "Min. =          0.1687",
	"Max.=          0.7716", "Median =    0.4620",
	"Mean =       0.4641", "Std. dev. =  0.0966")
legend (x=0.07, y=4.7, bty="n", leg.txt)

### AGAIN, HISTOGRAM OF SQRT(PPOV) BUT THIS TIME WITH ALTERNATIVE
### CODING FOR NORMAL DENSITY USING THE curve FUNCTION

me <- mean(sqrt(PPOV))
sd <- sd(sqrt(PPOV))
hist(sqrt(PPOV), breaks=seq(0.10, 0.80, 0.01), col='papayawhip',
	probability=T,
	ylab='Density', main='Histogram of Square Root of PPOV',
	xlab='sqrt(PPOV)')
box()
curve(dnorm(x, mean=me, sd=sd), from=0.1, to=0.8, add=T, col='red', lwd=2)
leg.txt <- c("   sqrt(PPOV)", "Min. =          0.1687",
	"Max.=          0.7716", "Median =    0.4620",
	"Mean =       0.4641", "Std. dev. =  0.0966")
legend (x=0.07, y=4.7, bty="n", leg.txt)

### In some earlier work with all counties in US, we used a logit
### transformation for the child poverty variable.  This transformation is
### presented as a variable in the soco data set as L0_POV.  Let's check
### whether it's a better transformation here for the southern counties
### than is the square root transformation:

minlogodds <- min(logit(PPOV))
maxlogodds <- max(logit(PPOV))
melogodds <- mean(logit(PPOV))
sdlogodds <- sd(logit(PPOV))
medlogodds <- median(logit(PPOV))
minlogodds
maxlogodds
melogodds
sdlogodds
medlogodds

hist(logit(PPOV), breaks=seq(-4, 0.5, 0.1), col=8, probability=T,
	ylab='Density', main='Histogram of Logodds of PPOV',
	xlab='logit(PPOV)')
box()
curve(dnorm(x, mean=melogodds, sd=sdlogodds), from=-3.8, to=0.5,
	add=T, col='red', lwd=2)
	leg.txt <- c("LOGODDS(PPOV)", "Min. =           -3.530",
	"Max.=            0.386",
	"Mean =        -1.320", "Median =     -1.304", "Std. dev. =    0.558")
legend (x=-3.9, y=0.8, leg.txt)

# Despite similarity of mean and median, the logodds transformed PPOV
# seems a bit heavy in the left tail.  Can further check for normality
# using a QQ plot.

# Let's plot them with the 95% confidence envelope.  First, plot the
# 95% confidence envelope for the qqPlot.  Here it is for
# the untransformed PPOV variable using qqPlot {car}:

qqPlot(PPOV, distribution="norm",
	xlab='', main='Quantile Comparison Plot PPOV',
	envelope=.95, labels=FALSE, las=0, pch=NA, lwd=2, col="red",
	line="quartiles")

# Now add the QQ-Plot in black

par(new=TRUE)
qqPlot(PPOV, distribution="norm", envelope=FALSE, labels=FALSE,
	pch=1, cex=1, col="black")
par(new=FALSE)

# This pretty much confirms that if we want a useful univariate
# distribution involving PPOV we need to think about transformations.
# Here it is for the square root transformation:

qqPlot(sqrt(PPOV), distribution="norm",
	xlab='', main='Quantile Comparison Plot sqrt(PPOV)',
	envelope=.95, labels=FALSE, las=0, pch=NA, lwd=2, col="red",
	line="quartiles")
par(new=TRUE)
qqPlot(sqrt(PPOV), distribution="norm", envelope=FALSE, labels=FALSE,
	pch=1, cex=1, col="black")
par(new=FALSE)

# Perhaps a bit bumpy in the left tail, but not too bad.
# Now let's compare the closest competitor, logit(PPOV):

qqPlot(logit(PPOV), distribution="norm",
	xlab='', main='Quantile Comparison Plot logit(PPOV)',
	envelope=.95, labels=FALSE, las=0, pch=NA, lwd=2, col="red",
	line="quartiles")
par(new=TRUE)
qqPlot(logit(PPOV), distribution="norm", envelope=FALSE, labels=FALSE,
	pch=1, cex=1, col="black")
par(new=FALSE)

### As anticipated from the histogram, the logit transformed variable is
### quite heavy in the left tail.  The sqrt(PPOV) transformation 
### certainly looks a bit better.

### One more check:  The latest version of the car package has a nice option
### for checking symmetry using the classic Box-Cox transformation.  It's
### contained in the function symbox, and replaces the function Ask which
### was found in the earlier version of the package car (now depreciated).
### Here's how it works.  We observed that PPOV has a univariate distribution
### with a positive skew.  Positive skew generally is corrected in the Box-Cox
### test by moving the variable "down the ladder" of powers & roots.
### 1 means no transformationqrt transformation
### 0.5 is the square root transformation
### 0 is log transformation
### Function symbox {car} helps us make a choice by displaying side-by-side
### boxplots.  Let's run it to confirm what we already know (that the
### square root transformation is serving us pretty well).

symbox(PPOV, powers=c(1, .75, .5, .25, 0))
title(main="Box-Cox Transformation to Symmetry\n(moving 'down the ladder')")

### Again, square root transform looks about as good as we're going to get.
### We can do this another way by creating our own side-by-side boxplots.
### Not surprisingly, we again see the superiority of the square root
### transformation.

par(mfrow=c(4,1))
boxplot(PPOV, xlab='PPOV', horizontal=TRUE, cex=0.9, width=2, outcol="red",
	cex.lab=1.8)
title("\nComparing Various Transformations of PPOV\n\n", cex.main=2.2)
boxplot(log(PPOV), xlab='LOG PPOV', horizontal=TRUE, cex=0.9, width=2,
	outcol="red", cex.lab=1.8)
boxplot(sqrt(PPOV), xlab='SQRT PPOV', horizontal=TRUE, cex=0.9, width=2,
	outcol="red", cex.lab=1.8)
boxplot(logit(PPOV), xlab='LOGIT PPOV', horizontal=TRUE, cex=0.9, width=2,
	outcol="red", cex.lab=1.8)
par(mfrow=c(1,1))

### This can also be done with the layout function {graphics}:

def.par <- par(no.readonly = TRUE) # Save default, for resetting...

# Divide the device into four rows & one column, and allocate
# boxplots to the four plot areas:

layout(matrix(c(1,2,3,4), 4, 1, byrow = TRUE))

# Show the regions that have been allocated to each plot:

layout.show(4)

boxplot(PPOV, xlab='PPOV', horizontal=TRUE, cex=0.9, width=2, outcol="red",
	cex.lab=1.8)
title("\nComparing Various Transformations of PPOV\n\n", cex.main=2.2)
boxplot(log(PPOV), xlab='LOG PPOV', horizontal=TRUE, cex=0.9, width=2,
	outcol="red", cex.lab=1.8)
boxplot(sqrt(PPOV), xlab='SQRT PPOV', horizontal=TRUE, cex=0.9, width=2,
	outcol="red", cex.lab=1.8)
boxplot(logit(PPOV), xlab='LOGIT PPOV', horizontal=TRUE, cex=0.9, width=2,
	outcol="red", cex.lab=1.8)
par(def.par)   # Reset to default

### The two methods yield the same results visually.  The real
### advantage of the layout function is revealed when we are trying
### to do something fancy -- like creating a plot with several different
### features (e.g., boxplot, histogram and QQ-plot) each with a prescribed
### plot area allocation.  This is demonstrated here:

### First, set outer margins and margins of plot region.
### Divide the device into two rows & two columns, and allocate less
### space in the upper area to accommodate a boxplot, with a histogram
### and qqplot below it:

par(oma=c(1,0,1,0))
par(mar=c(2,2,6,2))
MyLayOut <- matrix(c(1,1,2,3), 2, 2, byrow = TRUE)
MyLayOut
nf <- layout(mat = MyLayOut, widths=c(2,1), heights = c(3,7), respect = FALSE)

# Show the regions that have been allocated to each plot:

layout.show(nf)

# Commence the plotting.  First the boxplot:

boxplot(sqrt(PPOV), main="Boxplot sqrt(PPOV)", cex.main=1.1,
	horizontal=TRUE, width=2, outcol="red")

title(main="Various Views, Variable sqrt(PPOV)", cex.main=1.5, line=5)

# Now the histogram:

hist(sqrt(PPOV), breaks=seq(0.10, 0.80, 0.01), col="lightskyblue1",
	probability=T, ylab='Density',
	main="Histogram sqrt(PPOV)\nwith normal density", cex.main=1.1,
	xlab='sqrt(PPOV)')
box()
par(lwd=1.8)
curve(dnorm(x, mean=me, sd=sd), from=0.1, to=0.8, add=T, col='red', lwd=2)
par(lwd=1.0)

# Finally the QQ-Plot:

qqPlot(sqrt(PPOV), distribution="norm",
	xlab='', ylab='', main="Normal QQ-Plot",
	envelope=.95, labels=FALSE, las=0, pch=NA, lwd=0.5, col="red",
	line="quartiles")
par(new=TRUE)
qqPlot(sqrt(PPOV), distribution="norm", envelope=FALSE, labels=FALSE,
	pch=1, cex=0.7, col="black")

par(def.par)   # Reset to default

### Another comparison might be to simply plot the sorted data against an
### index; the advantage of this is that we see the distribution and
### possible outliers.  Let's standardize the vars so we can plot both
### on a single graph.

layout(matrix(1,1), 1, 1)
sLOGITPPOV <- (logit(PPOV)-mean(logit(PPOV)))/sd(logit(PPOV))
sSQRTPPOV <- (sqrt(PPOV)-mean(sqrt(PPOV)))/sd(sqrt(PPOV))
summary(sLOGITPPOV)
summary(sSQRTPPOV)
plot(sort(sLOGITPPOV), pch="-", col="black",
	main="sqrt(PPOV) vs. logit(PPOV)
	sqrt: red; logit: black")
lines(sort(sSQRTPPOV), lwd=3, col="red")

## OKAY; ENOUGH. LET'S AGREE TO GO WITH sqrt(PPOV) AS OUR DEPENDENT
## VARIABLE

# Can make one final jazzed up histogram of our new dependent variable.
# We're combining the histogram, a kernel density smoother, the normal
# density, and adding a so-called "rug plot" as well; whoop-de-do!

hist(sqrt(PPOV), nclass=74, probability=T, xlim=c(0.1, 0.81),
	ylim=c(0, 4.9), ylab='Density', col="wheat")

# Add the kernel density non-parametric smoother:

lines(density(sqrt(PPOV)), lwd=2, col='blue')

# Add a rug:

points(sqrt(PPOV), rep(0, length(sqrt(PPOV))), pch="|")

# And now the normal density is added:

curve(dnorm(x, mean=me, sd=sd), from=0.1, to=0.81, add=T, col='red', lwd=3)

# Finally give it some class with a box outline and add the legend box:

box()
leg.txt <- c("   sqrt(PPOV)", "Min. =         0.1687",
	"Max.=         0.7716", "Median =    0.4620",
	"Mean =       0.4641", "Std. dev. =  0.0966")
legend (x=0.05, y=5.1, bty="n", cex=1.1, leg.txt)

###########################################################################
###
### BUT CAN WE MAP IT?  LET'S GIVE IT A TRY!
### We'll need package RColorBrewer
###
###########################################################################

min(sqrt(PPOV))      # 0.168695
max(sqrt(PPOV))      # 0.771556

# Let's create a choropleth quantile map it with 7 color categories
# 1387/7 = 198.1.  So, roughly 200 counties per color bin

sort(sqrt(PPOV))

# Examining the sorted values suggests something like the following:
# 0.16 to 0.37, 0.37 to 0.41, 0.41 to 0.45, 0.45 to 0.48, 0.48 to 0.52,
# 0.52 to 0.57, 0.57 to 0.78
#
# Let's choose a palette

brewer.pal(7, "YlOrBr")
display.brewer.pal(7, "YlOrBr")    # That should work
palette(brewer.pal(7, "YlOrBr"))   # Sets the pallette we want
colors <- brewer.pal(7, "YlOrBr")  # Stores our colors in object color

color.category.POV <- findInterval(sqrt(PPOV),
	c(0.16, 0.37, 0.41, 0.45, 0.48, 0.52, 0.57, 0.78), all.inside=TRUE)

# Set the margins to fit everything in the plot window.  This requires
# a bit of experimentation

def.par <- par(no.readonly = TRUE) # Save default, for resetting...
par(mar=c(3,3,1,3))

plot(soco, axes=FALSE)
title(main="Plot of sqrt(PPOV)", line=-1)
plot(soco, col=colors[color.category.POV], border="black", add=TRUE)
labels <- c("0.16 to 0.37", "0.37 to 0.41", "0.41 to 0.45", "0.45 to 0.48",
	"0.48 to 0.52", "0.52 to 0.57", "0.57 to 0.78")
legend(-900000, 700000, legend=labels, fill=colors, cex=1.0,
	y.intersp = 0.9, bty="n")
box()

# Add a simple north arrow.  Need function Northarrow from package StatDA

Northarrow(1480000, 400000, 1480000, 600000, 1480000, 700000,
	Alength=0.15, Aangle=015, Alwd=2.0, Tcex=1.4)

par(def.par)   # Reset to default

########################################################################
###                                                                  ###
### SO... THERE IT IS.  EVEN A NICE MAP!                             ###
###                                                                  ###
### END OF DEMO FOR UNIVARIATE EDA/ESDA                              ###
###                                                                  ###
########################################################################

# Here's a bit of housecleaning which is very helpful when you wish to
# end one R session but immediately continue on to another session.
# It's a way to remove all the detritis from the present session:

ls()
rm(list=ls())
ls()

#########################################################################
#########################################################################
