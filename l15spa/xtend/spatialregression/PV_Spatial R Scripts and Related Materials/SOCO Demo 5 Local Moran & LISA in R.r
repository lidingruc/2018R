##########################################################################
###
### LISA STATISTICS & MAPS IN R
### KENTUCKY DEMO 5
### LAST UPDATED APRIL 30, 2011
### RUN SUCCESSFULLY IN R VERSION 2.13.0 ON 5/4/11
###
###########################################################################
###
### LISA STATISTICS IN R
### IN THIS DEMO WE:
###     1. Read in the shape file and text file data
###     2. Create the local Moran statistic
###     3. Create a LISA map
###
###########################################################################
#
# Set working directory:

# setwd("C:/Teaching/Shape Files/Southern Counties Shape file")

# Load the necessary packages:

library(classInt)
library(car)
library(graphicsQC)
library(lmtest)
library(spdep)
library(sp)
library(spatstat)
library(RColorBrewer)

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

### Attach soco.df as the default data set.

attach(soco.df)
class(soco.df)
names(soco.df)

# Create some new variable names so that R doesn't have to do
# the transformation afresh every time a variable is called upon:

cpov <- sqrt(PPOV)
femhh <- sqrt(PFHH)
unem <- sqrt(PUNEM)
hied <- log(1-PHSLS)

####### READ IN A WEIGHTS MATRIX CREATED IN GEODA AS IN DEMOS 3 & 4:

socogal <- read.gal("socoQueen1.GAL", override.id=TRUE)
socoQ1.gal <- nb2listw(socogal)

######### CREATING THE LOCAL MORAN STATISTIC IN R

# Create local Moran for variable cpov.
# Use the 1st order queen weights matrix socoQueen1.GAL.

locm <- localmoran(cpov, socoQ1.gal, alternative="two.sided")
summary(locm)

# We can export the table (actually a matrix object) for
# mapping outside of R if desired:
#
# write.table(locm, "socolocm.txt")

# If we want to see all 1,387 local moran statistics (NOT ADVISED!),
# just type the object name. >locm.  Better to be satisfied with the 
# command >summary(locm) as given above.

######################################################################
#####
##### CREATING A LISA MAP IN R
#####
######################################################################

# We can plot the local Moran (LISA) values but it's a bit complicated
# to mask out those that are not significant, and perhaps even harder
# to get the colors to match the GeoDa LISA map. But here we go anyway. 
# For the LISA output, which is a map, we need to have previously read
# the shape file into R.

# Create the logical variables in the data frame rather than as
# individual objects.  First, standardize the variables:

Scpov <- (cpov - mean(cpov))/sd(cpov)
lagScpov <- lag.listw(socoQ1.gal, Scpov)
summary(Scpov)
summary(lagScpov)
plot(Scpov, lagScpov, main="Moran Scatterplot cpov")
abline(h=0, v=0)
abline(lm(lagScpov ~ Scpov), lty=2, lwd=2, col="blue")

# This should look like a Moran scatterplot from Demo 3, here without
# the fancy colors, etc.  Since this was generated from a simple plot
# call, we can even use the identify function from the car package

row.names(soco.df) <- soco.df$NAME
identify(Scpov, lagScpov, row.names(soco.df), cex=0.8)

# Here's some easily retrieved information on a specific observation:

which(soco.df$NAME=="DC")  # record number 146
Scpov[146]     # Standardized sqrt(CPOV) for Washington DC
lagScpov[146]  # Average value of Standardized(sqrt(CPOV)) for DC's neighbors
locm[146,1]    # Local Moran I for DC
locm[146,4]    # z-value for significance
locm[146,5]    # p-value

soco.df$hh <- (Scpov>= 0 & lagScpov>= 0) & (locm[,5]<= 0.05)
soco.df$ll <- (Scpov<= 0 & lagScpov<= 0) & (locm[,5]<= 0.05)
soco.df$hl <- (Scpov>= 0 & lagScpov<= 0) & (locm[,5]<= 0.05)
soco.df$lh <- (Scpov<= 0 & lagScpov>= 0) & (locm[,5]<= 0.05)
soco.df$ns <- locm[,5]> 0.05

which(soco.df$hh==TRUE)
which(soco.df$ll==TRUE)
which(soco.df$hl==TRUE)
which(soco.df$lh==TRUE)
which(soco.df$ns==TRUE)

# Create a single categorial variable summing up the
# five logical variables

soco.df$var <- 0
soco.df$var
soco.df$var <- replace(soco.df$var, (soco.df$hh==TRUE), 1)
soco.df$var
soco.df$var <- replace(soco.df$var, (soco.df$ll==TRUE), 2)
soco.df$var
soco.df$var <- replace(soco.df$var, (soco.df$hl==TRUE), 3)
soco.df$var
soco.df$var <- replace(soco.df$var, (soco.df$lh==TRUE), 4)
soco.df$var
soco.df$var <- replace(soco.df$var, (soco.df$ns==TRUE), 5)
soco.df$var

# Set the breaks for the thematic map classes

breaks <-seq(1,5,1)

# Set the corresponding labels for the thematic map classes

labels <- c("high-High", "low-Low", "High-Low", "Low-High", "Not Signif.")

# Determine which map class each observation falls into based on
# the value of soco.df$var
# sort(breaks, decreasing = FALSE)

np <- findInterval(soco.df$var, breaks, all.inside=FALSE)

# Assign colors to each map class

colors <- c("red", "blue", "lightpink", "skyblue2", "white")

# plot the SpatialPolygonsDataFrame using specified breaks
# and color scheme

plot(soco)
plot(soco, col=colors[np], add=TRUE)
mtext("LISA Map SQRT(CPOV)", cex=1.5, side=3, line=1)
legend("topleft", legend=labels, fill=colors, bty="n")

### Looks something like the GeoDa LISA map.  Would be hard for it to 
### precisely replicate the GeoDa map because of the randomization used
### in GeoDa to determine significance

########################################################################
#####
##### END OF DISCUSSION FOR LOCAL MORAN AND LISA MAP
#####
#########################################################################

# Housekeeping:

ls()
rm(list=ls())
ls()

##########################################################################
##########################################################################
###
### END OF DEMO 5
###
##########################################################################
##########################################################################
