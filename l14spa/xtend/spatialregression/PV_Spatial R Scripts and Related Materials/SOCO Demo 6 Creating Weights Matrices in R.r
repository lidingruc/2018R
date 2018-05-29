##########################################################################
###
### CREATING WEIGHTS MATRICES IN R
### KENTUCKY DEMO 6
### LAST UPDATED APRIL 30, 2011
### RUN SUCCESSFULLY IN R VERSION 2.13.0 ON 5/4/11
###
###########################################################################
###
### LISA STATISTICS IN R
### IN THIS DEMO WE:
###     1. Read in the shape file and text file data
###     2. Read in the Queen1 matrix generated in GeoDa
###     3. Learn some differences between GeoDa and R
###     4. Generate some weights matrics in R
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
row.names(soco.df) <- soco.df$NAME
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

####### READ IN A WEIGHTS MATRIX CREATED IN GEODA AS IN EARLIER DEMOS:

socogal <- read.gal("socoQueen1.GAL", override.id=TRUE)
class(socogal)   # nb object; an assignment of neighbors to each observation.
head(socogal)

# Most of the functions in the package spdep require conversion to a
# listw object:

socoQ1.gal <- nb2listw(socogal)
class(socoQ1.gal)  # Also now a listw object
head(socoQ1.gal)   # Same info with a different layout

########################################################################
###
### CREATING A WEIGHTS MATRIX IN R
###
########################################################################

# We needn't rely exclusively on bringing our weights matrices into R
# after first creating them in GeoDa.  Indeed, in R we can have several
# different wights matrices to serve different purposes.  Can even create
# an inverse distance weight (IDW) that often is much more appealing
# then the simple binary distance matrix in GeoDa.

### Now create the coordinate list from the shapefile

coords <- coordinates(soco)

# Remember, for better or worse (probably worse), the map is unprojected
# and the metric units are meters from an origin along the Kansas-Nebraska
# state boundary. Take a look at the first 10 rows in object coords:

coords[c(1:10),]
head(coords)
length(coords[,1])

# Here we generate (in R!) a neighbor list for the shape file polygons.
# It's reasonably fast despite the size of our dataset.

R.neigh.list <- poly2nb(soco)
class(R.neigh.list)

# Check to see if our new weights matrix (R.neigh.list) is different from
# that produced by GeoDa:

diff <- card(socogal) - card(R.neigh.list)
which(diff!=0)    # There's a difference in the neighbor lists

row.names(soco.df)[c(571, 1240)]

# It looks like they differ for two observations:
# Montgomery_MD and Arlington_VA

# Let's look at the summary of each:

summary(R.neigh.list)
summary(socogal)

# It appears that in R, these 2 counties are considered neighbors, and they're
# not in GeoDa.  Why?  In the soco shape file there must be an extremely small
# distance separator between these two.  R has a "snap" tolerance that spanned
# this difference.

#########################################################################
###
### GENERATE A DISTANCE MATRIX IN R:
###
#########################################################################

coords1 <- coords
coords2 <- coords
dist_matrix <- crossdist(coords1[,1], coords1[,2], coords2[,1], coords2[,2])
class(dist_matrix)
isSymmetric(dist_matrix)

# Set diagonal elements of matrix to 0

diag(dist_matrix) <- 0.0
dim(dist_matrix)

################ Generate an inverse distance and
################ inverse distance-squared matrices

IDW_matrix <- (1 / dist_matrix)
IDWsq_matrix <- (1 / (dist_matrix^2))
isSymmetric(IDW_matrix)
isSymmetric(IDWsq_matrix)

# Set diagonal elements of matrix to 0; several of the following calls
# take quite a bit of time to run

diag(IDW_matrix) <- 0.0
diag(IDWsq_matrix) <- 0.0
dim(IDW_matrix)
dim(IDWsq_matrix)

##### For now, focus on the inverse distance-squared weights matrix:

dist.list.IDWsq <- mat2listw(IDWsq_matrix, row.names=socotxt$NAME)
class(dist.list.IDWsq)
names(dist.list.IDWsq)

# Generate a binary connectivity matrix with 100 km threshold

nb.binary <- ifelse((dist_matrix <= 100000), 1, 0)
diag(nb.binary) <- 0
nb.binary.list <- mat2listw(nb.binary, row.names=socotxt$NAME)
class(nb.binary.list)
names(nb.binary.list)
summary(nb.binary.list)
dist.B <- nb2listw(nb.binary.list$neighbours, glist=NULL, style="B")
dist.W <- nb2listw(nb.binary.list$neighbours, glist=NULL, style="W")
isSymmetric(nb.binary)
dist.list.binary <- mat2listw(nb.binary, row.names=socotxt$NAME)

# If the centroids are within 100 km, then weight them according to
# the inverse distance between the centroids

dlist <- nbdists(dist.list.binary$neighbours, coords1, longlat = NULL)
idw.sq <- lapply(dlist, function(x) 1/(x^2))
dist.B.IDW.sq <- nb2listw(dist.list.binary$neighbours, glist=idw.sq,
	style="B")
summary(dist.B.IDW.sq)
class(dist.B.IDW.sq)

######### Check it out

names(attributes(dist.B.IDW.sq))  # Attribute names 
card(dist.B.IDW.sq$neighbours)   # Shows the no. of neighbors for each obs.
range(card(dist.B.IDW.sq$neighbours))  # Range of the no. of neighbors
1/rev(range(card(dist.B.IDW.sq$neighbours))) # Range of the weights

# Try it out using the OLS regression from the previous Demos

reg1 <- lm(cpov ~ femhh + unem + hied)

moran.test(reg1$residuals, dist.B.IDW.sq, alternative="two.sided")
moran.test(reg1$residuals, socoQ1.gal, alternative="two.sided")

########################################################################

# Housekeeping:

ls()
rm(list=ls())
ls()

##########################################################################
##########################################################################
###
### END OF DEMO 6
###
##########################################################################
##########################################################################

