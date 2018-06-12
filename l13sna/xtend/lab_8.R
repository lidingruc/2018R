#########################################################################
# Lab 8: ERGM LAB
#########################################################################
# Lab goals: 
# 1) To create ERGM models
# 2) To compare ERGM models
# 3) Consider ERGM performance complications 
##########################################################################


# NOTE: if you have trouble because some packages are not installed, 
# see lab 1 for instructions on how to install all necessary packages.

#outline
#1) ERGM creation
#2) MCMC diagnostics
#3) ERGM model simulation
#4) Model comparison
#5) ERGM performance improvements


# load the "ergm" library
library(ergm)

## Load the data:
data(studentnets.ergm173, package = "NetData")

# The IDs in the data correspond to IDs in the complete dataset. 
# To execute the ERGM, R requires continuous integer IDs: [1:n],
# where n is the total number of nodes in the ERGM. So, create
# node IDs acceptable to R and map these to the edges.

# Create 22 unique and sequenced IDs
id <- seq(1,22,1)

# Join these IDs to the nodes data (cbind = column bind), and
# reassign this object to 'nodes'
nodes<-cbind(id, nodes)

# Check the new nodes data to see what we've got.  Notice that we
# now have integer-increasing IDs as a vector in our data frame.  
nodes

# Merge the new IDs from nodes with the ego_id and alter_id values
# in edges. Between merge steps, rename variables to maintain
# consistency.  Note that you should check each data step using
# earlier syntax.  Note that R requires the same ordering for node
# and edge-level data by ego_id.  The following sequence preserves
# the edgelist ordering, rendering it consistent with the 
# node ordering.
edges2<-merge(nodes[,1:2], edges, by.x = "std_id", by.y="alter_id")

# Note that we lose some observations here.  This is because the
# alter_id values do not exist in the node list.  Search will
# indicate that these IDs are also not in the set of ego_id values.
names(edges2)[1]<-"alter_id"

# just assigning new names to first column.
names(edges2)[2]<-"alter_R_id"
edges3<- merge(nodes[,1:2], edges2, by.x = "std_id", by.y="ego_id")

# shows that we merged new alter id that reflects 
# integer id which R requires.
names(edges3)[1]<-"ego_id"
names(edges3)[2]<-"ego_R_id"
edges3
# The edges3 dataset now contains integer-increasing IDs sorted by
# ego_R_id. For our work, we will use the ego_R_id and alter_R_id
# values, but we retain the std_id values for reference.

# Specify the network we'll call net - where dyads 
# are the unit of analysis...
net<-network(edges3[,c("ego_R_id", "alter_R_id")])

# Assign edge-level attributes - dyad attributes
set.edge.attribute(net, "ego_R_id", edges[,2])
set.edge.attribute(net, "alter_R_id", edges[,4])

# Assign node-level attributes to actors in "net"
net %v% "gender" <- nodes[,3]
net %v% "grade" <- nodes[,4]
net %v% "race" <- nodes[,5]
net %v% "pci" <- nodes[,6]

# Review some summary information regarding the network to make
# sure we have #specified things correctly.  
summary(net)

# Let's take a look at the network.
pdf("8.1_lab8_network.pdf")
plot(net)
dev.off()

# Let's execute a model in which we attempt to explain semester 2
# friendship selections exclusively with node-level
# characteristics.
m1<-ergm(net ~ edges + mutual + nodematch("gender") + absdiff
	("pci"),burnin=15000,MCMCsamplesize=30000,verbose=FALSE)

# The ERGM runs by an MCMC process with multiple starts, and this
# helps you see if the model is converging.  If the estimated
# coefficient values were to change dramatically, it might be a
# sign of a poorly specified model).  You should see the 
# log-likelihood increase with each iteration.

# You will see a loop warning.  You can ignore this for now.

# Before trying to interpret the data, it is a good idea to check
# the MCMC process
pdf("8.2_lab8_mcmc_m1.pdf")
mcmc.diagnostics(m1)
dev.off()

# You will see several plots and output.  The plots to the right
# should look approximately normal. The output tells us three
# things of interest: 

# 1) The accuracy of the model (r)
# 2) If we used a sufficiently large burn-in
# 3) If we used a sufficiently large sample in the simulation

# In our case the samples might be too small.  This doesn't mean
# the results of the ERGM results are wrong, but we should take
# care in specifying the sample size.

# Let's look at the summary of the results.  We could create a new
# object that is the summary score info, but here we'll just send
# it to the screen.

# Let's assess the model
summary(m1)

# show the exp() for the ERGM coefficients
lapply(m1[1],exp)

# The first section gives the model (formula) estimated in the
# ERGM. Here we said that the network was a function of edges, 
# mutual ties and matching with respect gender.  Another way 
# to think about this, is that we're generating random networks
# that match the observed network with respect to the number of
# edges, the number of mutual dyads, the number of ties within
# between race and within/between gender. 

# The second section tells how many iterations were done.  The
# MCMC sample size is the number of random networks generated in
# the production of the estimate. 

# The next section gives the coefficients, their SEs and pValues.  
# These are on a log-odds scale, so we interpret them like logit
# coefficients.  

# An edges value of -2.314 means that the odds of seeing a tie on
# any dyad are exp(-2.314) ~= 0.098, which could be thought of as
# density net of the other factors in the model. If you only
# have 'edges' in the model, then exp(b1) should be very close to
# the observed density. Edges are equivalent to a model intercept
# -- while possible, I can't imagine why one would estimate a
# model without an edges parameter.

# A mutual value of 2.412 means that reciprocity is more common
# than expected by chance (positive and significant), and here we
# see that exp(2.412)=11.15, so it's much more likely than chance
# -- we are 11 times as likely to see a tie from ij if ji than if
# j did not nominate i.

# We are exp(1.574e-02)=1.015 times more likely to nominate within
# gender than across gender.

# The final section refers to overall model fit and MCMC
# diagnostic statistics (AIC, BIC).

# Let's now create a couple of additional networks so that we can
# add earlier friendships and seating proximity to our model. 
# We'll do this 2 different ways.  For seating, we'll create an
# entirely new network.  For friend_sem1, we'll assign additional 
# attributes to the original network.  These are interchangeable.   
seat <- net

# Assign an edge-level attribute of 'seat' to capture the network
# of seating we create a proximity network via seating location...
set.edge.attribute(seat, "seat_net", edges3[,7])

# Assign an edge-level attribute of 'net' to capture sem1
# friendships.
set.edge.attribute(net, "friend1", edges3[,5])

# Note: thus far, we've treated gender as a homogenous matching
# parameter.  We can alternatively allow this effect to vary
# across grades.  Do this by adding a 'diff=TRUE' option for the
# nodematch term.  Many terms have options that change their
# effect, so look at the help files to clarify.

# Create variables to represent sem1 mutuality and transitivity
# Create a new network based on the sem1 friendships.  Use the
# network commands to convert this to a matrix.
test<-edges["sem1_friend">=1,]

test2<-merge(nodes[,1:2], test, by.x = "std_id", by.y="alter_id")
names(test2)[1]<-"alter_id"
names(test2)[2]<-"alter_R_id"
test3<- merge(nodes[,1:2], test2, by.x = "std_id", by.y="ego_id")
names(test3)[1]<-"ego_id"
names(test3)[2]<-"ego_R_id"
net1<-network(test3[,c("ego_R_id", "alter_R_id")])

A<-as.matrix(net1)
B<-t(as.matrix(net1)) #B = A transpose
mut_mat <- A + B
lag_mut<-as.network(mut_mat) # relies on dichotomous
                             # interpretation of edges

# Calculate sem1 transitivity using A matrix from above
# This is highly colienar with our response variable and will
# cause the ERGM to fail. For a different network, you would use
# the code below to calculate semester 1 transitvity:
# sqA<-A%*%A #matrix multiplication
# sem2_trans<-sqA*A #element-wise multiplication
# sem2_trans_net <- as.network(sem2_trans)

# Create another model that uses the sem1 mutuality 
m2<-ergm(net ~ edges + mutual + nodematch("gender") + 
	nodematch("race")  + edgecov(lag_mut),burnin=20000,
	MCMCsamplesize=70000,verbose=FALSE,seed=25,
	calc.mcmc.se = FALSE,maxit=6)

pdf("8.3_lab8_mcmc_m2.pdf")
mcmc.diagnostics(m2)
dev.off()

summary(m2)
# We might get a warning here.  This means that R was unable to
# compute standard errors for all predictors.  This could be due
# to a number of causes for the purpose of this example we ignore
# the waring and move on, but in your work you will want to check
# your data for potential problems

# Now let's look at goodness of fit.  In addition to the standard
# GOF statistics, we can use the simulation features of the
# program to see if our models match reality.  Since the models
# are effectively proposals about what is driving the observed
# network, we can ‘back predict from the model to produce a set
# of random networks that are draws from the distribution of
# networks implied by the model.  We can then compare the
# predicted model to the observed model for features not built
# into the model.  So, for example, if the only features 
# generating the global network in reality are mixing by grade and 
# race, then we should get matching levels of transitivity,
# geodesic distances and so forth with the predicted model.  The
# tools for doing this are (a) to simulate from the model and (b)
# to use the built in GOF functions.

# (a) simulating networks from an estimated model
# The higher the value of nsim the longer this will take
m2.sim<-simulate(m2,nsim=100);

simnet1<-m2.sim$networks[[1]]
summary(simnet1)
pdf("8.4_lab8_m2_simulation.pdf")
plot(m2.sim$networks[[1]],vertex.col="WHITE")
dev.off()

# Note the resulting net looks a lot like what we have estimated.
# You could easily simulate, say, 1000 nets from your model and
# the write a loop that pulls statistics of interest out of each
# one (like centralization or some such), to compare against your
# observed network.

#This is, essentially, what the built-in GOF models do….

# (b) Generating GOF fits from an estimated model
# the built in goodness-of-fit routine is also very useful.
m2.gof <- gof(m2~idegree)
pdf("8.5_lab8_m2_gof.pdf")
plot(m2.gof)
dev.off()

# This figure plots the distribution of simulated popularity (in-
# degree) as box-plots, with the observed values overlain.  Here
# we see a pretty-good fit, particularly in the middle and tail
# regions. Recall that in model 5, popularity is *not* one of the
# parameters in the model, so this suggests that with the features
# we do include, we can account for the observed degree
# distribution.  

# There are also a number of advanced options for running ERGM
# models designed to (a) allow one to specify structural
# parameters of interest, (b) evaluate the convergence of the
# MCMC, and (c) test for degenerate models (models that look
# like they fit, but that actually predict an odd portion of the
# graph sample space).


# LAB QUESTIONS:

# 1. On your own, using these variables and the 'summary(<model
# name>)' command, explore the model that you believe to be the best
# one.  Explain its strengths relative to the other models and the
# logic that suggests this answer to you.

# 2. Why don't we use the node-level variable 'grade' for any of
# the models?  Using the syntax above as a guide, include 'grade'
# in a variant of m1, m1.2, and report the results from R.

# 3. Describe what we did to calculate the mutuality and
# transitivity scores.

# 4. Describe each of the command terms: edgecov, mutual,
# edges, nodematch, and absdiff.  
# NOTE: the command '
 
help("ergm-terms")

#will be very useful here.  

################################
#improving ergm performance
################################

# ergm is slow, but modern computers can help a lot.
# an ergm model tries to compute the same general result multiple
# times we can use many threads to harness the power of multicore
# processors we do this with the parallel arguement in ergm

#####WARNING######
# if you are not using a multicore processor this will slow down
# your analysis for most new computers you should use parallel=4.

# let's run the model 4 again with four threads.
 
m2_fast<-ergm(net ~ edges + mutual + nodematch("gender") + 
	nodematch("race")  + edgecov(lag_mut),burnin=20000,
	MCMCsamplesize=70000,verbose=FALSE,seed=25,
	calc.mcmc.se = FALSE,maxit=6,parallel=4)

# this takes a while to run...

summary(m2)