##############################################
# Lab 7: PEER INFLUENCE & QAP REGRESSION LAB #
##############################################

# NOTE: if you have trouble because some packages are not installed, 
# see lab 1 for instructions on how to install all necessary packages.

###########################################################################
# 
# Lab 7
#  
# Part I - Develop peer influence models that meet the high standards of 
# publication in todays top journals (i.e., addressing autocorrelation
# and issues of causality). 
#
# Part II - Use QAP regression to predict increases in social interaction 
# and task interaction for two classrooms. Basically run dyadic level 
# regressions suitable for continuous variables and count data (what 
# ERGM- Exponential Random Graph's cannot do). 
#
###########################################################################


##########################################################################
# PART I -- PEER INFLUENCE
###########################################################################
#
# This lab examines peer effects in classroom data. It first introduces
# the reader to the necessary data processing and manipulation, then basic 
# visualization related to peer-effects. It then introduces the Linear Network
# Autocorrelation Model (lnam), which can be used to better model the 
# dependencies in network data (well, better than an OLS model anyway).
#
# The next section introduces the reader to the concept of matching--treating
# the explanatory variable of interest as an experimental "treatment" 
# and then matching on other covariates. Matching allows the application 
# of straightforward analytical techniques used to analyze experiments,
# namely comparison of means. Code for bootstrapping is introduced to allow for
# non-parametric estimation of the standard errors for the mean. Code is also provided 
# to produce a dotplot showing the means and a (bootstrapped) 95% CI. 
# 
# The data was collected by Dan McFarland (McFarland 2001). The key measures of interest
# for this example are the time 1 and 2 measures of how much each student liked 
# the subject, and the friend network at time 1. 
#
# The codebook is available here: http://stanford.edu/~messing/codebook.v12.txt
###############################################################################

#Clear Variables
rm(list=ls(all=TRUE))
gc()

# Install and load necessary packages:
install.packages("reshape", dependencies = TRUE)
library(reshape)
library(igraph)

data(studentnets.peerinfl, package="NetData")

######################################################################################
# Now we need to reshape our attitudes data according to semester. The data is 
# currently in "long" format, so that student ids and measures are repeated on 
# multiple lines and "semester" is a single variable. We need 
# to transform the data so that measures are repeated on a single 
# line, or "wide" format. We'll use the excellent "reshape" package
# to accomplish this. Take a look at the documentation using ?reshape

?reshape
attitudesw = reshape( attitudes, idvar="std_id",
		timevar="sem_id", direction="wide" )

# Notice that all semester 1 variables now feature a ".1" postscript, while all 
# semester 2 variables feature a ".2" postscript

# We'll first visualize the data. We want to know whether the change in 
# a student's appreciation of a subject (t_2 - t_1) changes in the direction of his 
# friends' appreciation at t_1. 

# So, we'll first take the difference of a few dependent variables 
# for which we might expect peer influence (e.g. "sub" (liking of subject), 
# "cmt" (liking of classmates), and "tch" (liking of teacher)),
# and then take the mean value for an individual's friends' values at t_1.

# First create the delta variables:
attitudesw$deltasub = attitudesw$sub.2 - attitudesw$sub.1   
attitudesw$deltatch = attitudesw$tch.2 - attitudesw$tch.1   
attitudesw$deltacmt = attitudesw$cmt.2 - attitudesw$cmt.1   

# Next we'll create the mean of friends' sub.1.
# We'll use the adjacency matrix, multiply that by the attitudesw$sub.1
# (element-wise, not matrix multiplication), and then take the mean of each row. 

sem1graph = graph.data.frame(d = sem1[1:2], directed=TRUE)
#sem1graph = network(x = sem1[1:2], directed=TRUE)
#sem2graph = graph.data.frame(d = sem2, directed=TRUE)

# But first, we'll need to clean the data to make sure the rows line up!

# let's check to see whether the edge data has the same cases as the attitudes data 
which(!(V(sem1graph)$name %in% as.character(attitudesw$std_id)))
which(!(as.character(attitudesw$std_id) %in% V(sem1graph)$name ))

# They are not the same... we'll need to address this below.

# Now let's get the matrix representation of the network:
sem1matrix = get.adjacency(sem1graph, binary=T)
# This is often referred to as "W" in the literature 

# When you have a large square matrix like this, you can get a good idea of 
# it's density/sparsity using the image function.
image(sem1matrix)

# It's also generally good to know the degree distribution of any network you
# are handling:

plot(density(degree(sem1graph)))

# Looks like degree might be distributed exponentially. We can check as follows:
simexpdist100 = rexp(n=length(V(sem1graph)), rate = .100)
simexpdist125 = rexp(n=length(V(sem1graph)), rate = .125)
simexpdist150 = rexp(n=length(V(sem1graph)), rate = .150)
lines(density(simexpdist100), col="red")
lines(density(simexpdist125), col="blue")
lines(density(simexpdist150), col="green")

# It's not a precise fit, but it might be a good approximation. 

# Let's reorder the attitudes data to make sure it's in the same order as 
# sem1matrix
attitudesw = attitudesw[match(row.names(sem1matrix), attitudesw$std_id),]

# Make sure it worked (this should return 0 if so):
which(row.names(sem1matrix)!=as.character(attitudesw$std_id))
which(colnames(sem1matrix)!=as.character(attitudesw$std_id))

# Let's also make sure that igraph read the graph in correctly:
# (note that igraph can behave strangely if you pass it a 
#  parameter like "directed = FALSE" for a directed network like this one).

sem1$alter_id[sem1$std_id == 149824]
V(sem1graph)[0]
V(sem1graph)[unique(neighbors(sem1graph, v=0, mode = 1))]

# looks good.

# Now we can compute the mean of sub.1 for a student's friends, 
# by element-wise multiplying sub.1 by the matrix and then taking the 
# mean of each non-zero cell in each row:

# Let's first test this out with a simple matrix to make sure we know what 
# we are doing:
(M = matrix(c(1,0,1,0,1,0,1,0,1), nrow=3, ncol=3))

(R = c(1,2,3)) 

R * M

# This is multiplying the first row by 1, the second row by 2, and so on. 
# Instead we want:

t(R * t(M))

# This is multiplying the first column by 1, the second column by 2, and so on.
# Recall that we will be analyzing this data so that rows are cases, so
# this is what we want if we are going to calculate sub.1 for each 
# student's friends' sub.1. (The first option would return the
# student's sub.1 for each non-zero row, not his/her friends' sub.1). 

sem1Wxsub1 = t(attitudesw$sub.1 * t(sem1matrix)) 
sem1Wxtch1 = t(attitudesw$tch.1 * t(sem1matrix)) 
sem1Wxcmt1 = t(attitudesw$cmt.1 * t(sem1matrix)) 

# Now we'll take the mean of cells not equal to zero:
attitudesw$frsub1mean = numeric(length(attitudesw$std_id))
attitudesw$frtch1mean = numeric(length(attitudesw$std_id))
attitudesw$frcmt1mean = numeric(length(attitudesw$std_id))
for(i in 1:length(attitudesw$std_id)){
	attitudesw$frsub1mean[i] = mean(sem1Wxsub1[i,sem1Wxsub1[i,]!=0 ], na.rm=T)
	attitudesw$frtch1mean[i] = mean(sem1Wxtch1[i,sem1Wxtch1[i,]!=0], na.rm=T) 
	attitudesw$frcmt1mean[i] = mean(sem1Wxcmt1[i,sem1Wxcmt1[i,]!=0], na.rm=T)
}

# Now let's plot the data and look at the relationship:
plot(attitudesw$frsub1mean, jitter(attitudesw$deltasub))
plot(attitudesw$frtch1mean, jitter(attitudesw$deltatch))
plot(attitudesw$frcmt1mean, jitter(attitudesw$deltacmt))

# Looks noisy, but there might be a relationship for "sub".

############################################################################
# An alternative way to compute this is iteratively. The advantage
# to the following approach is that if you are working with a large
# data set, you do not need to load an adjacency matrix into 
# memory, you can keep things in edgelist format, which is MUCH
# more efficient. 

attitudesw$mfrsub.1 = numeric(length(attitudesw$sub.1))
attitudesw$mfrtch.1 = numeric(length(attitudesw$sub.1))
attitudesw$mfrcmt.1 = numeric(length(attitudesw$sub.1))

# If you thought the number of alters who like the class mattered
# more than the mean, you might uncomment the code for the nfrsubgt3
# variable and incorporate it into your analysis (two occurrences):
#attitudesw$nfrsubgt3 = numeric(length(attitudesw$sub.1))

for(i in 1:length(attitudesw$std_id)){
	# first get alters of student i:
	altrs = sem1$alter_id[ sem1$std_id == attitudesw$std_id[i] ]
	
	# then get alters' attitudes
	altatts = attitudesw$sub.1[ attitudesw$std_id %in% altrs ]
	
	# now count how many friends like the class more than "3"  
	 attitudesw$nfrsubgt3[i] = length(which(altatts > 3)) 
	
	# then take the mean, ignoring NAs:
	attitudesw$mfrsub.1[i] = mean(altatts, na.rm = TRUE)
	
	# Note that this can all be done in one line of code:
	# attitudesw$mfrsub.1[i] = mean(attitudesw$sub.1[ attitudesw$std_id %in% sem1$alter_id[sem1$std_id %in% attitudesw$std_id[i]]], na.rm=TRUE )
	 attitudesw$mfrtch.1[i] = mean(attitudesw$tch.1[ attitudesw$std_id %in% sem1$alter_id[sem1$std_id %in% attitudesw$std_id[i]]], na.rm=TRUE )
	 attitudesw$mfrcmt.1[i] = mean(attitudesw$cmt.1[ attitudesw$std_id %in% sem1$alter_id[sem1$std_id %in% attitudesw$std_id[i]]], na.rm=TRUE )
}	 
# if you are going to run this with a lot of data, you may wish to include
# the following lines inside your for-loop:
# gc()
# print(i)

# this will run the garbage collector, gc(), which helps R manage memory
# and print(i) will let you know your progress as R chugs away.

# The plot is exactly the same:
plot(attitudesw$mfrsub.1, jitter(attitudesw$deltasub))

# And the correlation should be 1
cor(attitudesw$mfrsub.1,attitudesw$frsub1mean, use= "complete")

# The plots suggest that there might be a relationship for "sub", or 
# how much each student likes the subject. 

# Let's cheat a little bit and run linear models predicting 
# change in each of our variables based on mean friend's values at t=1.
# The data violates many of the assumptions of OLS regression so the 
# estimates are not particularly meaningful, other than to let us know
# that we may be on the right path. 

summary(lm(deltasub~mfrsub.1, data=attitudesw))
summary(lm(deltatch~mfrtch.1, data=attitudesw))
summary(lm(deltacmt~mfrcmt.1, data=attitudesw))

# Significance is always encouraging, even though we are only predicting a tiny
# fraction of the variance (R^2 = .026)

# We'll cheat a little more and remove everyone whose attitudes did not change:
summary(lm(deltasub~mfrsub.1, data=attitudesw, subset = deltasub!=0 ))
summary(lm(deltatch~mfrtch.1, data=attitudesw, subset = deltasub!=0 ))
summary(lm(deltacmt~mfrcmt.1, data=attitudesw, subset = deltasub!=0 ))

# Very nice, for "sub" the effect grows in strength, signficance, 
# and R^2 goes up to .067.

# Let's also take a look at the plot:
pdf("7.1_Peer_effect_on_opinion_of_subject.pdf")
plot( 	x = attitudesw$mfrsub.1, 
		y = jitter(attitudesw$deltasub),
		main = "Peer effects on opinion of subject",
		xlab = "mean friends' opinion",
		ylab = expression(Delta~"opinion from "~t[1]*~to~t[2])  
)

# We can draw a regression line on the plot based on the regression 
# we estimated above:
abline(lm(deltasub~mfrsub.1, data=attitudesw ))
dev.off()

# We've got some evidence of a peer effect on how much a student 
# likes the subject. Our evidence is based on temporal precendence: 
# your friends opinion of the subject predicts how much 
# your opinion the subject changes from t_1 to t_2. 

############################################################################
# Now let's run some "real" models. 
# We'll first estimate the model Y_2 = Y_1 + W*Y_alter, using a network
# autocorrelation model. Read up on it here:

# Roger Th. A. J. Leenders, Modeling social influence through network 
# autocorrelation: constructing the weight matrix, Social Networks, Volume 24, Issue 1, January 2002, Pages 21-47, ISSN 0378-8733, DOI: 10.1016/S0378-8733(01)00049-1.
# (http://www.sciencedirect.com/science/article/B6VD1-44SKCC2-1/2/cae2d6b4cf4c1c21f4f870fd2d58b5cc)

# This model arguably takes into accounts the correlation between friends in
# the disturbances (error term). Yet there are problems with this kind of model. 
# See:

# Eric J. Neuman, Mark S. Mizruchi, Structure and bias in the network autocorrelation 
# model, Social Networks, In Press, Corrected Proof, Available online 31 May 2010, ISSN 0378-8733, DOI: 10.1016/j.socnet.2010.04.003.
# (http://www.sciencedirect.com/science/article/B6VD1-506H0SN-1/2/73a16c627271d29d9283b6d69b873a07)

# With that in mind, let's proceed:
library(sna)
library(numDeriv)

# We'll use the mfrsub.1 variable to approximate the W*Y_alter term. (If 
# we actually use the matrix, we cannot estimate the model--there 
# would be the same number of variables as cases).

# Let's remove the NA values from the attitudes measures
# (otherwise lnam will crash). We'll remove rows with NAs for
# sub.1, sub.2, or mfrsub.1. Note that we could remove any row with an 
# NA value with the syntax: na.omit(attitudesw), but if there are
# rows that contain our variables of interest but are NA for some other 
# variable, na.omit would also drop these rows and we would lose valuable
# data.

atts = attitudesw[!is.na(attitudesw$sub.2),]
atts = atts[!is.na(atts$sub.1),]
atts = atts[!is.na(atts$mfrsub.1),]
W = sem1matrix

# Now we'll make sure the rows and columns in W are in the same 
# order as atts:
W = W[match(atts$std_id,row.names(W)), match(atts$std_id,colnames(W))]
# Let's make sure (this will return 0 if we did it right):
which(rownames(W) != atts$std_id)

# Now we'll estimate the peer influence model using lnam, which stands for
# Linear Network Autocorrelation Model. 
pim1<-lnam(atts$sub.2,cbind(atts$sub.1, atts$mfrsub.1), W2 = W)
summary(pim1)

# This shows a pretty strong peer effect on subjects at time2. Notice that
# the adjusted R^2 is .41. This is not good news. Even after controlling 
# for t1 values of attitude for the subject, we are predicting less than 
# half of the variance in t_2 values. That means that there is a good
# chance of omitted variable bias.

# Below is an attempt to run the model with the actual WYalt as the X matrix.
# For the reasons stated above, it doesn't work. It is included in case the 
# reader wants to uncomment this code and see for him/herself.

#WYalt = sem1Wxsub1
#WYalt = WYalt[match(atts$std_id,row.names(WYalt)), match(atts$std_id,colnames(WYalt))]
#image(WYalt)
#
##pim2<-lnam(y = atts$sub.2, x = WYalt, W2 = W)
#pim2<-lnam(y = atts$sub.2, x = WYalt)
#summary(pim2)


############################################################################
# Matching
# 
# One method that is less vulnerable to some problems related to regression is 
# matching. In matching, one divides the key explanatory variable into discrete 
# "treatments" and then matches participants so that they are as similar on the
# remaining set of covariates as possible. A simple difference in means/medians/
# quantiles can be used. From there we can use a simple t-test to estimate the 
# "treatment" effect without having to worry about multicolinearity.  
# If the data violates the normality assumption (as will be the 
# case here), non-parametric methods such as a permutation test or bootstrapped 
# standard errors can be used. This approach also also violates less statistical 
# assumptions and therefore should be expected to be less biased than running a 
# simple OLS model. 

# For the original article on matching, see:
# Donald B. Rubin, Matching to Remove Bias in Observational Studies
# Biometrics, Vol. 29, No. 1 (Mar., 1973), pp. 159-183
# Stable URL: http://www.jstor.org/stable/2529684

# Also worthy of note is the Rubin Causal Model, which uses matching. 
# Read up on it here: http://en.wikipedia.org/wiki/Rubin_Causal_Model

# However, matching is not a cure-all and is obviously sensitive to the variables 
# on which one matches. For an excellent article on this, see:

# Jeffrey A. Smith, Petra E. Todd, Does matching overcome LaLonde's critique of 
# nonexperimental estimators?, Journal of Econometrics, Volume 125, Issues 1-2, 
# Experimental and non-experimental evaluation of economic policy and models, 
# March-April 2005, Pages 305-353.
# (http://www.sciencedirect.com/science/article/B6VC0-4CXTY59-1/2/06728bd79899fd9a5b814dfea9fd1560)
		
# A good introduction to matching using the MatchIt package can be found courtesy 
# of Gary King here: http://gking.harvard.edu/matchit/docs/matchit.pdf

library(MatchIt)

# First, we dichotomize the independent variable of interest, mean friend opinion regarding 
# subject: mfrsub.1
atts$mftreat = ifelse(atts$mfrsub.1 > 3, 1,0)

atts = atts[c("mftreat", "deltasub", "mfrsub.1","egrd.1", "sub.1", "tot.1", "frn.1", "cmt.1", "prev.1")]

# Before we do any matching, let's take a look at our new treatment variable.

# We'll make this into a factor, which will help interpret our results. A factor
# is a special type of object in R that is used for qualitative (nominal or ordinal) 
# data/variables. A factor is also generally more efficient with respect to memory, 
# because it stores an integer value corresponding to a label (e.g. 1 for "low" and 2
# for "high") rather than storing the entire string. 

# Factors have an order, which you can set by ordering the labels in ascending order 
# when you create the factor, or by using the reorder() function. We'll talk about this
# more in the section on dotplots below. 
atts$mftreatf = factor(atts$mftreat, labels = c("low", "high"))

# Let's compute the means:
tapply(X = atts$deltasub, INDEX = list(atts$mftreatf), FUN = mean)

# Note, the tapply function is useful if you have more than one factor for which
# you want to calcuate some function over. For example, if there were another 
# factor, say exptreat, we could get the mean for each cell using this syntax:
# tapply(X = atts$deltasub, INDEX = list(atts$mftreat, atts$exptreat), FUN = mean)

# A t-test is not totally kosher here because the DV is ordinal, and so it violates
# assumptions behind the t-test.

# But let's take a look anyway:
t.test(deltasub~mftreatf, data=atts)

# A better alternative is the Wilcoxon test (a.k.a. Mann-Whitney test), 
# suitable for ordinal variables.
wilcox.test(x=atts$deltasub[atts$mftreatf=="low"], 
		y=atts$deltasub[atts$mftreatf=="high"],
		conf.int = TRUE)

# Now let's match and redo the analyses. 

# We normally would assign each subject/case to treatment or control groups based on 
# whatever corresponds more closely to a treatment or a control. In this case, it 
# is debatable whether having friends who like the class is more of a "treatment" 
# than having friends who do not like the class. However, there are 246 students 
# whose friends do not like the class versus 99 whose friends do like the class, 
# so we can say that having friends who like the class is certainly more unusual 
# than the opposite. Accordingly, we'll conceptualize having friends who like the 
# class as the treatment.

# In the actual matching proceedure, what we'll do is try to find individuals in 
# the "control" condition (having friends who do not like the subject) who look 
# most similar to each treated individual based on a variety of variables. Ideally, 
# we will achieve "balance" on the covariates between those in the treatment condition, 
# and those we select from the control condition, so that the individuals are as similar 
# as possible. 

# Here are the variables we'll use (all are at time 1):

# sub.1 - how much the student likes the subject
# egrd.1 - expected grade 
# tot.1 - how much the student likes the way the course is taught
# frn.1 - how interesting the subject finds his/her friends
# cmt.1 - how interesting the student finds his/her classmates
# prev.1 - has the student had the teacher previously)

# We'll use "nearest neighbor" matching, which uses a distance measure to 
# select the individual in the "control" group that is closest to each 
# individual in the "treatment" group. The default distance measure is the 
# logistic regression propensity score.

# The codebook for this data is available here: http://stanford.edu/~messing/codebook.v12.txt

m.out = matchit(mftreat ~ egrd.1 + sub.1 + tot.1 + frn.1 + cmt.1 + prev.1,  
		data = atts, method = "nearest")

summary(m.out)
plot(m.out)

# Now let's assess the extent to which our matching algorithm effectively matched each
# treatment subject to a control subject, e.g., the extent to which we achieved balance. 
# The plot function displays Q-Q plots of the control (X-axis) versus treatment groups 
# (Y-axis) for the original sample and the matched sample. The Q-Q plots indicate perfect 
# balance on the covariate in question if all dots are perfectly aligned on the 
# 45 degree line. The further the deviation from this line, the worse the samples are
# balanced on this variable.

# Based on the plots, matching appears to have improved balance on our covariates a 
# little bit, but not perfectly. 
 
# Ideally we want to experiment with other covariates and try to find the combination 
# that best and most parsimoniously captures the key phenomena we want to control for.

matchatts = match.data(m.out)

# We'll make our treatment variable into a factor:
matchatts$mftreatf = factor(matchatts$mftreat, labels = c("low", "high"))

# Let's compute the means:
tapply(X = matchatts$deltasub, INDEX = list(matchatts$mftreatf), FUN = mean)

t.test(deltasub~mftreatf, data=matchatts)

wilcox.test(x=matchatts$deltasub[matchatts$mftreatf=="low"], 
		y=matchatts$deltasub[matchatts$mftreatf=="high"],
		conf.int = TRUE)

# We can also perform a full permuation test. A permutation test is an exact test of 
# whether it is possible to reject the null hypothesis that two distributions are 
# the same. It works by first calulating the mean difference, which fuctions as the 
# test statistic. The difference in sample means is calculated and recorded for every 
# possible way of dividing these pooled values into two groups of size n_A and n_B 
# for every permutation of the group labels A and B. The set of these calculated 
# differences is the exact distribution of possible differences under the null 
# hypothesis that group label does not matter. This test fuctions like a t.test but 
# does not rely on any distributional assumptions about the data. 

# For additional information, see: 
# http://en.wikipedia.org/wiki/Resampling_%28statistics%29#Permutation_tests

library(coin)
independence_test(deltasub ~ mftreatf, data = matchatts,
		distribution = exact())

# Another alternative is to look at bootstrapped estimates of the mean and standard
# error. This is often an attractive way to estimate statistical significance 
# because our data violates the distributional assumptions involved in estimating 
# standard errors in this way (namely, that our dependent variable resembles anything
# like a normal or t distribution--it cannot because it is ordinal with only 
# 7 levels). Because boostrapping relies on resampling instead of distributional 
# assumptions to estimate variance, it is more robust. 

# There are packages available to generate bootstrapped estimates, but seeing the code 
# is a valuable way to get a sense of exactly how bootstrapping works. Here is the code
# for a bootstrapping function designed to estimate the mean and standard error:

b.mean <- function(data, nsim) {	
	# Create a list object to store the sets of resampled data (in this case nsim = 1000).
	resamples = list() 
	
	# Create a vector of the same length to store the resampled means
	r.mean = numeric(nsim)
	
	# Generate a sample with replacement 
	for(i in 1:nsim){
		# generates a random sample of our data with replacement 
		resamples[[i]] = sample(x=data, size=length(data), replace=T)
				
		# Now calcuate the mean for this iteration
		r.mean[i] = mean(resamples[[i]], na.rm=T)
	}
	
	# Calculate the mean of the mean of the simulated estimates above:
	mean.adj = mean(r.mean, na.rm=T)
	
	# Calculate how this differs from the arithmatic mean estimate of the original 
	# sample:
	bias = mean(data, na.rm=T) - mean.adj
	
	# Generate the standard error of the estimate
	std.err <- sqrt(var(r.mean))
	
	# Return results
	return( data.frame(mean = mean(data, na.rm=T), #the mean estimate
					mean.adj = mean.adj, # the adjusted estimate based on simulations
					bias = bias, # the mean minus the adjusted estimate 
					std.err=std.err # the standard error of the estimate (based on simulations)
		) 
	)
}

# Before we use it on our data, let's make sure it works. We will simulate some
# data for which we know the parameters and then estimate the parameters using
# the bootstrap:

simdata = rnorm(n = 1000, mean = 5, sd = 1)
b.mean(simdata, nsim=1000)

# Calculate the theoretical mean and standard error
mean(simdata)
(se = sd(simdata)/sqrt(length(simdata)-1))
# here we use the formula SE = SD/sqrt(N - 1)

# We have demonstrated that our bootstraping function is a good estimator
# for "perfect" data that meets the assumptions behind traditional
# statistical tests. Now let's move on to our data.

# For those with friends with on average postive attitudes:
b.mean(data=matchatts$deltasub[matchatts$mftreatf=="high"],  
		nsim=1000) 

# For those with friends with on average negative attitudes:
b.mean(data=matchatts$deltasub[matchatts$mftreatf=="low"],  
		nsim=1000) 

# Here's how to do it using the boot package, which is faster and more
# extensible than the R-code above:

library(boot)

# You have to write a special function for the boot package, which 
# can be confusing at first, but is useful for more complicated 
# types of bootstrapping. The boot package is also orders of magnitude 
# faster than doing things in R, which is useful if you start to do 
# more complicated bootstrapping or use a higher number of simulations.

# First, write a basic function that will return the estimate in question
# for x data using d cases:

samplemean <- function(x, d) {
	return(mean(x[d]))
}

(bootlow = boot(data = matchatts$deltasub[matchatts$mftreatf=="low"], 
					statistic = samplemean, 
					R = 1000))

(boothigh =boot(data = matchatts$deltasub[matchatts$mftreatf=="high"], 
					statistic = samplemean, 
					R = 1000))

# Note that estimates of the standard errors will be slightly different
# each time due to randomness involved in estimation. As you might expect,  
# as the number of simulations increases, variance will decrease. 

# Based on our bootstrapped estimates and standard error calculations, 
# it's clear that the two means are quite different. So, based on our
# matched samples, it seems there is a significant peer effect. 


##########################################################################
# Let's try full matching so that we are not throwing away any of our data:
m.out = matchit(mftreat ~ egrd.1 + sub.1 + tot.1 + frn.1 + cmt.1 + prev.1,  
		data = atts, method = "full")
matchatts = match.data(m.out)	

# We'll make this into a factor, which will help inturpret our results:
matchatts$mftreatf = factor(matchatts$mftreat, labels = c("low", "high"))

# Note that we'll now have to use the weights that the output provided because
# we told it to use the full sample.

# Let's compute the means:
tapply(X = matchatts$deltasub, 
		INDEX = list(matchatts$mftreatf), 
		FUN = weighted.mean, weights = matchatts$weights )

# There is no straightforward way to compute a t-test in R with weighted data. 

# We can however compute weighted standard errors and a corresponding
# 95% CI:

# Recall that a 95% CI of an estimate is calculated by taking the estimate
# +/- the standard error * 1.96. Actually, +/- 1.96 is just an estimate of the 
# the 0.05 and 0.95 quantiles of a statistical distribution. We'll use
# qt(quantile, N-1) in this case. 
library(Hmisc)
meanlow = wtd.mean(matchatts$deltasub[matchatts$mftreatf=="low"], 
		weights = matchatts$weights[matchatts$mftreatf=="low"])
varlow = wtd.var(matchatts$deltasub[matchatts$mftreatf=="low"], 
		weights = matchatts$weights[matchatts$mftreatf=="low"])
selow = sqrt(varlow)/sqrt(sum(matchatts$weights[matchatts$mftreatf=="low"]))
meanlow + selow * qt(.025, ( sum(matchatts$weights[matchatts$mftreatf=="low"]) - 1))
meanlow + selow * qt(.975, ( sum(matchatts$weights[matchatts$mftreatf=="low"]) - 1))

meanhigh = wtd.mean(matchatts$deltasub[matchatts$mftreatf=="high"], 
		weights = matchatts$weights[matchatts$mftreatf=="high"])
varhigh = wtd.var(matchatts$deltasub[matchatts$mftreatf=="high"], 
		weights = matchatts$weights[matchatts$mftreatf=="high"])
sehigh = sqrt(varhigh)/sqrt(sum(matchatts$weights[matchatts$mftreatf=="high"]))
meanhigh + sehigh * qt(.025, ( sum(matchatts$weights[matchatts$mftreatf=="high"]) - 1))
meanhigh + sehigh * qt(.975, ( sum(matchatts$weights[matchatts$mftreatf=="high"]) - 1))

# Here's how to do it with the boot package:
sample.wtd.mean <- function(x, d, wts) {
	return(weighted.mean(x[d], w = wts))
}

(bootlow = boot(data = matchatts$deltasub[matchatts$mftreatf=="low"], 
					wts = matchatts$weights[matchatts$mftreatf=="low"],
					statistic = sample.wtd.mean, 
					R = 1000))
(boothigh =boot(data = matchatts$deltasub[matchatts$mftreatf=="high"], 
					wts = matchatts$weights[matchatts$mftreatf=="high"],
					statistic = sample.wtd.mean, 
					R = 1000))

# We can make a nice plot of the results with the lattice package:
library(lattice)

# Dot plots: 
visual = list()
visual$var = c("Friends do not like subj.", "Friends like subj.")
visual$M = c(bootlow$t0, boothigh$t0)
visual$se = c(sd(bootlow$t), sd(boothigh$t))
visual$N = c(length(matchatts$weights[matchatts$mftreatf=="high"]),
		length(matchatts$weights[matchatts$mftreatf=="low"]))
visual$lo = visual$M - visual$se 
visual$up = visual$M + visual$se
visual = as.data.frame(visual)

#print it out:
pdf(file="7.2_dotplot_matchoutput_bootstrapped_SE.pdf", width = 6, height = 3)
dotplot( reorder(var,M) ~ M, 
		data = visual,
		main="Effect of friends' attitude on opinion change,\nmatched samples with bootstrapped SE",
		panel = function (x, y, subscripts, groups, ...) {
			panel.dotplot(x, y)
			panel.segments(x0=visual$lo[subscripts],y0=as.numeric(y),
					x1=visual$up[subscripts],y1=as.numeric(y), 
					lty = 1 )
		},
		xlab=expression(Delta~"opinion from "~t[1]*~to~t[2]),
		xlim= c(-.5, .2)
)
dev.off()

# Very nice! 

############################################
# QUESTION #1 - What do these results show? 
# What assumptions are being made? 
# What might lead you to question the results? What would improve them?
############################################

#########################################################################
# Extra-credit:
# 
# Use the McFarland dataset to acquire other individual level variables 
# and develop better matching. Then follow this lab and explore the 
# variety of outcomes suitable for illustrating peer effects in 
# classrooms (e.g., PSAT scores, engagement, conflict, etc). Results 
# are likely suitable for an A-journal publication. 
# 
# Again, the codebook is available here: http://stanford.edu/~messing/codebook.v12.txt
#
#########################################################################


#########################################################################
# PART II -- QAP REGRESSION
#########################################################################
#
# 	Here we want to compare different classrooms and discern what leads 
# participants to become more or less sociable and academically engaged 
# with one another. 
#
# 	Class m173 is an accelerated math class of all 10th graders taught 
# by a rigid teacher who used teacher-centered instructional formats. 
# Class m 182 is a regular math class of 10th and 11th grade students 
# taught by a casual teacher who used student-centered instructional formats. 
#
#########################################################################


# igraph and sna don't mix well.  Before we load sna, we will detach igraph.
detach(package:igraph)

# Load the "sna" library
library(sna)

#Clear Variables
rm(list=ls(all=TRUE))
gc()

#(2) QAP Regressions for m l73

# Note that each matrix must be the same size (n x n). You'll want to make  
# sure all input and output matrices are the same size. Predictor matrices 
# thus are not individual level variables; they are differences, matches, 
# etc. You'd use a matrix of differences in of quantitative (node level) 
# variables, and matches in the case of categorical/dichotomous (node 
# level) variables. There's a really easy way to do it in R 
# (say your node-level variables are x and y)
# x <- seq(1,4)
# y <- seq(2,5)
# outer(x,y,FUN="-") # find the difference between x and y
# outer(x,y,FUN="==") # produce 0/1 similairty matrix
# For this lab we will use matrices that were previously generated in UCINet

# Loading predictor matrices.  Each matrix is a n x n matrix 
# data is saved in a CSV format.  You can open the files in Excel
# to see the structure.

data(studentnets.mrqap173, package="NetData")

# Look at what we loaded via
ls()

# We need the data in matrix format 
# predictor matrices
m173_sem1_SSL <- as.matrix(m173_sem1_SSL)
m173_sem1_TSL <- as.matrix(m173_sem1_TSL)
m173_sem1_FRN <- as.matrix(m173_sem1_FRN)
m173_sem1_SEAT <- as.matrix(m173_sem1_SEAT)
m173_sem1_RCE <- as.matrix(m173_sem1_RCE)
m173_sem1_GND <- as.matrix(m173_sem1_GND)

# Load response matrices
m173_sem2_SSL <- as.matrix(m173_sem2_SSL)
m173_sem2_TSL <- as.matrix(m173_sem2_TSL)

# In order to run the QAP regression we must create an array of matrices
# containing all the predcitor matrices.  We are, in effect, creating a 
# 3-d matrix (predcitor x n x n).

# Important: All matrices must be the same size!  

response_matrices <- array(NA, c(6, length(m173_sem1_SSL[1,]),length(m173_sem1_SSL[1,]))) 
response_matrices[1,,] <- m173_sem1_SSL
response_matrices[2,,] <- m173_sem1_TSL
response_matrices[3,,] <- m173_sem1_FRN
response_matrices[4,,] <- m173_sem1_SEAT
response_matrices[5,,] <- m173_sem1_RCE
response_matrices[6,,] <- m173_sem1_GND

##############################
#(2a) SSL2 <- SSL1 + TSL1 + FRN1 + SEAT1 + RCE + GND
##############################

# Fit a netlm model by using netlm, the response matrix and the array of predictor matrices
# This may take a LONG time.
nl<-netlm(m173_sem2_SSL,response_matrices)

# Make the model easier to read by adding lables for each predictor matrix.
nlLabeled <- list()
nlLabeled <- summary(nl)
# Labels are provided in the same order as they were assigned in the response_matrices array
nlLabeled$names <- c("Intercept", "Social normalized and labeled (SSL1)", "Task normalized and labeled (TSL1)", "Friends 1=friend, 2=best friend(FRN1)", "Seat in first semester (Seat1)","Race (RCE)","Gender (GND)")

# Round the ocefficients to two decimals
nlLabeled$coefficients = round(nlLabeled$coefficients, 2)
nlLabeled

##############################
#(2b) TSL2 <- TSL1 + SSL1 + FRN1 + SEAT1 + RCE + GND
##############################

# Fit a netlm model by using netlm, the response matrix and the array of predictor matrices
nl<-netlm(m173_sem2_TSL,response_matrices)

#make the model easier to read
nlLabeled <- list()
nlLabeled <- summary(nl)
nlLabeled$names <- c("Intercept", "Social normalized and labeled (SSL1)", "Task normalized and labeled (TSL1)", "Friends 1=friend, 2=best friend(FRN1)", "Seat in first semester (Seat1)","Race (RCE)","Gender (GND)")

nlLabeled$coefficients = round(nlLabeled$coefficients, 2)
nlLabeled

##############################
#(3) QAP Regressions for m 182
##############################

# Repeat for class m 182

# Clear Variables
rm(list=ls(all=TRUE))
gc()

data(studentnets.mrqap182, package = "NetData")

# Look at what we loaded via
ls()

# Again, we need the data in matrix format 
# predictor matrices
m182_sem1_SSL <- as.matrix(m182_sem1_SSL)
m182_sem1_TSL <- as.matrix(m182_sem1_TSL)
m182_sem1_FRN <- as.matrix(m182_sem1_FRN)
m182_sem1_SEAT <- as.matrix(m182_sem1_SEAT)
m182_sem1_RCE <- as.matrix(m182_sem1_RCE)
m182_sem1_GND <- as.matrix(m182_sem1_GND)

#response matrices
m182_sem2_SSL <- as.matrix(m182_sem2_SSL)
m182_sem2_TSL <- as.matrix(m182_sem2_TSL)

#This class will require you to make multiple extraction operations. 
#A student exits the class at the end of first semester and another 
#enters at the start of second semester. These students must be removed 
#before you can conduct QAP regression (you need the same row-column orderings).
#Extract actor 15 for TSL and SSL of second semester (# 15 <- new student). 

response_matrices <- array(NA, c(6, length(m182_sem1_SSL[1,]),length(m182_sem1_SSL[1,]))) 
response_matrices[1,,] <- m182_sem1_SSL
response_matrices[2,,] <- m182_sem1_TSL
response_matrices[3,,] <- m182_sem1_FRN
response_matrices[4,,] <- m182_sem1_SEAT
response_matrices[5,,] <- m182_sem1_RCE
response_matrices[6,,] <- m182_sem1_GND

##############################
#(3a) SSL2 <- SSL1 + TSL1 + FRN1 + SEAT1 + RCE + GND
##############################

#Fit a netlm model
nl<-netlm(m182_sem2_SSL,response_matrices)

#make the model easier to read
nlLabeled <- list()
nlLabeled <- summary(nl)
nlLabeled$names <- c("Intercept", "Social normalized and labeled (SSL1)", "Task normalized and labeled (TSL1)", "Friends 1=friend, 2=best friend(FRN1)", "Seat in first semester (Seat1)","Race (RCE)","Gender (GND)")

nlLabeled$coefficients = round(nlLabeled$coefficients, 2)
nlLabeled

##############################
#(3b) TSL2 <- SSL1 + TSL1 + FRN1 + SEAT1 + RCE + GND
##############################

#Fit a netlm model
nl<-netlm(m182_sem2_TSL,response_matrices)

#make the model easier to read
nlLabeled <- list()
nlLabeled <- summary(nl)
nlLabeled$names <- c("Intercept", "Social normalized and labeled (SSL1)", "Task normalized and labeled (TSL1)", "Friends 1=friend, 2=best friend(FRN1)", "Seat in first semester (Seat1)","Race (RCE)","Gender (GND)")

nlLabeled$coefficients = round(nlLabeled$coefficients, 2)
nlLabeled

#Report your results. Describe what they mean? Repeat for ETSL of second semester.

#######################################################################
# QUESTION #2 - Compare your results for m 173 and m 182. 
# Do the classes have different results?   
# If not, why not?
# If so, why so?
# What sort of story can you derive about the change in 
# task and social interactions within these classrooms?  
#
# Remember you are predicting changes in relations of sociable or task 
# interaction using other types of relations (i.e., friendship, 
# proximate seating, same race, etc).

#########################################################################
#
# Extra-credit - run the QAP regression part of the lab on the 
# entire McFarland dataset of classrooms, and perform meta-analyses 
# on the results using multi-level modeling - then presto - 
# you will have results on academic and social relations in classrooms 
# which is suitable for publication in an A-journal. 
#
#########################################################################

