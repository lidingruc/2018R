#######################################
# LAB 3: Clusters, Factions and Cores #
#######################################

# NOTE: if you have trouble because some packages are not installed, 
# see lab 1 for instructions on how to install all necessary packages.


##############################################################
# 
# Lab 3 
#
# The purpose of this lab is to identify friendship groups 
# or cliques using different methods, to discern the best 
# fitting clique structure, and to develop clique-models of 
# task and social interaction patterns. 
#
##############################################################


###
#1. SETUP
###

# For this lab, we'll use a few different packages. "sna" is a
# social-network-analysis package similar to igraph, but with some
# different and useful functions. "cluster" is a generic cluster-
# analysis package with applications beyond the social-network
# context. "animation" is used, not surprisingly, to produce
# animations.

install.packages("animation", repos = "http://cran.cnr.berkeley.edu/", dependencies = TRUE)
library('igraph')
library('cluster')
library('animation')


###
#2. LOADING AND FORMATTING DATA
###

# We'll use the NetData package to load the data this time around:

data(studentnets.M182, package = "NetData")

# For this lab, we'll use three files from Student Nets M182 Sem
# 2. FRN.DAT shows self-reported friendship ties, SSL.DAT shows 
# observed social interactions, and TSL.DAT shows observed 
# task interactions.
#
# All of the files should be in the following format:
#        1        1 0.000000
#        1        2 2.000000
#        1        3 1.000000
# where the first column is the ego, the second column is the alter, 
# and the third column is an integer or floating-point number
# greater than or equal to zero showing the strength of the 
# association for the given two vertices.

# Reduce to non-zero edges and build a graph object
m182_full_nonzero_edges <- subset(m182_full_data_frame, (friend_tie > 0 | social_tie > 0 | task_tie > 0))
head(m182_full_nonzero_edges)

m182_full <- graph.data.frame(m182_full_nonzero_edges) 
summary(m182_full)

# Create sub-graphs based on edge attributes
m182_friend <- delete.edges(m182_full, E(m182_full)[get.edge.attribute(m182_full,name = "friend_tie")==0])

m182_social <- delete.edges(m182_full, E(m182_full)[get.edge.attribute(m182_full,name = "social_tie")==0])
summary(m182_social)

m182_task <- delete.edges(m182_full, E(m182_full)[get.edge.attribute(m182_full,name = "task_tie")==0])
summary(m182_task)

# Look at the plots for each sub-graph
friend_layout <- layout.fruchterman.reingold(m182_friend)
plot(m182_friend, layout=friend_layout, edge.arrow.size=.5)

social_layout <- layout.fruchterman.reingold(m182_social)
plot(m182_social, layout=social_layout, edge.arrow.size=.5)

task_layout <- layout.fruchterman.reingold(m182_task)
plot(m182_task, layout=task_layout, edge.arrow.size=.5)


###
# 3. COMMUNITY DETECTION
###

# We'll use the friend sub-graph as the basis for our community 
# detection methods. For clarity and simplicity, we'll set the
# network to undirected and remove isolated vertices. Comparing
# m182_friend before and after these operations, you'll notice
# that the number of edges decreases as reciprocated directed ties
# are consolidated into single undirected ties, and the number of
# vertices decreases as isolates are removed.
m182_friend_und <- as.undirected(m182_friend, mode='collapse')
m182_friend_no_iso <- delete.vertices(m182_friend_und, V(m182_friend_und)[degree(m182_friend_und)==0])
summary(m182_friend)
summary(m182_friend_no_iso)

# There are many different ways to detect communities. In this 
# lab, we'll use three: hierarchical NetCluster, walktrap, and 
# edge-betweenness. As you use them, consider how they portray 
# clusters and consider which one(s) afford a sensible view of 
# the social world as cohesively organized. 


###
# 3A. COMMUNITY DETECTION: WALKTRAP
###

# This algorithm detects communities through a series of short
# random walks, with the idea that the vertices encountered on 
# any given random walk are more likely to be within a community
# than not. The algorithm initially treats all nodes as
# communities of their own, then merges them into larger
# communities, and these into still larger communities, and so on.
# In each step a new community is created from two other
# communities, and its ID will be one larger than the largest
# community ID so far. This means that before the first merge we
# have n communities (the number of vertices in the graph)
# numbered from zero to n-1. The first merge creates community n,
# the second community n+1, etc. This merge history is returned by
# the function.

# The Walktrap algorithm calls upon the user to specify the length of random walks, 
# and Pons and Latapy (2005) recommend walks of 4 or 5 steps. However, Waugh 
# et al found that for many groups (Congresses), these lengths did not provide the 
# maximum modularity score (this is what I assume to be the numbers on the left 
# hand side of the dendrogram—I can’t figure it out. If it’s not this, we should #
# output the modularity score and plot that [same for edge betweenness – what 
# Newman-Girvan do]). To be thorough in their attempts to optimize modularity, 
# they performed the walktrap algorithm 50 times for each group (using random 
# walks of lengths 1–50) and selected the network partition with the highest 
# modularity value from those 50. They call this the “maximum modularity 
# partition” and insert the parenthetical “(though, strictly speaking, this cannot be 
# proven to be the optimum without computationally-prohibitive exhaustive 
# enumeration (Brandes et al. 2008)).”


friend_comm_wt <- walktrap.community(m182_friend_no_iso, steps=200,modularity=TRUE,labels=TRUE)
friend_comm_wt

# As with hierarchical NetCluster above, we can also visualize 
# the clusters generated by walktrap as a dendrogram (but note that 
# the clusters themselves may be different). Here, the y-axis 
# reflects the distance metric used by the walktrap algorithm; for 
# more on this, see Pascal Pons, Matthieu Latapy: Computing communities 
# in large networks using random walks, http://arxiv.org/abs/physics/0512106.
friend_comm_dend <- as.dendrogram(friend_comm_wt, use.modularity=TRUE)
plot(friend_comm_dend)

# Question #2 - How many clusters would you select here and why?


###
# 3B. COMMUNITY DETECTION: EDGE BETWEENNESS METHOD 
###

# The edge-betweenness score of an edge measures the number of
# shortest paths from one vertex to another that go through it. 
# The idea of the edge-betweenness based community structure
# detection is that it is likely that edges connecting separate
# cluster have high edge-betweenness, as all the shortest paths
# from one cluster to another must traverse through them. So if we
# iteratively remove the edge with the highest edge-betweenness
# score we will get a hierarchical map of the communities in the
# graph. 

# The idea of the edge betweenness based community structure detection is that it 
# is likely that edges connecting separate modules have high edge betweenness as # all the shortest paths from one module to another must traverse through them. 
# So if we gradually remove the edge with the highest edge betweenness score we 
# will get a hierarchical map, a rooted tree, called a dendrogram of the graph. The 
# leafs of the tree are the individual vertices and the root of the tree represents the 
# whole graph.
# The Dendrogram we made seems to show values on the left for the average 
# number of inter-community edges per vertex. Since the algorithm takes out the 
# most between edges first, it forms cliques and leaves fewer and fewer inter-
# community edges in place. 




# The following function will find the betweenness for each
# vertex.
friend_comm_eb <- edge.betweenness.community(m182_friend_no_iso)
friend_comm_eb

# This process also lends itself to visualization as a dendrogram.
# The y-axis reflects the distance metric used by the edge betweennes 
# algorithm; for more on this, see M Newman and M Girvan: Finding and 
# evaluating community structure in networks, Physical Review E 69, 026113
# (2004), http://arxiv.org/abs/cond-mat/0308217. 
plot(as.dendrogram(friend_comm_eb))

# Question #3 - How many clusters would you select here and why?


# The following code produces an animation of the edge-betweeness
# process. It's easiest to run all of this code at once. The 
# result is a set of .png files that will be saved to the default 
# working directory (or the working directory specified by setwd(), 
# if any). Note that the code may throw an error, but this doesn't 
# necessarily mean it didn't work; check the appropriate folder 
# for the .png files to see. Highlight all the .png files and open 
# them in Preview as a slideshow. 

# Before running this code, you need to install ImageMagick:
# http://www.imagemagick.org/script/binary-releases.php
# Scroll down to the Windows/Mac section (you probably do not want
# the Unix files at the top.) 

# *** START ANIMATION CODE ***

jitter.ani <-function(x, g){

  l <- layout.kamada.kawai(g, niter=1000)
  ebc <- edge.betweenness.community(g)

  colbar <- rainbow(6)
  colbar2 <- c(rainbow(5), rep("black",15))

  for (i in 1:x) {
    g2 <- delete.edges(g, ebc$removed.edges[seq(length=i-1)])
    eb <- edge.betweenness(g2)
    cl <- clusters(g2)$membership
    q <- modularity(g, cl)
    E(g2)$color <- "grey"
    E(g2)[ order(eb, decreasing=TRUE)[1:5]-1 ]$color <- colbar2[1:5]

    E(g2)$width <- 1
    E(g2)[ color != "grey" ]$width <- 2

    plot(g2, layout=l, vertex.size=12,
       edge.label.color="red", vertex.color=colbar[cl+2],
       edge.label.font=2)
    title(main=paste("Q=", round(q,3)), font=2)
    ty <- seq(1,by=-strheight("1")*1.5, length=20)
    text(-1.3, ty, adj=c(0,0.5), round(sort(eb, dec=TRUE)[1:20],2),
       col=colbar2, font=2)
  }
}

saveMovie(jitter.ani(20, m182_friend_no_iso), interval = 0.5, outdir = getwd())

# *** END ANIMATION CODE ***


# Now we'll decide on an optimum number of communities for this
# network. All of the community methods above allow us to iterate
# through different numbers of communities and judge how well
# correlated a given idealized community structure is with the
# observed network. 
#
# We'll start by getting an adjacency matrix based on the network 
# with isolates removed.
friend_adj_mat_no_iso <- get.adjacency(m182_friend_no_iso, binary=TRUE)
friend_adj_mat_no_iso
num_vertices = nrow(friend_adj_mat_no_iso)
num_vertices

# Next, we'll derive a set of idealized community structures 
# at each possible number of communities. We can correlate each 
# of these structures with the observed community and store 
# the correlation statistics in a vector.
#
# This code loops through each possible number of communities that 
# can be derived using the edge-betweenness method, but it could 
# readily be applied to the other clustering methods above. For each 
# number of communities, it prints out an adjacency matrix with 0s
# indicating that the row and column vertices do not share a 
# community and 1s indicating that they do.
# 
# Thus, the first iteration returns 14 communities, one for each
# vertex, and an adjacency matrix of all 0s. The last iteration 
# returns a single community comprised of all vertices, and an 
# adjacency matrix of all 1s (except on the diagonal).
#
# Meanwhile, we can correlate each community structure with the 
# observed community to generate a list of ideal-observed
# correlations.
library(sna)
ideal_observed_cors = vector()
for (i in 0:(num_vertices-1)) {
	num_comms = (num_vertices - i)
	cat('number of communities: ', num_comms, '\n')
	community <- community.to.membership(m182_friend_no_iso, friend_comm_eb$merges, i)
	print(community)
	idealized_comm_mat <- matrix(nrow=num_vertices, ncol=num_vertices)

	for (m in 1:num_vertices) {
		for (n in 1:num_vertices) {
			if (m==n) {
				idealized_comm_mat[m,n] = 0
			} else if (community$membership[m] == community$membership[n]) {
				idealized_comm_mat[m,n] = 1
			} else {
				idealized_comm_mat[m,n] = 0
			}
		}
	}
	print(idealized_comm_mat) 

	if (num_comms > 1 & num_comms < num_vertices) {
		ideal_observed_cors <- append(ideal_observed_cors, (gcor(idealized_comm_mat, friend_adj_mat_no_iso)))
		print(ideal_observed_cors[length(ideal_observed_cors)])
	} else {
		ideal_observed_cors <- append(ideal_observed_cors, 0)
		print('unable to calcuate correlation; setting value to 0')
	}
	cat('\n')
}
ideal_observed_cors <- rev(ideal_observed_cors)
ideal_observed_cors 

# We can then plot the list of correlations to determine the 
# optimum number of communities for this particular network.
plot(ideal_observed_cors)

# QUESTION #4 - Which clustering technique affords the best clique 
# solution and why? What supports your claim? Why use say 4 clusters 
# instead of 10? What evidence supports the claim for # of clusters?


### 
# 4. BLOCKMODELING
###


# Now we'll look at some community statistics across different 
# relationship networks. For all of these, we'll use the three 
# edge-betweenness communities generated above as our basic 
# social structure. We will reintroduce the two isolates we 
# removed, assigning each to his/her own "community."
#
# First, we'll set a vector of communities, ordered according 
# to the order of vertices. 
communities <- community.to.membership(m182_friend_no_iso, friend_comm_eb$merges, num_vertices-3)
communities

# Now we have to manually insert communities corresponding to the
# isolates. We can get their vertex ID numbers via:
friend_adj_mat_full <- get.adjacency(m182_friend, binary=TRUE)
which(degree(friend_adj_mat_full)==0)

# Insert the isolate communities at indices 4 and 16.
# IN ORDER TO PERFORM THE COMMUNITY DETECTION ALGORITHMS, WE HAD TO REMOVE THE ISOLATES FROM THE GRAPH (HENCE THE NO_ISO MATRICES ABOVE). NOW WE WANT TO RE-INSERT THEM, SO WE ASSIGNING EACH TO ITS OWN "COMMUNITY" (NUMBERED 3 AND 4) AND MANUALLY ADD THESE COMMUNITIES TO THE VECTOR OF COMMUNITIES IN THE APPROPRIATE PLACE. 
communities_full <- vector()
communities_full <- append(communities_full, communities$membership[1:3])
communities_full <- append(communities_full, 3)
communities_full <- append(communities_full, communities$membership[4:14])
communities_full <- append(communities_full, 4)
communities_full

# We can use this community membership vector to produce a
# blockmodel, showing the within- and between-cluster densities.
# For this we'll use the blockmodel() function in sna. Unlike 
# igraph, sna counts from 1, so the first thing we need to do 
# is renumber the IDs in the communities vector.
communities_full <- communities_full+1
communities_full

# Now we can generate the blockmodel using the adjacency matrix 
# and community vector. Each "block" corresponds to a community
# listed in the communities_full vector; the values are the 
# densities within and between communities.

friend_blockmodel <- blockmodel(friend_adj_mat_full, communities_full)
friend_blockmodel

# Compare the density values from the blockmodel to the overall 
# density of the network.
graph.density(m182_friend)

# Now we can repeat the blockmodel process to get within- and
# between-cluster densities for the social-interaction network,
# using the communities generated with the friends network for our 
# clusters. 
#
# For this data set, we want to retain the tie values.
social_adj_mat <- get.adjacency(m182_social, attr='social_tie')
social_adj_mat
social_blockmodel <- blockmodel(social_adj_mat, communities_full)
social_blockmodel

# Because we retatined tie values above, we should compare the 
# blockmodel values not to the graph's overall density but to its
# overall mean.
social_mean <- mean(social_adj_mat)
social_mean

# We can repeat one more time for the task-interaction network,
# once again retaining valued ties.
task_adj_mat <- get.adjacency(m182_task, attr='task_tie')
task_adj_mat
task_blockmodel <- blockmodel(task_adj_mat, communities_full)
task_blockmodel

task_mean <- mean(task_adj_mat)
task_mean

# QUESTION #5 - Using the average density for each type of tie as 
# an alpha-cutoff level, answer the following: (a) How do friendship 
# cliques shape patterns of task and social interaction? (b) Are task
# or social interactions following clique boundaries? Why or why not 
# might this happen? 


###
# 5. K-CORES
###

# The graph.coreness() function in igraph returns a vector containing 
# the degree of the highest-degree k-core to which each vertex 
# belongs. 
coreness = graph.coreness(m182_task)
coreness

# Note that the output of graph.coreness refers simply to the *degree*
# of the k-core, not to the k-core itself; thus, two vertices both with 
# coreness of 3 may not be connected at all and thus may be in separate
# k-cores. 
#
# One way to get a sense of the actual k-core structure is to simply 
# plot the graph and color-code by k-core:
make_k_core_plot <- function (g) {
	lay1 <- layout.fruchterman.reingold(g)
	plot(g, 
	     vertex.color = graph.coreness(g), 
	     layout=lay1, 
	     edge.arrow.size = .5)
} 

make_k_core_plot(m182_friend)
make_k_core_plot(m182_social)
make_k_core_plot(m182_task)

# Here's an artificial example showing two separate 3-cores:
g1 <- graph.ring(10)
g1 <- add.edges(g1, c(0,2, 1,3, 0,3, 5,7, 5,8, 6,8))
make_k_core_plot(g1)

# Question #6 - What's the difference between K-cores and cliques? 
# Why might one want to use one over the other?
 

###
# EXTRA CREDIT:
# igraph's built-in functionality merely shows the the degree of the
# highest-degree k-core to which each vertext belongs. However, 
# vertices with the same coreness may be part of different k-cores,
# as shown in the artifical 3-core example above.
#
# Can you figure out a way to to generate not only the coreness of 
# each vertex, but also an indicator of which specific k-core the 
# vertex belongs to (and thus which other vertices are in the same 
# k-core)?
###