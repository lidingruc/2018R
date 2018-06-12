##########################################################################
# You may cite these labs as follows: McFarland, Daniel, Solomon Messing,
# Mike Nowak, and Sean Westwood. 2010. "Social Network Analysis          
# Labs in R." Stanford University.                                       
##########################################################################
 
 
##########################################################################
# LAB 6 - Blockmodeling Lab                                              
# The point of this lab is to introduce students to blockmodeling 
# techniques that call for a metric of structural equivalence, a method
# and rationale for the selection of the number of positions, and then 
# a means of summary representation (mean cutoff and reduced graph 
# presentation). Students will be shown how to identify positions using
# correlation as a metric of structural equivalence (euclidean distance 
# is used in earlier lab), and they will be taught how to identify more
# isomorphic notions of role-position using the triad census. Last, the 
# lab calls upon the user to compare positional techniques and come up 
# with a rationale for why they settle on one over another.  
##########################################################################
 



# NOTE: if you have trouble because some packages are not installed, 
# see lab 1 for instructions on how to install all necessary packages.

###
#1. SETUP
###

library(igraph)
library(sna)
library(triads)
library(psych)
library(nFactors)
library(NetCluster)

###
#2. LOADING AND FORMATTING DATA
###

data(studentnets.M182, package = "NetData")

# Reduce to non-zero edges and build a graph object
m182_full_nonzero_edges <- subset(m182_full_data_frame, (friend_tie > 0 | social_tie > 0 | task_tie > 0))
head(m182_full_nonzero_edges)

m182_full <- graph.data.frame(m182_full_nonzero_edges) 
summary(m182_full)

# Create sub-graphs based on edge attributes
m182_friend <- delete.edges(m182_full, E(m182_full)[E(m182_full)$friend_tie==0])
summary(m182_friend)

m182_social <- delete.edges(m182_full, E(m182_full)[E(m182_full)$social_tie==0])
summary(m182_social)

m182_task <- delete.edges(m182_full, E(m182_full)[E(m182_full)$task_tie==0])
summary(m182_task)

# Look at the plots for each sub-graph
pdf("6.1_m182_studentnet_friend_social_task_plots.pdf", width = 10)
par(mfrow = c(1,3))

friend_layout <- layout.fruchterman.reingold(m182_friend)
plot(m182_friend, layout=friend_layout, main = "friend", edge.arrow.size=.5)

social_layout <- layout.fruchterman.reingold(m182_social)
plot(m182_social, layout=social_layout, main = "social", edge.arrow.size=.5)

task_layout <- layout.fruchterman.reingold(m182_task)
plot(m182_task, layout=task_layout, main = "task", edge.arrow.size=.5)
dev.off()

###
# 3. HIERARCHICAL CLUSTERING ON SOCIAL & TASK TIES
###

# We'll use the "task" and "social" sub-graphs together as the
# basis for our structural equivalence methods. First, we'll use
# the task graph to generate an adjacency matrix.
#
# This matrix represents task interactions directed FROM the 
# row individual TO the column individual. 
m182_task_matrix_row_to_col <- get.adjacency(m182_task, attr='task_tie')
m182_task_matrix_row_to_col

# To operate on a binary graph, simply leave off the "attr" 
# parameter:
m182_task_matrix_row_to_col_bin <- get.adjacency(m182_task)
m182_task_matrix_row_to_col_bin

# For this lab, we'll use the valued graph. The next step is to 
# concatenate it with its transpose in order to capture both 
# incoming and outgoing task interactions.
m182_task_matrix_col_to_row <- t(m182_task_matrix_row_to_col)
m182_task_matrix_col_to_row

m182_task_matrix <- rbind(m182_task_matrix_row_to_col, m182_task_matrix_col_to_row)
m182_task_matrix

# Next, we'll use the same procedure to add social-interaction
# information.
m182_social_matrix_row_to_col <- get.adjacency(m182_social, attr='social_tie')
m182_social_matrix_row_to_col

m182_social_matrix_row_to_col_bin <- get.adjacency(m182_social)
m182_social_matrix_row_to_col_bin

m182_social_matrix_col_to_row <- t(m182_social_matrix_row_to_col)
m182_social_matrix_col_to_row

m182_social_matrix <- rbind(m182_social_matrix_row_to_col, m182_social_matrix_col_to_row)
m182_social_matrix

m182_task_social_matrix <- rbind(m182_task_matrix, m182_social_matrix)
m182_task_social_matrix

# Now we have a single 4n x n matrix that represents both in- and
# out-directed task and social communication. From this, we can
# generate an n x n correlation matrix that shows the degree of
# structural equivalence of each actor in the network. 
m182_task_social_cors <- cor(m182_task_social_matrix)
m182_task_social_cors

# To use correlation values in hierarchical NetCluster, they must 
# first be coerced into a "dissimilarity structure" using dist().
# We subtract the values from 1 so that they are all greater than 
# or equal to 0; thus, highly dissimilar (i.e., negatively 
# correlated) actors have higher values.
dissimilarity <- 1 - m182_task_social_cors
m182_task_social_dist <- as.dist(dissimilarity)
m182_task_social_dist

# Note that it is also possible to use dist() directly on the 
# matrix. However, since cor() looks at associations between 
# columns and dist() looks at associations between rows, it is
# necessary to transpose the matrix first.
#
# A variety of distance metrics are available; Euclidean 
# is the default.
#m182_task_social_dist <- dist(t(m182_task_social_matrix))
#m182_task_social_dist

# hclust() performs a hierarchical agglomerative NetCluster 
# operation based on the values in the dissimilarity matrix 
# yielded by as.dist() above. The standard visualization is a 
# dendrogram. By default, hclust() agglomerates clusters via a
# "complete linkakage" algorithm, determining cluster proximity
# by looking at the distance of the two points across clusters
# that are farthest away from one another. This can be changed via
# the "method" parameter.

pdf("6.2_m182_studentnet_social_hclust.pdf")
m182_task_social_hclust <- hclust(m182_task_social_dist)
plot(m182_task_social_hclust)
dev.off()

# cutree() allows us to use the output of hclust() to set
# different numbers of clusters and assign vertices to clusters
# as appropriate. For example:
cutree(m182_task_social_hclust, k=2)

# Now we'll try to figure out the number of clusters that best 
# describes the underlying data. To do this, we'll loop through
# all of the possible numbers of clusters (1 through n, where n is
# the number of actors in the network). For each solution
# corresponding to a given number of clusters, we'll use cutree()
# to assign the vertices to their respective clusters 
# corresponding to that solution.
#
# From this, we can generate a matrix of within- and between-
# cluster correlations. Thus, when there is one cluster for each 
# vertex in the network, the cell values will be identical to the
# observed correlation matrix, and when there is one cluster for 
# the whole network, the values will all be equal to the average
# correlation across the observed matrix.
#
# We can then correlate each by-cluster matrix with the observed
# correlation matrix to see how well the by-cluster matrix fits
# the data. We'll store the correlation for each number of
# clusters in a vector, which we can then plot.

# First, we initialize a vector for storing the correlations and 
# set a variable for our number of vertices.
clustered_observed_cors = vector()
num_vertices = length(V(m182_task))

# Next, we loop through the different possible cluster 
# configurations, produce matrices of within- and between-
# cluster correlations, and correlate these by-cluster matrices
# with the observed correlation matrix.

pdf("6.3_m182_studentnet_task_social_clustered_observed_corrs.pdf")
clustered_observed_cors <-clustConfigurations(num_vertices,m182_task_social_hclust,m182_task_social_cors)
clustered_observed_cors
plot(clustered_observed_cors$correlations)
dev.off()

clustered_observed_cors$correlations
# From a visual inspection of the correlation matrix, we can 
# decide on the proper number of clusters in this network. 
# For this network, we'll use 4. (Note that the 1-cluster 
# solution doesn't appear on the plot because its correlation 
# with the observed correlation matrix is undefined.)
num_clusters = 4
clusters <- cutree(m182_task_social_hclust, k = num_clusters)
clusters

cluster_cor_mat <- clusterCorr(m182_task_social_cors,
                                            clusters)
cluster_cor_mat

# Let's look at the correlation between this cluster configuration 
# and the observed correlation matrix. This should match the 
# corresponding value from clustered_observed_cors above.
gcor(cluster_cor_mat, m182_task_social_cors)


#####################
# Questions:
# (1) What rationale do you have for selecting the number of 
# clusters / positions that you do?
#####################
  


### NOTE ON DEDUCTIVE CLUSTERING

# It's pretty straightforward, using the code above, to explore
# your own deductive NetCluster. Simply supply your own cluster
# vector, where the elements in the vector are in the same order
# as the vertices in the matrix, and the values represent the
# cluster to which each vertex belongs. 
#
# For example, if you believed that actors 2, 7, and 8 formed one
# group, actor 16 former another group, and everyone else formed 
# a third group, you could represent this as follows:
deductive_clusters = c(1, 2, 1, 1, 1, 1, 2, 2, 1, 1, 1, 1, 1, 1,
                       1, 3)

# You could then examine the fitness of this cluster configuration
# as follows:
deductive_cluster_cor_mat <- generate_cluster_cor_mat(
  m182_task_social_cors,
  deductive_clusters)
deductive_cluster_cor_mat
gcor(deductive_cluster_cor_mat, m182_task_social_cors)

### END NOTE ON DEDUCTIVE CLUSTERING

# Now we'll use the 4-cluster solution to generate blockmodels, 
# using the raw tie data from the underlying task and social 
# networks.

# Task valued
task_mean <- mean(m182_task_matrix_row_to_col)
task_mean

task_valued_blockmodel <- blockmodel(m182_task_matrix_row_to_col, clusters)
task_valued_blockmodel

# Task binary
task_density <- graph.density(m182_task)
task_density

task_binary_blockmodel <- blockmodel(m182_task_matrix_row_to_col_bin, clusters)
task_binary_blockmodel


# Social valued
social_mean <- mean(m182_social_matrix_row_to_col)
social_mean

social_valued_blockmodel <- blockmodel(m182_social_matrix_row_to_col, clusters)
social_valued_blockmodel

# Social binary
social_density <- graph.density(m182_social)
social_density

social_binary_blockmodel <- blockmodel(m182_social_matrix_row_to_col_bin, clusters)
social_binary_blockmodel

# We can also permute the network to examine the within- and 
# between-cluster correlations. 

cluster_cor_mat_per <- permute_matrix(clusters, cluster_cor_mat)
cluster_cor_mat_per


#####################
# Questions:
# (2) What is the story you get from viewing these clusters, 
# and their within and between cluster densities on task and 
# social interaction? What can you say about M182 from this?
#####################


###
# 4. HIERARCHICAL CLUSTERING ON TRIAD CENSUS
###

# Another way to think about roles within a network is by looking 
# at the triads that each actor belongs to. We can then use 
# correlations between triad-type memberships to identify people 
# with similar roles regardless of the specific people with whom
# they interact.

# First, we'll generate an individual-level triad census of the 
# network using triadcensus() from the triads package.
task_triads <- triadcensus(m182_task)
task_triads

# Next, we'll generate a matrix of correlations between actors 
# in the network based on their similarity in triad-type 
# membership. Note that the cor() function in R operates on 
# columns, not rows, so in order to get correlations between 
# the actors in the network we have to transpose it.
m182_task_triad_cors <- cor(t(task_triads))
m182_task_triad_cors

# As above, we can use the correlation matrix to generate a 
# dissimilarity structure, which we can then hierarchically
# cluster into groups of similar people.
dissimilarity <- 1 - m182_task_triad_cors
m182_task_triad_dist <- as.dist(dissimilarity)
m182_task_triad_dist

m182_task_triad_hclust <- hclust(m182_task_triad_dist)

pdf("6.4_m182_studentnet_task_triad_hclust.pdf")
plot(m182_task_triad_hclust)
dev.off()

# As above, we'll loop through each possible cluster solution 
# and see how well they match the observed matrix of triad-type 
# correlations.
clustered_observed_cors = vector()
num_vertices = length(V(m182_task))

pdf("6.5_m182_studentnet_task_hclust_triad_corrs.pdf")
clustered_observed_cors <-clustConfigurations(num_vertices,m182_task_triad_hclust,m182_task_triad_cors)
dev.off()

clustered_observed_cors 


# From a visual inspection of the data, we'll use a 3-cluster 
# solution (though a case could also be made for using 5.)
num_clusters = 3
clusters <- cutree(m182_task_triad_hclust, k = num_clusters)
clusters

cluster_cor_mat <- clusterCorr (m182_task_triad_cors,
		clusters)

cluster_cor_mat
gcor(cluster_cor_mat, m182_task_triad_cors)


# As before, we can use these clusters to run a blockmodel
# analysis using the underlying tie data from the task network.

# Task valued
task_mean <- mean(m182_task_matrix_row_to_col)
task_mean

task_valued_blockmodel <- blockmodel(m182_task_matrix_row_to_col, clusters)
task_valued_blockmodel

# Task binary
task_density <- graph.density(m182_task)
task_density

task_binary_blockmodel <- blockmodel(m182_task_matrix_row_to_col_bin, clusters)
task_binary_blockmodel


# Finally, we can try to get a sense of what our different
# clusters represent by generating a cluster-by-triad-type matrix.
# This is an m x n matrix, where m is the number of clusters and n 
# is the 36 possible triad types. Each cell is the average 
# number of the given triad type for each individual in the 
# cluster. 
cluster_triad_mat <- matrix(nrow=max(clusters), ncol=ncol(task_triads))
for (i in 1:max(clusters)) {
	for (j in 1:ncol(task_triads)) {
		cluster_triad_mat[i,j] <- mean(task_triads[which(clusters==i),j])
	}
}

cluster_triad_mat


#####################
# Questions:
# (3) What does clustering of the triadic census afford us? 
# What roles do you see? Redo the initial blockmodel analysis
# without social interaction (only task) and then compare to 
# this solution. Do they differ? 
#
# Extra credit: Try running the triad census on task AND 
# social interaction separately and then correlating persons. 
# What result do you get? Is it different from our initial 
# blockmodel result? Show your code.  
######################



###
# 5. FACTOR ANALYSIS
###

# Note that although we are conducting a principal components
# analysis (PCA), which is technically not exactly the same as
# factor analysis, we will use the term "factor" to describe the
# individual components in our PCA. 

# PCA is often used in network analysis as a form of detecting 
# individuals global positioning. We say "global" because these
# clusters aren't defined on local cohesion but from the overall 
# pattern of ties individuals have with all others (structural 
# equivalence). Identifying the first two largest components that
# organize the variance in tie patterns is one way of doing this.

# We'll analyze the 4n x n matrix generated above.

# First, we want to determine the ideal number of components
# (factors) to extract. We'll do this by examining the eigenvalues
# in a scree plot and examining how each number of factors stacks
# up to a few proposed non-graphical solutions to selecting the
# optimal number of components, available via the nFactors
# package. 
ev <- eigen(cor(m182_task_social_matrix)) # get eigenvalues
ap <- parallel(subject=nrow(m182_task_social_matrix),
		var=ncol(m182_task_social_matrix),
		rep=100,cent=.05)
nS <- nScree(ev$values, ap$eigen$qevpea)

pdf("6.6_m182_studentnet_task_social_pca_scree.pdf")
plotnScree(nS) 

# To draw a line across the graph where eigenvalues are = 1,
# use the following code:
plotnScree(nS) 
abline(h=1)
dev.off()


# For more information on this procedure, please see 
# the references provided in the parallel() documentation
# (type "?parallel" in the R command line with the package
# loaded).

# Now we'll run a principal components analysis on the matrix,
# using the number of factors determined above (note this may not
# be the same number as you get):
pca_m182_task_social = principal(m182_task_social_matrix, nfactors=5, rotate="varimax") 

# Let's take a look at the results in the R terminal:
pca_m182_task_social 

# You can see the standardized loadings for each factor for each
# node. Note that R sometimes puts the factors in a funky order
# (e.g. RC1, RC2, RC5, RC4, RC3) but all of the factors are there.
# You can see that the SS loadings, proportion of variance
# explained and cumulative variance explained is provided below. A
# Chi Square test of the factors and various other statistics are
# provided below. 

# Note that the eigenvalues can be accessed via the following
# command:
pca_m182_task_social$values

# Now we will use the factor loadings to cluster and compare that
# to our other NetCluster techniques, using dendrograms.

# Take the distance based on Euclidian Distance
m182_task_factor_dist = dist(pca_m182_task_social$loadings)

# And cluster
m182_task_factor_hclust <- hclust(m182_task_factor_dist)

pdf("6.7_m182_studentnet_task_social_pca_hclust.pdf")
plot(m182_task_factor_hclust)
dev.off()

# And compare to NetCluster based on correlations and triads:
pdf("6.8_m182_task_cluster_by_correlation_PCA_Triads.pdf")
par(mfrow = c(1,3))
plot(m182_task_social_hclust, main = "Correlation")
plot(m182_task_factor_hclust, main = "PCA")
plot(m182_task_triad_hclust, main = "Triads")
dev.off()



#####################
# Questions:
# (4) How do the results across blockmodel techniques differ? 
# Why might you use one over the other? Why might you want to 
# run more than one in your analyses?
#####################