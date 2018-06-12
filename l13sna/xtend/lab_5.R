################################################
# LAB 5: Two-Mode Networks and Mobility Models #
################################################


# NOTE: if you have trouble because some packages are not installed, 
# see lab 1 for instructions on how to install all necessary packages.

#################################################################### Lab 5## This lab covers affiliation data. It shows the user how to plot# affiliation data using the igraph package, how to transform it# into one mode data and generate centrality measures.# This lab also introduces additional material on plotting network# data, with special attention to two-mode networks. It also# covers using opacity/transparency in network plots, hand-placing# nodes in a network layout (in case labels overlap), and various# other considerations that may be of interest when creating# network visualizations in your publications.## It then covers network mobility. Specifically, it covers how to # compute transition probability matrices and then instructs the# user to create visualizations similar to those generated for the # affiliation data.## We'll work with affiliation data collected by Dan McFarland # on student extracurricular affiliations (CITE). It's a# longitudinal data set, with 3 waves - 1996, 1997, 1998.  It# consists of students (anonymized) and the student organizations# in which they are members (e.g. National Honor Society,# wrestling team, cheerleading squad, etc.).  ##################################################################

# To-do
# (A) need to add 3 affiliations: 
#	1.	"entry" affiliations for students: for any kid younger than 8th grade (for Magnet) or 9th grade (for Rural) in any particular year, this would be their affiliation that year. 
#	2.	"leave" affiliations for students: for any kid older than 12th grade in any particular year, this would be their affiliation. 
#	3.	"non-participant": for any student who is not in a leave or entry position and who has a row sum of 0. 
# (B) Using my table of affiliation characteristics, we can omit certain fluff clubs like pep club, NHS, foreign language clubs, etc from the graph as they drive a lot of non-sensical patterns. 
# Load the "igraph" librarylibrary(igraph) # (1) Read in the data files, NA data objects coded as "na"magact96 = read.delim("http://sna.stanford.edu/sna_R_labs/data/mag_act96.txt", na.strings = "na", check.names = FALSE)magact97 = read.delim("http://sna.stanford.edu/sna_R_labs/data/mag_act97.txt", na.strings = "na", check.names = FALSE)magact98 = read.delim("http://sna.stanford.edu/sna_R_labs/data/mag_act98.txt", na.strings = "na", check.names = FALSE)# Missing data is coded as "na" in this data, which is why we gave# R the command na.strings = "na". We need to preserve the# original column names for labeling our visualizations so we use# the "check.names = FALSE" argument as well. If we needed to# access our data using the "magact96$variable" syntax, we would# NOT want to read in our data like this.# These files consist of four columns of individual-level# attributes (ID, gender, grade, race), then a bunch of group# membership dummy variables (coded "1" for membership, "0" for no # membership).  We need to set aside the first four columns (which# do not change from year to year). 

# take a look at the data as follows (you can edit or "fix" the data
# using this perspective if necessary):
fix(magact96)
################################################################# (2) Create the attribute data. Attributes appear the same# from 1996-1998magattrib = magact96[,1:4]################################################################# (3) Drop columns so we have a square incidence matrix for each# yearg96 = as.matrix(magact96[,-(1:4)]); row.names(g96) = magact96[,1]g97 = as.matrix(magact97[,-(1:4)]); row.names(g97) = magact97[,1]g98 = as.matrix(magact98[,-(1:4)]); row.names(g98) = magact98[,1]# For future reference, you can access these via# data(studentnets.magact96.97.98, package = "NetData")# Now load in these two-mode graphs into igraph.i96 <- graph.incidence(g96, mode=c("all") )i97 <- graph.incidence(g97, mode=c("all") )i98 <- graph.incidence(g98, mode=c("all") )################################################################# (3a) Run some plots:## Now, let's plot these graphs. The igraph package has excellent# plotting functionality that allows you to assign visual# attributes to igraph objects before you plot. The alternative is# to pass 20 or so arguments to the plot.igraph() function, which# gets really messy.# In past labs, you may have accessed vertex (node attributes via# this function: get.edge.attribute(krack_full, 'reports_to_tie')# A "shorthand" to access edge or vertex (node) attributes works# similarly to R's handling of lists and dataframes (e.g.,# 'dataframe$variablename' or'list$sublist$object').# Each node (or "vertex") object is accessible by calling V(g),# and you can call (or create) a node attribute by using the $# operator so that you call V(g)$attribute. # Let's now use this notation to set the vertex color, with# special attention to making graph objects slightly transparent.# We'll use the rgb function in R to do this. We specify the# levels of Red, Green, and Blue and "alpha-channel" (a.k.a.# opacity) using this syntax: rgb(red, green, blue, alpha). To# return solid red, one would use this call: rgb(red = 1,green =# 0,blue = 0, alpha = 1). We will make nodes, edges, and labels# slightly transparent so that when things overlap it is still# possible to read them.# You can read up on the RGB color model at # http://en.wikipedia.org/wiki/RGB_color_model.# Here's how to set the color attribute for a set of nodes in a# graph object:V(i96)$color[1:1295] = rgb(red = 1, green = 0, blue = 0, alpha = .5)V(i96)$color[1296:1386] = rgb(red = 0, green = 1, blue = 0, alpha = .5)# Notice that we index the V(g)$color object by a seemingly# arbitrary value, 1295.  This marks the end of the student nodes,# and 1296 is the first group node. You can view which nodes are# which by typing V(i96). R prints out a list of all the nodes in# the graph, and those with an id number can be distinguished from# group names. # From here on out, we do not specify "red = ", "green = ", "blue# = ", and "alpha = ". These are the default arguments (R knows# the first number corresponds to red, the second to blue, and so# on).# Now we'll set some other graph attributes:V(i96)$label = V(i96)$nameV(i96)$label.color = rgb(0,0,.2,.5)V(i96)$label.cex = .4V(i96)$size = 6V(i96)$frame.color = V(i96)$color# You can also set edge attributes. Here we'll make the edges# nearly transparent and slightly yellow because there will be so# many edges in this graph:E(i96)$color = rgb(.5,.5,0,.2)# Now, we'll open a pdf "device" on which to plot. This is just a# connection to a pdf file. Note that the code below will take a# minute or two to execute (or longer if you have a pre- Intel# dual-core processor), because the graph is so large.pdf("5.1_magact_stdnt_actvts_1996.pdf")plot(i96, layout=layout.fruchterman.reingold)dev.off()# Note that we've used the Fruchterman-Reingold force-directed# layout algorithm here. Generally speaking, when you have a# ton of edges, the Kamada-Kawai layout algorithm works well but,# it can get really slow for networks with a lot of nodes. Also,# for larger networks, layout.fruchterman.reingold.grid is faster,# but can fail to produce a plot with any meaningful pattern if you# have too many isolates, as is the case here. Experiment for# yourself. # Now, if you open the pdf output, you'll notice that you can zoom# in on any part of the graph ad infinitum without losing any# resolution. How is that possible in such a small file? It's# possible because the pdf device output consists of data based on# vectors: lines, polygons, circles, ellipses, etc., each specified# by a mathematical formula that your pdf program renders when you# view it. Regular bitmap or jpeg picture output, on the other# hand, consists of a pixel-coordinate mapping of the image in# question, which is why you lose resolution when you zoom in on a# digital photograph or a plot produced with most other programs.# This plot is oddly reminiscent of a crescent and star, but# impossible to read. Part of the problem is the way in which# layout algorithms deal with isolates. For example,# layout.fruchterman.reingold will squish all of the connected# nodes into the center creating a useless "hairball"-like# visualization. The same applies to# layout.fruchterman.reingold.grid (it's even worse). And# layout.kamada.kawai takes exponentially longer to converge (it# may not ever converge in igraph's implementation of the# algorithm with isolates).# Let's remove all of the isolates (the crescent), change a few# aesthetic features, and replot. First, we'll remove isolates, by# deleting all nodes with a degree of 0, meaning that they have# zero edges. Then, we'll suppress labels for students and make# their nodes smaller and more transparent. Then we'll make the# edges more narrow more transparent. Then, we'll replot using # various layout algorithms:i96 = delete.vertices(i96, V(i96)[ degree(i96)==0 ])V(i96)$label[1:857] = NAV(i96)$color[1:857] =  rgb(1,0,0,.1)V(i96)$size[1:857] = 2 E(i96)$width = .3E(i96)$color = rgb(.5,.5,0,.1)pdf("5.2_magact_stdnt_actvts_1996_layout.kamada.kawai.pdf")plot(i96, layout=layout.kamada.kawai)dev.off()pdf("5.3_magact_stdnt_actvts_1996_layout.fruchterman.reingold.grid.pdf")plot(i96, layout=layout.fruchterman.reingold.grid)dev.off()pdf("5.4_magact_stdnt_actvts_1996_layout.fruchterman.reingold.pdf")plot(i96, layout=layout.fruchterman.reingold)dev.off()# The nice thing about the Fruchterman-Reingold layout in this # case is that it really emphasizes centrality -- the nodes that# are most central are nearly always placed in the middle of the # plot. # Now repeat for other years (we do not delete the vertices in# these years.# 1997i97 = delete.vertices(i97, V(i97)[ degree(i97)==0 ])V(i97)$color = rgb(0, 1, 0, .5)V(i97)$color[1:752] =  rgb(1,0,0,.1)V(i97)$frame.color = V(i97)$colorV(i97)$size = 6V(i97)$size[1:752] = 2 V(i97)$label = V(i97)$nameV(i97)$label.color = rgb(0,0,.2,.5)V(i97)$label.cex = .4V(i97)$label[1:752] = NAE(i97)$width = .3E(i97)$color = rgb(.5,.5,0,.2)pdf("5.5_magact_stdnt_actvts_1997.pdf")
plot(i97, layout=layout.fruchterman.reingold)dev.off()# 1998i98 = delete.vertices(i98, V(i98)[ degree(i98)==0 ])V(i98)$color = rgb(0, 1, 0, .5)V(i98)$color[1:724] =  rgb(1,0,0,.1)V(i98)$frame.color = V(i98)$colorV(i98)$size = 6V(i98)$size[1:724] = 2 V(i98)$label = V(i98)$nameV(i98)$label.color = rgb(0,0,.2,.5)V(i98)$label.cex = .4V(i98)$label[1:724] = NAE(i98)$width = .3E(i98)$color = rgb(.5,.5,0,.2)pdf("5.6_magact_stdnt_actvts_1998.pdf")
plot(i98, layout=layout.fruchterman.reingold)dev.off()################################################################# (4) Now produce single mode co-event matrices for each year. This# is done using R's matrix algebra commands. You first need to get# your data in matrix format. We already have a matrix representation# of our data, but if you did not, a network object can be coerced via# as.matrix(your-network) if you are using the network or sna# packages; with the igraph package you would use get.adjacency(your# network). # To get the one-mode representation of ties between rows (people in # our example), multiply the matrix by its transpose. To get the  # one-mode representation of ties between columns (clubs in our# example), multiply the transpose of the matrix by the matrix.  # Note that you must use the matrix-multiplication operator %*% # rather than a simple asterisk. The R code is for our data follows:g96e = t(g96) %*% g96g97e = t(g97) %*% g97g98e = t(g98) %*% g98i96e = graph.adjacency(g96e, mode = "undirected")# Now we need to transform the graph so that multiple edges become an# attribute of each unique edge, accessible via E(g)$weight:E(i96e)$weight <- count.multiple(i96e)i96e <- simplify(i96e)# Now plot the first single mode co-event matrices. Set vertex# attributes, making sure to make them slightly transparent by# altering the gamma via the rgb(r,g,b,gamma) function.# Set vertex attributesV(i96e)$label = V(i96e)$nameV(i96e)$label.color = rgb(0,0,.2,.8)V(i96e)$label.cex = .6V(i96e)$size = 6
V(i96e)$color = rgb(0,0,1,.5)
V(i96e)$frame.color = V(i96e)$color# We set edge opacity/transparency as a function of how many students# each group has in common (the weight of the edge that connects the# two groups). # In order to do so, we need to transform the edge weights so that # they are between about .05 and 1, otherwise they will not show up on # the plot. # We use the log function + .3 to make sure all transparencies are # on relatively the same scale, then divide by the maximum edge weight# to get them on a scale from about .2 and 1.egalpha = (log(E(i96e)$weight)+.3)/max(log(E(i96e)$weight)+.3)E(i96e)$color = rgb(.5,.5,0,egalpha)# For illustrative purposes, let's compare how the Kamada-Kawai and# Fruchterman-Reingold algorithms render this graph:
pdf("5.7_magact_stdnt_actvts_1996_clubs.pdf")
plot(i96e, main = "layout.kamada.kawai", layout=layout.kamada.kawai)plot(i96e, main = "layout.fruchterman.reingold", layout=layout.fruchterman.reingold) dev.off()# Be sure to go tot the second page in the pdf to see the FR layout. 
# You might like the Kamada-Kawai layout for this graph, because the# center of the graph is very busy if you use the Fruchterman-Reingold# layout.################################################################# (5) and (6) Group Overlap Networks and Plots# We are also interested in the percent overlap between groups. # Note that this will be a directed graph, because the percent overlap# will not be symmetric across groups--for example, it may be that 3/4# of Spanish NHS members are in NHS, but only 1/8 of NHS members are# in the Spanish NHS. We'll create this graph for all years in our# data (though we could do it for one year only).		# First we'll create a percent overlap graph. We start by dividing # each row by the diagonal (this is really easy in R):ol96 = g96e/diag(g96e)ol97 = g97e/diag(g97e)ol98 = g98e/diag(g98e)################################################################# (7) Next, we'll sum the matrices and set any NA cells (caused by # dividing by zero in the step above) to zero:magall = ol96 + ol97 + ol98magall[is.na(magall)] = 0# Note that magall now consists of a percent overlap matrix, but# because we've summed over 3 years, the maximun is now 3 instead of# 1. ################################################################# 7 (a) compute average club size, by taking the mean across each # value in each diagonalmagdiag = apply(cbind(diag(g96e), diag(g97e), diag(g98e)), 1, mean ) ################################################################# 7 (a) i. Generate centrality for magall# First we'll want to make an igraph object:magallg = graph.adjacency(magall, weighted=T)# We need to set weighted=T because otherwise igraph dichotomizes# edges at 1. Note also that this graph is directed, so we no longer # tell igraph that the graph is "undirected." The graph is directed # because the percent overlap will not be symmetric across groups -  # for example, it may be that 3/4 of Spanish NHS members are in NHS,  # but only 1/8 of NHS members are in the Spanish NHS.# Now we'll calculate some node-level centrality measures:# Degree V(magallg)$degree = degree(magallg)# Betweenness centralityV(magallg)$btwcnt = betweenness(magallg)################################################################# 7 (b) plot magall, for relationships > 1# Before we plot this network, we should probably filter some of the# edges, otherwise our graph will probably be too busy to make sense# of visually. Take a look at the distribution of connection strength# by plotting the density of the magall matrix:
pdf("5.8_magact_stdnt_actvts_club_overlap_density.pdf")
plot(density(magall))dev.off()
# Nearly all of the edge weights are below 1 -- or in other words, the # percent overlap for most clubs is less than 1/3. Let's filter at 1, # so that an edge will consists of group overlap of more than 1/3 of # the group's members in question.magallgt1 = magallmagallgt1[magallgt1<1] = 0magallggt1 = graph.adjacency(magallgt1, weighted=T)# Removes loops:magallggt1 <- simplify(magallggt1, remove.multiple=FALSE, remove.loops=TRUE)# Degree V(magallggt1)$degree = degree(magallggt1)# Betweenness centralityV(magallggt1)$btwcnt = betweenness(magallggt1)# Before we do anything else, we'll create a custom layout based on# Fruchterman-Ringold wherein we adjust the coordates by hand using# the tkplot gui tool to make sure all of the labels are visible. This# is very useful if you want to create a really sharp-looking network# visualization for publication.magallggt1$layout = layout.fruchterman.reingold(magallggt1)V(magallggt1)$label = V(magallggt1)$nametkplot(magallggt1)# Let the plot load, then maximize the window, and select to View -># Fit to Screen so that you get maximum resolution for this large# graph. Now hand-place the nodes, making sure no labels overlap.

# Pay special attention to whether the labels overlap (or might# overlap if the font was bigger) along the vertical. Save the layout# coordinates to the graph object:magallggt1$layout = tkplot.getcoords(1)# We use "1" here only if this was the first tkplot object you# called. If you called tkplot a few times, use the last plot object. # You can tell which object is visible because at the top of the# tkplot interface, you'll see something like "Graph plot 1".# Set vertex attributesV(magallggt1)$label = V(magallggt1)$nameV(magallggt1)$label.color = rgb(0,0,.2,.6)V(magallggt1)$size = 6V(magallggt1)$color = rgb(0,0,1,.5)V(magallggt1)$frame.color = V(magallggt1)$color# Set edge attributesE(magallggt1)$arrow.size = .3# Set edge alpha according to edge weightegalpha = (E(magallggt1)$weight+.1)/max(E(magallggt1)$weight+.1)E(magallggt1)$color = rgb(.5,.5,0,egalpha)# One thing that we can do with this graph is to set label size as a# function of degree, which adds a "tag-cloud"-like element to the# visualization:V(magallggt1)$label.cex = V(magallggt1)$degree/(max(V(magallggt1)$degree)/2)+ .3# Note, as presently coded, you must play with the formula above to# get the ratio of big text to small text just right for other graphs.# You may want to hand place the nodes as we did earlier using tkplot.#Now plot it:pdf("5.9_magact_stdnt_actvts_club_overlap_handplaced_layout.pdf")
plot(magallggt1)dev.off()# This plots our network with our custom layout. (Because we specified # our layout so that it was part of our igraph object as# "magallggt1$layout," igraph used our layout by default).################################################################# 7 (c) Play around with the visualizations above, save the best# Variations might include scaling vertex size based on degree,# scaling edge width or edge transperancy (gamma) based on # edge weight.################################################################# (8) Play around with cohesion and centrality measurements. # What can we say about the general comemberships at Magnet # High over 3 years?# useful code:plot(density(degree(i96e)))plot(density(betweenness(i96e)))degree.distribution(i96e)############################################################
###########################################################
# Part II: Mobility and Careers:
###########################################################
 
# (1) create a new matrix that multiplies 1996 magnet with 1997
# magnet so you see the number of students moving from 1996
# membership to 1997 memberships.
 
# Before we actually do this, we need to make sure that the
# rows and columns for g96 and g97 are the same. We'll
# use the match() function for this:
 
# First, let's get an idea of how many column-names (activities) and row
# names (student ids) are in common between the two years:
 
(cnames = intersect( colnames(g96), colnames(g97) ) )
(rnames = intersect( row.names(g96), row.names(g97) ) )
 
# Great, there are a lot of names in common. Now we
# need to make sure we are only using the rows
# and columns of each matrix that contain entries used in
# both years. We also need to make sure that the columns and
# rows are in the same order.
 
# In order to accomplish this we are going to exploit R's
# indexing capabilities. We are going to have R "rebuild"
# each matrix according to the order of rnames and cnames.
# We'll use the match() function to accomplish this.
g96matched = g96[ match(rnames, row.names(g96)), match(cnames, colnames(g96)) ]
g97matched = g97[ match(rnames, row.names(g97)), match(cnames, colnames(g97)) ]
 
# We need to do the same thing for the diagonal of the matrix g96e, which is
# our co-membership/affiliation matrix computed above:
mag96diagmatched = diag( g96e[ match(cnames, colnames(g96e)), match(cnames, colnames(g96e)) ] )
 
# Now let's check to make sure things worked correctly:
which(row.names(g96matched) != row.names(g97matched))
which(colnames(g96matched) != colnames(g97matched))
 
# NOW we can multiply to get the transition probability matrix:
mag96_97 = t(g96matched) %*% g97matched
 
# Now we need to do the same for 97 and 98:
cnames = intersect( colnames(g97), colnames(g98) ) 
rnames = intersect( row.names(g97), row.names(g98) )
g97matched = g97[ match(rnames, row.names(g97)), match(cnames, colnames(g97)) ]
g98matched = g98[ match(rnames, row.names(g98)), match(cnames, colnames(g98)) ]
 
mag97_98 = t(g97matched) %*% g98matched
mag97diagmatched = diag( g97e[ match(cnames, colnames(g97e)), match(cnames, colnames(g97e)) ] )
 
 
############################################################
## (2) Create mag96-diag and mag97-diag
## what we'll need to do is to run the same proceedure as above 

#mag96diag = diag(g96matched)
#mag97diag = diag(g97matched)
 
###########################################################
# 2 (a) Create magmob96_97 by dividing by mag96diagmatched in
#       order to get a transition probability matrix:
magmob96_97 = mag96_97/mag96diagmatched
 
###########################################################
# 2 a (i) Repeat for mag97 98 using mag97-diag values.
#           Save as magmob97 98.
 
magmob97_98 = mag97_98/mag97diagmatched
 
###########################################################
# (3) aggregate the transition probability matrix across years
 
# Now use the match() function to make it so that the
# transition probability matrices have the same row and
# column entries, using the example above as a guide.
 
# One you have that, you can just add the matrices and
# (scalar) divide by 2.
 
###########################################################
# (4) Now plot as you did with the event-overlap graphs
 
 
############################################################
#
# Questions:
#   (1) How does the story differ for the mobility vs the
#       cross-sectional overlaps?
#   (2) What network properties (cohesion or centrality) afford
#       some insight into your claims?
#   (3) From these results, what substantive story can you make
#       about the extracurriculum and it's career structure?
#
############################################################