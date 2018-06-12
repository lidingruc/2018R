####################################################################
# LAB 9: Converting igraph to SoNIA with R
####################################################################
 
 
# NOTE: if you have trouble because some packages are not installed,
# see lab 1 for instructions on how to install all necessary packages.
# Also see Lab 1 for prior functions.
 
 
##############################################################
# SoNIA visualizes networks based on vertex and edge (arc) data.
# A number of variables commonly used in iGraph graph objects
# directly map to SoNIA counterparts.  At minimum, SoNIA requires:
# Vertex ids and an edgelsit of directed interactions.
#
# Note: For clarity, the iGraph terms Vetex and Edge are used in
# this lab in place of SoNIA's node and arc.
##############################################################
 
 
 
###
# 1. SET UP SESSION
###

library('igraph')
library("igraphtosonia")

#################################################################
# SoNIA requirements
# For vertex (node) entires, we must supply:
#
#
# NodeId - must be an integer. values can be used more than once 
#	(to specify changes in a node's attributes over time) BUT 
#	MUST FORM A CONTINUOUS SEQUENCE. (if you try to leave out 
#	numbers it will throw an error, as this would mess up the 
#	matrix references.
#
# Additionally, the following vetex attirbutes are supported by 
# the igraphToSoNIA package:
#
# Label - text to be displayed as the vertex label.
# ColorName - text specifying a color for the vertex, one of: 
#	Black DarkGray LightGray White Cyan Green Magenta Orange 
#	Pink Red Yellow Blue 
# NodeShape - text specifying the shape for the node, current 
#	can only be "ellipse" or "rect"
# NodeSize - positive real number specifying the size of the 
#	vertex in pixels
# StartTime - real value specifying the start time for the node
# EndTime - real value specifying the end time for the node. 
#
#
#
# For edge (arc) entires, we must supply: 
#
# FromId - integer (or string, if aplpha id is used) 
#	indicating the ego node must match with a vetext (node) id
# ToId - integer indicating the alter node must match 
#	with a vetext (node) id
#    
# Additionally, the following edge attirbutes are supported by 
# the igraphToSoNIA package:   
#
# ArcWeight - real value indicating the strength of the relation
# ArcWidth - real value indicating how wide to draw the arrow
# ColorName text specifying a color for the edge, one of: 
#	Black DarkGray LightGray White Cyan Green Magenta Orange 
#	Pink Red Yellow Blue. 
# StartTime - real value indicating 
#	the edge's start
# EndTime - real value indicating the edge's termination 
#################################################################

# The first step is to create an iGraph graph object.  In practice,
# this code is most useful if you already have an iGraph graph you wish
# to visualize with SoNIA

edgelist <- read.csv('http://sna.stanford.edu/sna_R_labs/data/edges.csv',header=T)
attributes <- read.csv("http://sna.stanford.edu/sna_R_labs/data/vertices.csv",header=T)
graph <- graph.data.frame(d= edgelist, directed=T, vertices = attributes)

#################################################################
# Important
# If some of the optional attirbues are not present in your 
# data comment out the entry to remove the attribute from your 
# export and to instruct SoNIA to use the default value
#################################################################

# Set required vertex attribute

# set node label
V(graph)$label <- V(graph)$name

# Set optional vertex attributes.  If we will not use a vertex 
# attribute in SoNIA, we will set the attribute to NA

# set vertex color (named colors per the list above)
V(graph)$frame.color = c("red","blue","gray")[V(graph)$polticalparty]

# set vertex shape (circle and rectangle only)
V(graph)$shape = c("circle","rectangle")[V(graph)$gender]
V(graph)$frame.shape = V(graph)$shape

# set vertex size
V(graph)$vertex.size =10

# if we do not have individual start and end times for the nodes, 
# do not set a value for these nodes by commenting out the following lines
# values of 0 and 30 will display all vertices at all times in SoNIA
# set start time for the node
V(graph)$start.time <-1

# set end time for the node
V(graph)$end.time <-6


# The fromId and toId will be set by igraph to sequential values 
# by default. If V(graph)$label is used, then the ids will be strings 
# corresponding to the value set.

# set optional edge attributes

# set start time for the edge 
E(graph)$start.time <-E(graph)$start

# set end time for the edge
E(graph)$end.time <-E(graph)$start

# set edge weight
E(graph)$weight <- runif(ecount(graph))

# set the edge arrow width

E(graph)$arrow.size=1.6

# set edge color (named colors per the list above)
E(graph)$color = "blue"

# create the .son output
write.graph.to.sonia(graph,"export.son")

# when you load the file in SoNIA you MUST rescale the layout.  It will
# look as if nothing was loaded, but it is just a bug in SoNIA. "Rescaling
# layout to fit window" will fix the issue. 


