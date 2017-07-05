# 참고사이트
# https://rulesofreason.wordpress.com/2012/11/05/network-visualization-in-r-with-the-igraph-package/
# 날짜: 2015.10.13

# Load the igraph package (install if needed)
require(igraph)

# Data format. The data is in 'edges' format meaning that each row records a relationship (edge) between two people (vertices).
# Additional attributes can be included. Here is an example:
#  Supervisor	Examiner	Grade	Spec(ialization)
#	AA		BD		6	X	
#	BD		CA		8	Y
#	AA		DE		7	Y
#	...		...		...	...
# In this anonymized example, we have data on co-supervision with additional information about grades and specialization. 
# It is also possible to have the data in a matrix form (see the igraph documentation for details)

# Load the data. The data needs to be loaded as a table first: 

bsk<-read.table("http://www.dimiter.eu/Data_files/edgesdata3.txt", sep='\t', dec=',', header=T)#specify the path, separator(tab, comma, ...), decimal point symbol, etc.

# Transform the table into the required graph format:
bsk.network<-graph.data.frame(bsk, directed=F) #the 'directed' attribute specifies whether the edges are directed
# or equivelent irrespective of the position (1st vs 2nd column). For directed graphs use 'directed=T'

# Inspect the data:

V(bsk.network) #prints the list of vertices (people)
E(bsk.network) #prints the list of edges (relationships)
degree(bsk.network) #print the number of edges per vertex (relationships per people)

# First try. We can plot the graph right away but the results will usually be unsatisfactory:
plot(bsk.network)







###############################################################
g <- graph.ring(10)
plot(g);
shortest.paths(g)
get.shortest.paths(g, 5)
get.all.shortest.paths(g, 1, 6:8)
average.path.length(g)
## Weighted shortest paths
el <- matrix(nc=3, byrow=TRUE,
             c(1,2,0, 1,3,2, 1,4,1, 2,3,0, 2,5,5, 2,6,2, 3,2,1, 3,4,1,
               3,7,1, 4,3,0, 4,7,2, 5,6,2, 5,8,8, 6,3,2, 6,7,1, 6,9,1,
               6,10,3, 8,6,1, 8,9,1, 9,10,4) )
g2 <- add.edges(graph.empty(10), t(el[,1:2]), weight=el[,3])
shortest.paths(g2, mode="out")


# Some trivial ones
g <- graph.ring(10)
plot(g);
graph.knn(g)
g2 <- graph.star(10)
plot(g2);
graph.knn(g2)
# A scale-free one, try to plot ’knnk’
g3 <- ba.game(1000, m=5)
V(g3);
E(g3);
plot(g3);
graph.knn(g3)
# A random graph
g4 <- random.graph.game(1000, p=5/1000)
plot(g4);
graph.knn(g4)
# A weighted graph
g5 <- graph.star(10)
E(g5)$weight <- seq(ecount(g5))
graph.knn(g5)