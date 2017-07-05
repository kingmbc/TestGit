pacman::p_load(igraph)
mymatrix <- rbind(
		c(1,1,2,3,3,3,2,1,1,1),
		c(1,1,1,2,2,2,1,1,1,1),
		c(2,1,1,1,1,1,1,1,2,2),
		c(3,2,1,1,1,1,1,2,3,3),
		c(3,2,1,1,1,1,1,2,3,3),
		c(3,2,1,1,1,1,1,2,2,2),
		c(2,1,1,1,1,1,1,1,2,2),
		c(1,1,1,2,2,2,1,1,1,1),
		c(1,1,2,3,3,2,2,1,1,1),
		c(1,1,2,3,3,2,2,1,1,1))

#turn this into an adjacency matrix
adjMat <- mymatrix == 1
diag(adjMat) <- 0 #no self loops

g  <- graph.adjacency(adjMat)
plot(g)

#only works for undirected graphs, which this example is fine since symetric
fc <- fastgreedy.community(as.undirected(g))

#make colors for different communities
V(g)$color <- ifelse(membership(fc)==1,"red","blue")
plot(g)







karate <- make_graph("Zachary")
wc <- cluster_walktrap(karate)
modularity(wc)
membership(wc)
plot(wc, karate)
