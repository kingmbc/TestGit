require(igraph)
g <- random.graph.game(20, 5/20, directed=TRUE)
page.rank(g)$vector

g2 <- graph.star(10)
page.rank(g2)$vector

# Personalized PageRank
g3 <- graph.ring(10)
page.rank(g3)$vector
reset <- seq(vcount(g3))
page.rank(g3, personalized=reset)$vector

######################################################################################
# http://stackoverflow.com/questions/20206905/page-rank-algo-implementation-in-r
g <- graph(c(
  1, 2, 1, 3, 1, 4, 
  2, 3, 2, 6, 3, 1, 
  3, 5, 4, 2, 4, 1, 
  4, 5, 5, 2, 5, 6, 
  6, 3, 6, 4), 
  directed=TRUE)

M = get.adjacency(g, sparse = FALSE)
M = t(M / rowSums(M))
n = nrow(M)

U = matrix(data=rep(1/n, n^2), nrow=n, ncol=n)
beta=0.85
A = beta*M+(1-beta)*U
e = eigen(A)
v <- e$vec[,1]
v <- as.numeric(v) / sum(as.numeric(v))
v

page.rank(g)$vector
install.packages("expm")
library(expm)
n = nrow(M)
U = matrix(data=rep(1/n, n^2), nrow=n, ncol=n)
beta=0.85
A = beta*M+(1-beta)*U
r = matrix(data=rep(1/n, n), nrow=n, ncol=1)
t(A%^%100 %*% r)
