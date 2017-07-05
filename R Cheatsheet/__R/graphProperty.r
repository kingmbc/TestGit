install.packages("igraph")
require(igraph)
?igraph

g1 <- graph.empty()
g2 <- graph( c(1,2,2,3,3,4,5,6), directed=FALSE )
g5 <- graph.star(10, mode="out")
g6 <- graph.lattice(c(5,5,5))
g7 <- graph.lattice(length=5, dim=3)
g8 <- graph.ring(10)
g9 <- graph.tree(10, 2)
g10 <- graph.full(5, loops=TRUE)
g11 <- graph.full.citation(10)
g12 <- graph.atlas(sample(0:1252, 1))
el <- matrix( c("foo", "bar", "bar", "foobar"), nc=2, byrow=TRUE)
g13 <- graph.edgelist(el)
g15 <- graph.extended.chordal.ring(15, matrix(c(3,12,4,7,8,11), nr=2))

g <- graph.ring(10)
plot(g11, layout=layout.kamada.kawai, vertex.color="green")
write.graph(g, "/tmp/g.txt", "edgelist")

g <- graph.ring(10)
g$layout <- layout.circle
plot(g)
tkplot(g)
rglplot(g)

g <- graph.ring(10)
degree(g)
g2 <- erdos.renyi.game(1000, 10/1000)
degree.distribution(g2)


g <- graph.ring(10)
g <- set.graph.attribute(g, "name", "RING")
# It is the same as
g$name <- "RING"
g$name

g <- set.vertex.attribute(g, "color", value=c("red", "green"))
get.vertex.attribute(g, "color")

g <- set.edge.attribute(g, "weight", value=runif(ecount(g)))
get.edge.attribute(g, "weight")

# The following notation is more convenient

g <- graph.star(10)

V(g)$color <- c("red", "green")
V(g)$color

E(g)$weight <- runif(ecount(g))
E(g)$weight

print(g, g=TRUE, v=TRUE, e=TRUE)

g <- graph.ring(10)
transitivity(g)