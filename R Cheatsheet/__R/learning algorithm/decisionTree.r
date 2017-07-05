install.packages("tree")
require(tree)
iris
head(iris)
head(iris[,1:4])
attach(iris)


dt = tree(Species ~ ., iris)
plot(dt)
text(dt)


ct = cv.tree(dt, FUN=prune.tree)
plot(ct)


pt = prune.tree(dt, best=4)
plot(pt)
text(pt)
summary(pt)

predict(pt, iris[1,])



head(iris)
t <- tree(Species ~ Petal.Length + Petal.Width, iris)
# Draw plot in a square region. See the graph in the below.
par(pty="s")
# iris[, 3]: petal length, iris[, 4]: petal width
plot(iris[, 3], iris[, 4], type="n", xlab="petal length", ylab="petal width")
# iris[, 5]: Species as a factor.
class(iris[, 5])
iris[1,5]
# s: setosa, c: versicolor, v: virginica
text(iris[, 3], iris[, 4], c("s", "c", "v")[iris[, 5]])
partition.tree(t, add=TRUE, cex=1.5)
