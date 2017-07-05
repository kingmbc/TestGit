#http://www.r-tutor.com/gpu-computing/clustering/hierarchical-cluster-analysis

mtcars;
as.matrix(mtcars);
d <- dist(as.matrix(mtcars)) ;
d;

?dist

d <- dist(as.matrix(mtcars))   # find distance matrix 
hc <- hclust(d)                # apply hirarchical clustering 
plot(hc)                       # plot the dendrogram


test.data <- function(dim, num, seed=17) { 
  set.seed(seed) 
  matrix(rnorm(dim * num), nrow=num) 
}; 
m <- test.data(120, 4500) 

library(rpud)                  # load rpud with rpudplus 
d <- rpuDist(m)                # Euclidean distance 

system.time(hclust(d))         # complete linkage 
