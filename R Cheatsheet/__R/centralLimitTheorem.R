#http://using.tistory.com/63

sample <- matrix(sample(c(1,2,3,4,5,6), 5 * 500, replace = T), ncol = 5, byrow = T)
hist(rowMeans(sample), 
     nclass = 20, col = "light grey", 
     border = "grey", main = "Central Limit Theorem")

head(sample)
tail(sample)
rowMeans(sample)

apply(sample, 1, mean)

head(rowMeans(sample))


CLT <- function(no.throw = 5, no.rep = 500) {
  exp.res <- matrix(sample(1:6, no.throw * no.rep, replace = T), ncol = no.throw, byrow = T)
  
  # return(apply(exp.res, 1, mean))
  return(rowMeans(exp.res))
}

hist(CLT(no.throw = 15, no.rep = 50), 
     nclass = 20, col = "light grey", 
     border = "grey", main = "Central Limit Theorem")
