### NON-Negative Matrix Factorization
#https://cran.r-project.org/web/packages/NMF/vignettes/NMF-vignette.pdf
#https://cran.r-project.org/web/packages/NMF/NMF.pdf


install.packages("NMF");
require(NMF);

# generate a synthetic dataset with known classes
n <- 50; counts <- c(5, 5, 8);
#행 = 50, 3개의 속성이 있고, 각 속성별로 5,5,8 개의 Feature or Attribute을 가지는 , 
V <- syntheticNMF(n, counts)
# perform a 3-rank NMF using the default algorithm
res <- nmf(V, 3)
basismap(res)
coefmap(res)

summary(V)
head(V)
tail(V)

?syntheticNMF
