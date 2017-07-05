# load entropy library 
install.packages("entropy")
library("entropy")

# observed counts for each bin
y = c(4, 2, 3, 0, 2, 4, 0, 0, 2, 1, 1)  
y = c(0, 0, 0, 0, 0, 0, 4, 4, 0, 0, 0)
y = c(0, 0, 0, 0, 0, 0, 4, 0, 0, 0, 4)
y = c(0, 0, 0, 0, 0, 0, 4, 4, 4, 4, 4)
y = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 4)

y = c(32, 16, 8, 4, 1, 1, 1, 1)
y = c(1, 1, 1, 1, 1, 1, 1, 1);
y = c(5, 5);


y = c(1, 1, 1, 1);
y = c(1, 1);
y = c(2, 0);


# empirical estimate of entropy
entropy.empirical(y, "log2")
entropy.empirical(y)  ##exp log를 사용함
entropy.ChaoShen(y)
freqs.empirical(y)

# contigency table with counts for two discrete variables
y = rbind( c(1,2,3), c(6,5,4) )

# empirical estimate of mutual information
mi.empirical(y)

#####################################################################
#https://stat.ethz.ch/pipermail/r-help/2012-February/303452.html
x <- as.factor(c("a","b","a","c","b","c"))
y <- as.factor(c("b","a","a","c","c","b"))
x
y
#I can compute their entropies:
entropy(table(x))
#[1] 1.098612
#but it is not clear how to compute their mutual information directly.
#I can compute the joint entropy as
entropy(table(paste(x,y,sep="")))
#[1] 1.791759
#and then mutual information will be h(x) + h(y) - h(x,y) =
1.098612 + 1.098612 - 1.791759
0.405465