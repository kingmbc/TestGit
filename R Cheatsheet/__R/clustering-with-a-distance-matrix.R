# http://stats.stackexchange.com/questions/2717/clustering-with-a-distance-matrix

tmp <- matrix(c(0,20,20,20,40,60,60,60,100,120,120,120,
                20,0,20,20,60,80,80,80,120,140,140,140,
                20,20,0,20,60,80,80,80,120,140,140,140,
                20,20,20,0,60,80,80,80,120,140,140,140,
                40,60,60,60,0,20,20,20,60,80,80,80,
                60,80,80,80,20,0,20,20,40,60,60,60,
                60,80,80,80,20,20,0,20,60,80,80,80,
                60,80,80,80,20,20,20,0,60,80,80,80,
                100,120,120,120,60,40,60,60,0,20,20,20,
                120,140,140,140,80,60,80,80,20,0,20,20,
                120,140,140,140,80,60,80,80,20,20,0,20,
                120,140,140,140,80,60,80,80,20,20,20,0),
              nr=12, dimnames=list(LETTERS[1:12], LETTERS[1:12]))
d <- as.dist(tmp)
mds.coor <- cmdscale(d)
plot(mds.coor[,1], mds.coor[,2], type="n", xlab="", ylab="")
text(jitter(mds.coor[,1]), jitter(mds.coor[,2]),
     rownames(mds.coor), cex=0.8)
abline(h=0,v=0,col="gray75")

plot(hclust(dist(1-tmp), method="single"))
