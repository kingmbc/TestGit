install.packages("recommenderlab")
require("recommenderlab")


m <- matrix(sample(c(as.numeric(0:5), NA), 50, replace=TRUE, 
                   prob=c(rep(.4/6,6),.6)), ncol=10, dimnames=list(user=paste("u", 1:5, sep=''),
                               item=paste("i", 1:10, sep='')));
m

r <- as(m, "realRatingMatrix")
as(r,"dgCMatrix")


r_m <- normalize(r); r_m

image(r, main = "Raw Ratings")
image(r_m, main = "Normalized Ratings")

r_b <- binarize(r, minRating=4)
as(r_b, "matrix")

########## 5.4. Inspection of data set properties
data(Jester5k)
Jester5k
as(Jester5k, "dgCMatrix")

r <- sample(Jester5k, 1000);r


hist(getRatings(r), breaks=100)
hist(getRatings(normalize(r)), breaks=100)
hist(getRatings(normalize(r, method="Z-score")), breaks=100)
hist(rowCounts(r), breaks=50)
hist(colMeans(r), breaks=20)

########## 5.5. Creating a recommender
recommenderRegistry$get_entries(dataType = "realRatingMatrix")
r <- Recommender(Jester5k[1:1000], method = "POPULAR"); r
names(getModel(r))
getModel(r)$topN

recom <- predict(r, Jester5k[1001:1002], n=5);  recom
as(recom, "list")

recom3 <- bestN(recom, n = 3); recom3
as(recom3, "list")


recom <- predict(r, Jester5k[1001:1002], type="ratings"); recom
as(recom, "matrix")[,1:10]
