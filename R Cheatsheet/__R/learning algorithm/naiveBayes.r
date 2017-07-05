#############################################################
## "klaR" Package
#############################################################
## iris classification
require(klaR)
data(iris)
m <- NaiveBayes(Species ~ ., data = iris)
pred = predict(m)
table(pred$clas, iris[,5])

##Spam Filtering
install.packages("ElemStatLearn")
require(klaR)
data(spam, package="ElemStatLearn")

# set up a training sample
train.ind <- sample(1:nrow(spam), ceiling(nrow(spam)*2/3), replace=FALSE)

# apply NB classifier
nb.res <- NaiveBayes(spam ~ ., data=spam[train.ind,])

# predict on holdout units
nb.pred <- predict(nb.res, spam[-train.ind,])

# but this also works on the training sample, i.e. without using a `newdata`
head(predict(nb.res))

#############################################################
## "bnlearn" Package
#############################################################
require(bnlearn)
install.packages("bnlearn")

data(learning.test); summary(learning.test); head(learning.test); ?learning.test
res = empty.graph(names(learning.test))
modelstring(res) = "[A][C][F][B|A][D|A:C][E|B:F]"
plot(res)

bn = naive.bayes("A", LETTERS[2:6])
pred = predict(bn, learning.test)
table(pred, learning.test[, "A"])
# pred a b c
# a 1286 310 178
# b 192 977 203
# c 190 383 1281
tan = tree.bayes(learning.test, "A")
fitted = bn.fit(tan, learning.test, method = "bayes")
pred = predict(fitted, learning.test)
table(pred, learning.test[, "A"])
# pred a b c
# a 1300 180 184
# b 206 1342 239
# c 162 148 1239

##############################################################
## "e1071" Package
#############################################################
data(iris, packages = "e1071")
summary(iris)
model = naiveBayes(iris[,1:4], iris[,5])
table(predict(model, iris[,-5]), iris[,5])                                                                                                                     library(e1071)

##########################################################
## 1.Categorical data only:
require(e1071)
?naiveBayes
head(HouseVotes84)
model_noLap <- naiveBayes(Class ~ ., data = HouseVotes84)
predict(model_noLap, HouseVotes84[1:10,-1])
predict(model_noLap, HouseVotes84[1:10,-1], type = "raw")

pred_noLap <- predict(model_noLap, HouseVotes84[,-1])
res_noLap = table(pred_noLap, HouseVotes84$Class)

## 2.using laplace smoothing:
model_Lap <- naiveBayes(Class ~ ., data = HouseVotes84, laplace = 3)
pred_Lap <- predict(model_Lap, HouseVotes84[,-1])
res_Lap =table(pred_Lap, HouseVotes84$Class)

res_noLap;res_Lap;

############################################################
## Example of using a contingency table:
data(Titanic); head(Titanic)
m <- naiveBayes(Survived ~ ., data = Titanic)
m
pred_Tit = predict(m, as.data.frame(Titanic)[,1:3])
isSurvived = as.data.frame(Titanic)[,4];
res_Tit = table(pred_Tit, isSurvived)



############################################################
## Example with metric predictors:
data(iris)
m <- naiveBayes(Species ~ ., data = iris)
## alternatively:
m <- naiveBayes(iris[,-5], iris[,5])
m
table(predict(m, iris[,-5]), iris[,5])

## Compare with K-means clustering
data(iris) 
cl <- kmeans(iris[,1:4], 3) 
cl$cluster
cbind(1:150,iris$Species)
table(cl$cluster, iris[,5])