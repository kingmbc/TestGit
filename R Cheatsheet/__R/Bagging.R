#http://www.inside-r.org/packages/cran/adabag/docs/bagging
install.packages('adabag');
library(adabag)

## rpart library should be loaded
library(rpart)
data(iris)
head(iris)
?bagging
iris.bagging <- bagging(Species~., data=iris, mfinal=10)

## rpart and mlbench libraries should be loaded
library(rpart)
library(mlbench)
data(BreastCancer)
l <- length(BreastCancer[,1])
sub <- sample(1:l,2*l/3)
BC.bagging <- bagging(Class ~.,data=BreastCancer[,-1],mfinal=20, 
                      control=rpart.control(maxdepth=3))
BC.bagging.pred <- predict.bagging(BC.bagging,newdata=BreastCancer[-sub,-1])
BC.bagging.pred$confusion
BC.bagging.pred$error

# Data Vehicle (four classes)
library(rpart)
library(mlbench)
data(Vehicle)
l <- length(Vehicle[,1])
sub <- sample(1:l,2*l/3)
Vehicle.bagging <- bagging(Class ~.,data=Vehicle[sub, ],mfinal=40, 
                           control=rpart.control(maxdepth=5))
Vehicle.bagging.pred <- predict.bagging(Vehicle.bagging,newdata=Vehicle[-sub, ])
Vehicle.bagging.pred$confusion
Vehicle.bagging.pred$error