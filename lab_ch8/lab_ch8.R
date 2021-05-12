#8
#a. Split the data set into a training set and a test set.
library(ISLR)
set.seed(1)
train = sample(1:nrow(Carseats), nrow(Carseats) / 2)
Car.train = Carseats[train, ]
Car.test = Carseats[-train,]

#b Fit a regression tree to the training set. Plot the tree, and interpret the results. What test MSE do you obtain?
library (tree)
reg.tree = tree(Sales~.,data = Carseats, subset=train)
summary(reg.tree)

plot(reg.tree)
text(reg.tree ,pretty =0)
yh = predict(reg.tree,newdata = Car.test)
mean((yh - Car.test$Sales)^2)

#8c
#Use cross-validation in order to determine the optimal level of tree complexity. 
#Does pruning the tree improve the test MSE?
set.seed(1)
cv.car = cv.tree(reg.tree)
plot(cv.car$size, cv.car$dev, type = "b")
#selected by cross-validation. We now prune the tree to obtain the 10-node tree.
prune.car = prune.tree(reg.tree, best = 10)
plot(prune.car)
text(prune.car,pretty=0)

yh=predict(prune.car, newdata= Car.test)
mean((yh-Car.test$Sales)^2)

#8d
#Use the bagging approach in order to analyze this data. 
#What test MSE do you obtain? Use the importance() function to determine which variables are most important.
library(randomForest)
set.seed(1)
bag.car = randomForest(Sales~.,data=Car.train,mtry = 10, importance = TRUE)
yh.bag = predict(bag.car,newdata=Car.test)
mean((yh.bag-Car.test$Sales)^2)

importance(bag.car)
varImpPlot(bag.car)

#8e
#Use random forests to analyze this data. What test MSE do you obtain? Use the importance() function
#to determine which variables are most important. Describe the effect of m, 
#the number of variables considered at each split, on the error rate obtained.

set.seed(1)
rf.car = randomForest(Sales~.,data=Car.train,mtry = 3, importance = TRUE)
yh.rf = predict(rf.car,newdata=Car.test)
mean((yh.rf-Car.test$Sales)^2)

set.seed(1)
rf.car = randomForest(Sales~.,data=Car.train,mtry = 5, importance = TRUE)
yh.rf = predict(rf.car,newdata=Car.test)
mean((yh.rf-Car.test$Sales)^2)
#slightly less when m = 5

set.seed(1)
rf.car = randomForest(Sales~.,data=Car.train,mtry = 7, importance = TRUE)
yh.rf = predict(rf.car,newdata=Car.test)
mean((yh.rf-Car.test$Sales)^2)

set.seed(1)
rf.car = randomForest(Sales~.,data=Car.train,mtry = 9, importance = TRUE)
yh.rf = predict(rf.car,newdata=Car.test)
mean((yh.rf-Car.test$Sales)^2)

importance(rf.car)
varImpPlot(rf.car)
