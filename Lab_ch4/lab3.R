#
#
#Author: Suraj Rimal
#problem 11 from book
setwd("D:/cs lab/lab_3")
library(ISLR)
#Library for lda() function
library(MASS)
#library for knn() function
library(class)



#---11a---
mpg01 <- rep(0, nrow(Auto))
median_mpg = median(Auto$mpg)
mpg01[Auto$mpg >= median_mpg] = 1
as.factor(mpg01)
#single dataset containing both mpg01 and Auto
newauto = data.frame(Auto, mpg01)

#---11b---
# Data Exploration graphically
pdf("lab4.pdf")
pairs(newauto)
title(main="Figure 1: Scatterplot matrix")
boxplot(mpg01~cylinders, data=newauto, main = "Figure 2: Boxplot mpg01 vs cylinder")
boxplot(mpg01~acceleration, data=newauto, main = "Figure 3: Boxplot mpg01 vs acceleration")
boxplot(mpg01~horsepower, data=newauto, main = "Figure 4: Boxplot mpg01 vs horsepower")
boxplot(mpg01~weight, data=newauto, main = "Figure 5: Boxplot mpg01 vs weight")
dev.off()


#--11c--
# Splitted into training and testing dataset
set.seed(101) # Set Seed so that same sample can be reproduced in future also
# Now Selecting 75% of data as sample from total 'n' rows of the data  
sample <- sample.int(n = nrow(newauto), size = floor(.75*nrow(newauto)), replace = F)
training <- newauto[sample, ]
testing  <- newauto[-sample, ]

#--11d--
# Performing LDA on the data
lda.fit = lda(mpg01~horsepower+weight+acceleration, data=training, subset=sample)
lda.pred = predict(lda.fit, testing)
lda.class = lda.pred$class
accuracy = mean(lda.class == testing$mpg01) * 100
cat("-------------------------------Accuracy for LDA : ", accuracy, fill = TRUE)

#--11e--
# Performing QDA on the data
qda.fit = qda(mpg01~horsepower+weight+acceleration, data=training, subset=sample)
qda.pred = predict(qda.fit, testing)
qda.class = qda.pred$class
accuracy = mean(qda.class == testing$mpg01) * 100
cat("-------------------------------Accuracy for QDA : ", accuracy, fill = TRUE)

#--11f--
# Performing Logistic Regression on the data
glm.fit = glm(mpg01~horsepower+weight+acceleration, data=training, family = binomial, subset = sample)
glm.probs = predict(glm.fit, testing, type="response")
glm.pred = rep(0, nrow(testing))
glm.pred[glm.probs > .5] = 1
accuracy = mean(glm.pred == testing$mpg01) * 100
cat("-------------------------------Accuracy for LR : ", accuracy, fill = TRUE)

#--11g--
# Performing KNN  classification on the data
train.X = training[,c("horsepower", "weight", "acceleration")]
test.X = testing[,c("horsepower", "weight", "acceleration")]
set.seed(1)
knn.pred = knn(train.X, test.X, training$mpg01, k = 1)
table(knn.pred, testing$mpg01)
accuracy = mean(knn.pred == testing$mpg01) * 100
cat("-------------------------------Accuracy for KNN : ", accuracy, fill = TRUE)

knn.pred = knn(train.X, test.X, training$mpg01, k = 2)
#test error
1- mean(knn.pred == testing$mpg01)

knn.pred = knn(train.X, test.X, training$mpg01, k = 3)
1- mean(knn.pred == testing$mpg01)

knn.pred = knn(train.X, test.X, training$mpg01, k = 5)
1- mean(knn.pred == testing$mpg01)

knn.pred = knn(train.X, test.X, training$mpg01, k = 10)
1- mean(knn.pred == testing$mpg01)

knn.pred = knn(train.X, test.X, training$mpg01, k = 15)
1- mean(knn.pred == testing$mpg01)

knn.pred = knn(train.X, test.X, training$mpg01, k = 20)
1- mean(knn.pred == testing$mpg01)

knn.pred = knn(train.X, test.X, training$mpg01, k = 25)
1- mean(knn.pred == testing$mpg01)

knn.pred = knn(train.X, test.X, training$mpg01, k = 50)
1- mean(knn.pred == testing$mpg01)

knn.pred = knn(train.X, test.X, training$mpg01, k = 12)
1- mean(knn.pred == testing$mpg01)
