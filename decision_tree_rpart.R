#loading dataset and sufflling 
set.seed(678)
data <-read.csv("car_data.csv")
head(data)
shuffle_index <- sample(1:nrow(data))
head(shuffle_index)
data <- data[shuffle_index, ]
head(data)

#splitting the dataset into training and test
library(dplyr)
create_train_test <- function(data, size = 0.8, train = TRUE) {
  n_row = nrow(data)
  total_row = size * n_row
  train_sample <- 1: total_row
  if (train == TRUE) {
    return (data[train_sample, ])
  } else {
    return (data[-train_sample, ])
  }
}

data_train <- create_train_test(data, 0.8, train = TRUE)
data_test <- create_train_test(data, 0.8, train = FALSE)
dim(data_train)

prop.table(table(data_train$car_evaluation))
#install.packages("rpart.plot")	

#build the model

library(rpart)
library(rpart.plot)
fit <- rpart(car_evaluation~., data = data_train, method = 'class')
rpart.plot(fit)

#make prediction
predict_unseen <-predict(fit, data_test, type = 'class')
table_mat <- table(data_test$car_evaluation, predict_unseen)
table_mat
