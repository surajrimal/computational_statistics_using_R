library(ISLR)


set.seed(2)
train = sample(c(TRUE, FALSE), nrow(College), rep=TRUE)
test = (!train)

library(leaps)
regfit.best = regsubsets(Apps~., data=College[train,], nvmax = 18)
test.mat = model.matrix(Apps~., data = College[test,])

# Vector to store errors for different models
val.errors = rep(NA, 18)

for(i in 1:17){
  coefi = coef(regfit.best, id = i)
  pred = test.mat[, names(coefi)]%*%coefi
  val.errors[i] = mean((College$Apps[test]-pred)^2)
}

# Determining the minimum value for errors in vector
min_error = which.min(val.errors)
cat("Error using linear regression is ", val.errors[min_error])

#9c
library(glmnet)
x = model.matrix(Apps~., College)[,-1]
y = College$Apps

# Getting the best lambda using cross validation
cv.out = cv.glmnet(x[train,], y[train], alpha = 0)
best_lambda = cv.out$lambda.min

grid=10^seq(10,-2, length =50)
ridge.mod = glmnet(x[train,], y[train], alpha = 0, lambda = best_lambda)
ridge.pred = predict(ridge.mod, s= best_lambda, newx = x[test,])
error = mean((ridge.pred - y[test])^2)
cat("Error using ridge regression is", error)

#9d
cv.out = cv.glmnet(x[train,], y[train], alpha = 1)
best_lambda = cv.out$lambda.min
lasso.mod = glmnet(x[train,], y[train], alpha = 1, lambda = best_lambda)
out = glmnet(x, y, alpha = 1, lambda = best_lambda)
lasso.coef = predict(out, type="coefficients", s= best_lambda)[1:18,]
num_of_non_zero_coefficients = length(lasso.coef[lasso.coef != 0])
lasso.pred = predict(lasso.mod, s=best_lambda, newx=x[test,])
error = mean((lasso.pred-y[test])^2)
cat("Number of non-zero coefficients is ", num_of_non_zero_coefficients)
cat("Error using lasso regression is", error)

#9e
library(pls)
pcr.fit = pcr(Apps~., data=College, subset = train, scale=TRUE, valication="CV")
validationplot(pcr.fit, val.type="MSEP")
# We see that the lowest cross validation error occurs when M = 17
pcr.pred = predict(pcr.fit, x[test,], ncomp = 17)
error = mean ((pcr.pred-y[test])^2)
cat("Error using pcr is", error)

#9f
pls.fit = plsr(Apps~., data = College, subset = train, scale = TRUE, validation="CV")
validationplot(pls.fit, val.type="MSEP")
# We see that the lowest cross validation error occurs when M = 17
pls.pred = predict(pls.fit, x[test,], ncomp = 17)
error = mean((pls.pred-y[test])^2)
cat("Error using pls is", error)
