require('MASS')
data('Boston')

#9a
mu <- mean(Boston$medv)
mu

#9b
se <- sqrt(var(Boston$medv)/nrow(Boston))
se

#9c
library(boot)
set.seed(1)
boot.fn <- function(data, index) {
  mu <- mean(data[index])
  return (mu)
}
boot(Boston$medv, boot.fn, 1000)

#9e
med <- median(Boston$medv)
med

#9f
boot.fn <- function(data, index) {
  mu <- median(data[index])
  return (mu)
}
boot(Boston$medv, boot.fn, 1000)
