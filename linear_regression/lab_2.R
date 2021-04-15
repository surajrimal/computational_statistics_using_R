setwd("D:/cs lab/lab2")
library(ISLR)

#--9a--
pdf("figures.pdf")
pairs(Auto)
mtext("Figure-1", side = 3, line = -2, outer = TRUE)

#--9b--
cor(Auto[1:8])

#--9c--
linear_model <- lm(mpg~.-name, data=Auto)
summary(linear_model)

#--9c(i)__Answer: 
# Yes, the relation between the predictors and the model exists.
# We may assume that some of them have major relationships and some of them have
# negligible relationships when looking at the p-value of the various predictors.
# But there is, in the end, a relation among them.

#--9c(ii)__Answer:
# The most precise relationship to the response is the predictor with the least p-value.
# Looking at the code output, we can assume that the most relevant relationship
# to the response is weight, year and origin, i.e. the value of these predictors more contributes to the response.

#--9c(iii)__ Answer:
# The coefficient of the year variable means that the value of mpg is increased by 0.750773 
# for a single year-value increase. This is a positive increase.


#--9d--
par(mfrow=c(2,2))
plot(linear_model)
mtext("Figure 2", side = 3, line = -2, outer = TRUE)

#==>: By Looking at the residuals vs fitted plot, we can see there is a nearly horizontal line  but not
# linear. The data points #323, #326 and #327 look out of the line and they are called outliers.
# This suggests that a possible issue may be these plots.
# In the Normal Q-Q plot is a straight line, it suggests that the residuals are normally distributed
# except the points #323, #326 and #327 are outliers.
# Scale-location plot is a horizontal line with resudials equally spreaded except #323, #326 and #327.
# Finally, the Residual vs. leverage plot shows the most of the points inside the cook's curve
# but the data points #327 and #394 have high leverage points.


#--9e--
lm_interactions = lm(mpg ~ .+cylinders:origin + year:origin + displacement*weight + 
                       acceleration*weight +acceleration * horsepower + + weight*origin + weight*year,data = Auto[, 1:8])
summary(lm_interactions)

# Answer
# The interactions indicate that there is statistically significant displacement:weight,
# horsepower:acceleration and weight:year, which can be inferred from the p-value in the output.
# Weight:origin is the most insignificant interaction between the interactions tested above.

#--9f--

#Normal
par(mfrow=c(2,2))
lm_cylinder = lm(mpg~horsepower, data=Auto[, 1:8])
plot(lm_cylinder)
mtext("Figure 3 : MPG~horsepower", side = 3, line = -2, outer = TRUE)
#Logarithmic
par(mfrow=c(2,2))
lm_cylinder_log = lm(mpg~log(horsepower), data=Auto[, 1:8])
plot(lm_cylinder_log)
mtext("Figure 4 : MPG~Log(horsepower)", side = 3, line = -2, outer = TRUE)
#Square root
par(mfrow=c(2,2))
lm_cylinder_root = lm(mpg~sqrt(horsepower), data=Auto[, 1:8])
plot(lm_cylinder_root)
mtext("Figure 5 : MPG~Sqrt(horsepower)", side = 3, line = -2, outer = TRUE)
#Square
par(mfrow=c(2,2))
lm_cylinder_square = lm(mpg~I(horsepower^2), data=Auto[, 1:8])
plot(lm_cylinder_square)
mtext("Figure 6: MPG~I(horsepower^2)", side = 3, line = -2, outer = TRUE)

# We can say from the residual plots in Figure 3 that the relationship is non-linear between mpg and horsepower.
# We can see that the transformation in Figure 4, which is a logarithmic transformation, provides the somehow
# linear performance using different transformations.




#Question no. 13


#--13a--
set.seed(1)
x <- rnorm(100, 0, 1)

#--13b--
eps <- rnorm(100, 0, 0.25)

#--13c--
y <- -1 + 0.5*x + eps
length(y)
#Answer: The length of vector y is 100. B0 = -1 and B1 = 0.5


#--13d--
plot(x, y, main = "Figure 7 : Least Sq & Population Line")
# Remark: The scatterplot displays a very linear relationship between x and y.
# Though there are few outliers, they do not trigger much deviation from the linear relationship.

#--13e--
least_square = lm(y~x)
summary(least_square)
#Observation: Using the Least Square linear model, we got the following the values:
  #   B0 = -0.99309
  #   B1 = 0.48663

#by comparing the we get the close result. The values closely resemble to each other.

#--13f--
abline(least_square, col="blue")
abline(-1, 0.5, col="red")
legend("topleft", legend=c("Least Sq. Line", "Population Line"), col=c("blue","red"), lty=1:2, cex=0.8)


#--13G--
least_sq_polynomial = lm(y~x+I(x^2))
summary(least_sq_polynomial)
anova(least_square, least_sq_polynomial)
#Observation: The significant p-value correlated with the quadratic term shows that there is no change in the model. 
#The Anova also suggests that 2.1236 is the F-statistic and that the corresponding p-value is not zero. Thus, we can 
#infer that it does not help to fit the data well to add the quadratic term.

#--13h--
# decreasing the variance of the normal distribution
x2 <- rnorm(100, 0, 1);
eps2 <- rnorm(100, 0, 0.025)  
y2 <- -1 + 0.5*x2 + eps2
plot(x2, y2, main = "Figure 8 : Noise reduced")
least_square2 = lm(y2~x2)
summary(least_square2)
abline(least_square2, col="blue")
abline(-1, 0.5, col="red")
legend("topleft", legend=c("Least Sq. Line", "Population Line"), col=c("blue","red"), lty=1:2, cex=0.8)

#Observation: It decreases the number of outliers by adding less noise to the results. This means that it appears
# that the prediction line is closer to the results. The dependence of the intercept response, which can be
#interpreted from the p-value, also increases drastically. The number of residuals is therefore reduced.

#--13i--
#increasing the variance of the normal distribution

x3 <- rnorm(100, 0, 1);
eps3 <- rnorm(100, 0, 2.5)  
y3 <- -1 + 0.5*x3 + eps3
plot(x3, y3, main = "Figure 9 : Noise added")
least_square3 = lm(y3~x3)
summary(least_square3)
abline(least_square3, col="blue")
abline(-1, 0.5, col="red")
legend("topleft", legend=c("Least Sq. Line", "Population Line"), col=c("blue","red"), lty=1:2, cex=0.8)
dev.off()
#Observation: It increases the number of outliers by adding more noise to the data. This implies that the
# prediction line tends to isolate the sum of highly dispersed data. The dependence of the intercept response,
# which can be interpreted from the p-value, also decreases dramatically.


#--13j--
#Normal
confint(least_square)
#Noise Reduced
confint(least_square2)
#Noise Added
confint(least_square3)

#Observation: The confidence interval indicates that for the predictors and the intercepts from the original data 
# to the more noisy data(i), the confidence interval range is increased. In comparison, the less noisy data(h) indicates
#a dramatic reduction in the confidence interval. The more noise is applied to the results, the more
# the confidence interval increases, we can infer.