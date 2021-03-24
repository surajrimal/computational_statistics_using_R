#8a
setwd("D:/cs lab")
college = read.csv("College.csv", header=TRUE)

#8b
rownames(college) = college[, 1]
fix(college)

college = college[, -1]
college$Private = as.factor(college$Private)
fix(college)

#8c_i
summary(college)
#8c_ii
pairs(college[, 1:10])
#8_c_iii
plot(college[, 1], college[, 9])

#8_c_iv
Elite = rep("No", nrow(college));
Elite[college$Top10perc>50] = "Yes"
Elite = as.factor(Elite)
college = data.frame(college, Elite)
summary(college)
plot(college[ , "Outstate"], college[ , "Elite"], xlab = "Outstate", ylab = "Elite")

#8_c_v
par(mfrow=c(2,2))
hist(college$Enroll, col = 2,main = "Histogram of College Enroll", xlab = "Enroll")
hist(college$Apps, col = 2,main = "Histogram of College Apps", xlab = "Apps")
hist(college$Accept, col = 2,main = "Histogram of College Accepts", xlab = "Accepts")
hist(college$Top25perc, col = 2,main = "Histogram of Top 25 Percent colleges", xlab = "Top 25 Percent")

#8_c_vi
par(mfrow=c(2,2))
plot(college[, "Private"], college[, "Enroll"]);
#Conclusion: Enrollment in private college is relatively lower than in non-private colleges
plot(college[, "Private"], college[, "Grad.Rate"])
#Conclusion: Graduation rate in private college is relatively higher than in non-private colleges
plot(college[, "Private"], college[, "Accept"])
#Conclusion: Acceptance rate in Private colleges is lower compared to others
plot(college[, "Elite"], college[, "PhD"])
#Conclusion: PhD student in Elite colleges is higher compared to others
