library(MASS)
library(ISLR) #dataset by Statistical Learning professors

##Simple Linear Regression
data(Boston)
names(Boston)
?Boston

plot(medv~lstat, Boston) #as lower status people decrease, median value of houses increase

#response~predictor (response is modeled as predictor)
fit1<-lm(medv~lstat, Boston)
fit1
summary(fit1)

abline(fit1,col="red")
