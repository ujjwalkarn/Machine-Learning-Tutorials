library(MASS) #loads dataset from the book MASS
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

#add a line to the fit 
abline(fit1,col="red")

#see the components of fit
#access any one of these like "fit1$coefficients" etc.
names(fit1)
# [1] "coefficients"  "residuals"     "effects"       "rank"          "fitted.values" "assign"       
# [7] "qr"            "df.residual"   "xlevels"       "call"          "terms"         "model" 

fit1$coefficients
# (Intercept)      lstat 
# 34.5538409  -0.9500494 

#95% confidence interval
confint(fit1)
#                 2.5 %     97.5 %
# (Intercept) 33.448457 35.6592247
# lstat       -1.026148 -0.8739505

#predict medv (response) for these 3 values of lstat (predictor). 
#also show confidece intervals
predict(fit1,data.frame(lstat=c(5,10,15)),interval="confidence")
#        fit      lwr      upr
# 1 29.80359 29.00741 30.59978
# 2 25.05335 24.47413 25.63256
# 3 20.30310 19.73159 20.87461


##Multiple Linear Regression
fit2<-lm(medv~lstat+age,data=Boston) 
summary(fit2)
plot(fit2$residuals)
plot(fitted(fit2),fit2$residuals)
hist(fit2$residuals)

fit3<-lm(medv~.,data=Boston)
summary(fit3)

#plot residuals
par(mfrow=c(2,2))
plot(fit3)
hist(fit3$residuals)

par(mfrow=c(1,1))
plot(fitted(fit3),fit3$residuals)

#update function used below to remove 'age' and 'indus' from the model 
fit4<- update(fit3,~.-age-indus)
summary(fit4)

##Non Linearities and Interactions
#"*" in the formula means we'll have both main-effects &interaction
fit5<-lm(medv~lstat*age, Boston)
summary(fit5)

#"^" term has to be put inside identity function so that ^ is not computed while 
#executing and so that lstat^2 is treated as a separate term
fit6<-lm(medv~lstat+ I(lstat^2), Boston)
summary(fit6)
plot(fit6)

attach(Boston) #make the named variables in Boston avaliable in our R data space
par(mfrow=c(1,1))
plot(medv~lstat)
#cant use abline since that only works when we've a straight line fit
#fitted(fit6) gives fitted value from the model for each value of lstat in training data
points(lstat,fitted(fit6),col="red",pch=20)

#fit polynomial of degree 4
fit7<-lm(medv~poly(lstat,4))
points(lstat,fitted(fit7),col="blue",pch=20) #little more wiggly than desired (overfit)

#see all plotting characters
plot(1:20,1:20,pch=1:20,cex=2)

##Qualitative Predictors
fix(Carseats) #fix opens an external window with the dataframe
names(Carseats)
summary(Carseats)

#* and : both represent interaction terms 
fit8<-lm(Sales~.+Income*Advertising+Age:Price,data=Carseats)
summary(fit8)

#ShelveLoc is a qualitative predictor
#contrasts shows how factors are treated in the model
#only 2 dummy variables "Good" AND "Medium" are generated
#number of dummy variables is 1 less than number of levels in the
#factor variable (ShelveLoc) to prevent multi collinearity
contrasts(Carseats$ShelveLoc)
#        Good Medium
# Bad       0      0
# Good      1      0
# Medium    0      1
