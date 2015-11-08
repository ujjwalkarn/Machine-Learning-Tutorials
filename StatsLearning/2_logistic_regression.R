#install.packages("ISLR")
require(ISLR)
names(Smarket)
summary(Smarket)

mydata<-Smarket
?Smarket

#plot the data
pairs(Smarket,col=Smarket$Direction)

#Logistic Regression
#family=Binomial means logistic regression
glm.fit<- glm(Direction~ Lag1+Lag2+Lag3+Lag4+Lag5+Volume,data=Smarket,family=binomial)
#?glm
#?family
summary(glm.fit)
glm.probs=predict(glm.fit,type="response")

#install.packages("usdm")
library(car)
vif(glm.fit) # variance inflation factors 
sqrt(vif(glm.fit)) > 2
