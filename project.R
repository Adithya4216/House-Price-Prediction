
rm(list=ls())
library(rio)
library(moments)
library(car)
project_data=import("regression project data.xlsx")
colnames(project_data)=tolower(make.names(colnames(project_data)))
colnames(project_data)
set.seed(32769556)
project_sample=project_data[sample(1:nrow(project_data),100),]
str(project_sample)
attach(project_sample)


plot(project_sample, pch=19, col="maroon")
hist(project_sample$sqft.living, pch=19, col='red', 
     main="Square ft Living Area of houses", xlab="Square ft Area")
hist(project_sample$condition, pch=19, col='red', 
     main="Condition of houses", xlab="House Condition")
hist(project_sample$year.built, pch=19, col='red', 
     main="Built year of houses", xlab="Built Year")

simple_reg1=lm(price~sqft.living, data=project_sample)
simple_reg1
summary(simple_reg1)
plot(sqft.living, price, pch=19)
abline(simple_reg1, col="red", lwd=3)

simple_reg2=lm(price~condition, data=project_sample)
simple_reg2
summary(simple_reg2)
plot(condition, price, pch=19)
abline(simple_reg2, col='red', lwd=3)

simple_reg3=lm(price~year.built, data=project_sample)
simple_reg3
summary(simple_reg3)
plot(year.built, price, pch=19)
abline(simple_reg3, col='red', lwd=3)

multiple_reg1=lm(price~year.built+sqft.living, data=project_sample)
multiple_reg1
summary(multiple_reg1)

multiple_reg2=lm(price~sqft.living+condition, data=project_sample)
multiple_reg2
summary(multiple_reg2)

multiple_reg3=lm(price~year.built+condition, data=project_sample)
multiple_reg3
summary(multiple_reg3)

multiple_reg4=lm(price~year.built+sqft.living+condition, data=project_sample)
multiple_reg4
summary(multiple_reg4)

multiple_reg5=lm(price~year.built+sqft.living+year.built*sqft.living, 
                 data=project_sample)
multiple_reg5
summary(multiple_reg5)

multiple_reg6=lm(price~year.built+I(year.built^2), data=project_sample)
multiple_reg6
summary(multiple_reg6)

multiple_reg7=lm(price~sqft.living+I(sqft.living^2), data=project_sample)
multiple_reg7
summary(multiple_reg7)

par(mfrow=c(2,2))
# Linearity
plot(project_sample$price,multiple_reg5$fitted.values,pch=19,
     main="Price Plot: Actuals vs. Fitted Values", 
     xlab=c("Actual Values"), ylab=c("Fitted Values"))
abline(0,1,col="red",lwd=3)

# Normality
qqnorm(multiple_reg5$residuals,pch=19,main="Normality Plot (Price)")
qqline(multiple_reg5$residuals,col="red",lwd=2)

#histogram view
hist(multiple_reg5$residuals,col="blue",main="Sample Residuals", 
     xlab=c("Residuals"), probability=TRUE)
curve(dnorm(x,0,sd(multiple_reg5$residuals)),
      from = min(multiple_reg5$residuals),
      to = max(multiple_reg5$residuals),
      lwd=3,col='red',add=TRUE)

# Equality of Variances
plot(multiple_reg5$fitted.values,rstandard(multiple_reg5),pch=19,
     main="Standardized Residuals (Price)", 
     xlab=c("Fitted Values"), ylab=c("Standardized Values"))
abline(0,0,col="red",lwd=3)
par(mfrow=c(1,1))

#Independence of Residuals
dWT=car::durbinWatsonTest(multiple_reg5)
dWT


prediction_data=data.frame(sqft.living=2500, condition='1', year.built=2025)
predict(multiple_reg5,prediction_data,interval = "confidence")
predict(multiple_reg5,prediction_data,interval = "predict")


