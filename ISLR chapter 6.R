# Chapter 6 Problem 8

install.packages("ISLR2")
install.packages("leaps")
library(leaps)

# a)
set.seed(1)
x <- rnorm(100)
error <- rnorm(100)

#b)
b0<-2
b1<-3
b2<-(-4)
b3<-0.5

y<-b0+b1*x+b2*x^2+b3*x^3+error

#c)
library(leaps)
data.full<-data.frame(y=y,x=x)
regfit.full<-regsubsets(y~x+I(x^2) + I(x^3) + I(x^4) + I(x^5) + I(x^6) + I(x^7) + I(x^8) + I(x^9) + I(x^10), data = data.full, nvmax = 10,really.big = T)
reg.summary<-summary(regfit.full)
par(mfrow=c(1,3))

#plot model c_p value for different number of variables.Least value of c_p gives best model
plot(reg.summary$cp,xlab="Number of Variables",ylab="C_p",type="l")
points(which.min(reg.summary$cp),reg.summary$cp[which.min(reg.summary$cp)],col="blue",cex=2,pch=20)

#plot model BIC value
plot(reg.summary$bic,xlab="Number of Variables",ylab="BIC",type="l")
points(which.min(reg.summary$bic),reg.summary$bic[which.min(reg.summary$bic)],col="blue",cex=2,pch=20)

#plot model adj R square. HIgher adj r sqaure gives best model
plot(reg.summary$adjr2,xlab="Number of Variables",ylab="Adj R^2",type="l")
points(which.max(reg.summary$adjr2),reg.summary$adjr2[which.max(reg.summary$adjr2)],col="blue",cex=2,pch=20)

#Model
coef(regfit.full,which.max(reg.summary$adjr2))

#This model predicts X, X^2, X^5 as parameters.

#d


#Forward Selection Approach
library(leaps)
data.full<-data.frame(y=y,x=x)
regfit.fwd<-regsubsets(y~x+I(x^2) + I(x^3) + I(x^4) + I(x^5) + I(x^6) + I(x^7) + I(x^8) + I(x^9) + I(x^10), data = data.full, nvmax = 10,really.big = T,method = "forward")
reg.summary.fwd<-summary(regfit.fwd)
par(mfrow=c(1,3))

#plot model c_p value for different number of variables.Least value of c_p gives best model
plot(reg.summary.fwd$cp,xlab="Number of Variables",ylab="C_p",type="l")
points(which.min(reg.summary.fwd$cp),reg.summary.fwd$cp[which.min(reg.summary.fwd$cp)],col="blue",cex=2,pch=20)

#plot model BIC value
plot(reg.summary.fwd$bic,xlab="Number of Variables",ylab="BIC",type="l")
points(which.min(reg.summary.fwd$bic),reg.summary.fwd$bic[which.min(reg.summary.fwd$bic)],col="blue",cex=2,pch=20)

#plot model adj R square. HIgher adj r sqaure gives best model
plot(reg.summary.fwd$adjr2,xlab="Number of Variables",ylab="Adj R^2",type="l")
points(which.max(reg.summary.fwd$adjr2),reg.summary.fwd$adjr2[which.max(reg.summary.fwd$adjr2)],col="blue",cex=2,pch=20)


#Model
coef(regfit.fwd,which.max(reg.summary.fwd$adjr2))

# With Forward Selection Approach also three predictable variable model is picked.

#Backward Selection Approach
library(leaps)
data.full<-data.frame(y=y,x=x)
regfit.bwd<-regsubsets(y~x+I(x^2) + I(x^3) + I(x^4) + I(x^5) + I(x^6) + I(x^7) + I(x^8) + I(x^9) + I(x^10), data = data.full, nvmax = 10,really.big = T,method = "backward")
reg.summary.bwd<-summary(regfit.bwd)
par(mfrow=c(1,3))

#plot model c_p value for different number of variables.Least value of c_p gives best model
plot(reg.summary.bwd$cp,xlab="Number of Variables",ylab="C_p",type="l")
points(which.min(reg.summary.bwd$cp),reg.summary.bwd$cp[which.min(reg.summary.bwd$cp)],col="blue",cex=2,pch=20)

#plot model BIC value
plot(reg.summary.bwd$bic,xlab="Number of Variables",ylab="BIC",type="l")
points(which.min(reg.summary.bwd$bic),reg.summary.bwd$bic[which.min(reg.summary.bwd$bic)],col="blue",cex=2,pch=20)

#plot model adj R square. HIgher adj r sqaure gives best model
plot(reg.summary.bwd$adjr2,xlab="Number of Variables",ylab="Adj R^2",type="l")
points(which.max(reg.summary.bwd$adjr2),reg.summary.bwd$adjr2[which.max(reg.summary.bwd$adjr2)],col="blue",cex=2,pch=20)


#Model
coef(regfit.bwd,which.max(reg.summary.bwd$adjr2))


#e
install.packages("glmnet")
library(glmnet)
set.seed(1)
xmat<-model.matrix(y ~ x + I(x^2) + I(x^3) + I(x^4) + I(x^5) + I(x^6) + I(x^7) + I(x^8) + I(x^9) + I(x^10), data = data.full)[,-1]
set.seed(1)
cv.lasso<-cv.glmnet(xmat,y,alpha=1)
plot(cv.lasso)


bestlam<-cv.lasso$lambda.min
bestlam

fit.lasso<-glmnet(xmat,y,alpha=1)
predict(fit.lasso,s=bestlam,type="coefficients")[1:11,]

# After performing cross validation we get λ as 0.026845 for minimum MSE.
# We fit the lasso regression using this value of lambda.
# As before, we arrive at the coefficient estimations. The other coefficients are zero.


#f
#Best Subset Method

b7<-7
y <- b0 + b7 * x^7 + error
data.full <- data.frame(y = y, x = x)
regfit.full <- regsubsets(y ~ x + I(x^2) + I(x^3) + I(x^4) + I(x^5) + I(x^6) + I(x^7) + I(x^8) + I(x^9) + I(x^10), data = data.full, nvmax = 10)
reg.summary <- summary(regfit.full)
par(mfrow = c(1, 3))
plot(reg.summary$cp, xlab = "Number of variables", ylab = "C_p", type = "l")
points(which.min(reg.summary$cp), reg.summary$cp[which.min(reg.summary$cp)], col = "red", cex = 2, pch = 20)
plot(reg.summary$bic, xlab = "Number of variables", ylab = "BIC", type = "l")
points(which.min(reg.summary$bic), reg.summary$bic[which.min(reg.summary$bic)], col = "red", cex = 2, pch = 20)
plot(reg.summary$adjr2, xlab = "Number of variables", ylab = "Adjusted R^2", type = "l")
points(which.max(reg.summary$adjr2), reg.summary$adjr2[which.max(reg.summary$adjr2)], col = "red", cex = 2, pch = 20)


coef(regfit.full,2)
coef(regfit.full,1)
coef(regfit.full,4)

#We see cp chooses 2 variable model, BIC chooses 1 variable model while adj r sqaure chooses 4 variable model



#Lasso Model:
xmat <- model.matrix(y ~ x + I(x^2) + I(x^3) + I(x^4) + I(x^5) + I(x^6) + I(x^7) + I(x^8) + I(x^9) + I(x^10), data = data.full)[, -1]
set.seed(1)
cv.lasso <- cv.glmnet(xmat, y, alpha = 1)
bestlam <- cv.lasso$lambda.min
bestlam

fit.lasso <- glmnet(xmat, y, alpha = 1)
predict(fit.lasso, s = bestlam, type = "coefficients")[1:11, ]


# Here the lasso also chooses the best model with one variable.
# Thus lasso performs better as compared to best subset approach as it gives model close to simulated value of y.
