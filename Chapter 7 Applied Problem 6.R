#Chapter 7 Applied Problem 6:
#a)
install.packages("ISLR")
install.packages("boot")

library(boot)
library(ISLR)

set.seed(532)
poly.mse=c()
for(degree in 1:7){
  poly.fit=glm(wage~poly(age,degree,raw=T),data=Wage)
  mse=cv.glm(poly.fit,data = Wage,K=10)$delta[1]
  poly.mse=c(poly.mse,mse)
}

plot(poly.mse,xlab='Degree of Polynomial',ylab='Cross Validation Error',type='l')
x=which.min(poly.mse)
points(x,poly.mse[x],pch=20,cex=2,col='red')


#b)
set.seed(42)
step.mse=c()
for(br in 2:10){
  Wage.model=model.frame(wage~cut(age,br),data=Wage)
  names(Wage.model)=c('wage','age')
  
  step.fit=glm(wage~age,data=Wage.model)
  mse=cv.glm(step.fit,data = Wage.model,K=10)$delta[1]
  step.mse=c(step.mse,mse)
}

plot(step.mse,xlab='Degree of Polynomial',ylab='Cross Validation Error',type='l')
x=which.min(step.mse)
points(x,step.mse[x],pch=20,cex=2,col='red')
