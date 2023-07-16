#Chapter 7 Applied Problem 9:

#a)
library(MASS)

attach(Boston)
poly.fit=glm(nox~poly(dis,3),data=Boston)
summary(poly.fit)

plot(Boston[,c('dis','nox')])
pred=predict(poly.fit,
             data.frame(dis=seq(min(dis),max(dis),length.out = 100)))

lines(seq(min(dis),max(dis),length.out = 100),
      pred,col='red')


#b)
x=seq(min(dis),max(dis),length.out = 100)
clrs=rainbow(10)
plot(Boston[,c('dis','nox')])
rss=c()

for(d in 1:10){
  poly.fit=glm(nox~poly(dis,d),data = Boston)
  pred=predict(poly.fit,data.frame(dis=x))
  lines(x,pred,col=clrs[d])
  
  rss=c(rss,sum(poly.fit$residuals^2))
}

legend(x='topright',legend = 1:10,col=clrs,lty = c(1,1),lwd = c(2,2))

plot(rss,xlab='Degree of Polynomials',ylab='RSE',type='l')

#c)
library(boot)

set.seed(532)
poly.mse=c()
for(degree in 1:7){
  poly.fit=glm(nox~poly(dis,degree,raw=T),data=Boston)
  mse=cv.glm(poly.fit,data = Boston,K=10)$delta[1]
  poly.mse=c(poly.mse,mse)
}

plot(poly.mse,type='l',xlab='Polynomial Degree',ylab='Cross Validation MSE')
points(which.min(poly.mse),poly.mse[which.min(poly.mse)],col='red',pch=20,cex=2)


#d)
library(splines)
library(MASS)

spline.fit=lm(nox~bs(dis,df =4),data=Boston)
x=seq(min(Boston[,'dis']),max(Boston[,'dis']),length.out = 100)
y=predict(spline.fit,data.frame(dis=x))

plot(Boston[,c('dis','nox')],ylim=c(0,1))
lines(x,y,col=clrs[4])


#e)
plot(Boston[,c('dis','nox')],ylim=c(0,1))
clrs=rainbow(10)

x=seq(min(Boston[,'dis']),max(Boston[,'dis']),length.out = 100)

rss=c()
for(df in 3:10){
  spline.fit=lm(nox~bs(dis,df=df),data=Boston)
  y=predict(spline.fit,data.frame(dis=x))
  lines(x,y,col=clrs[df])
  
  rss=c(rss,sum(spline.fit$residuals^2))
}

legend(x='topright',legend=3:10,text.col=clrs[3:10],text.width=0.5,bty = 'n',horiz = T)

plot(3:10,rss,xlab='Df',ylab='Train RSS',type='l')


#f)
library(boot)

set.seed(42)
spline.mse=c()

for(df in 3:10){
  Boston.model=model.frame(nox~bs(dis,df=df),data=Boston)  
  names(Boston.model)=c('nox','bs.dis')                    
 
  
  spline.fit=glm(nox~bs.dis,data=Boston.model)
  mse=cv.glm(spline.fit,data=Boston.model,K=10)$delta[1]
  spline.mse=c(spline.mse,mse)
}

plot(3:10,spline.mse,type='l',xlab='Df',ylab='Cross Val. MSE for Splines')

x=which.min(spline.mse)
points(x+2,spline.mse[x],col='red',pch=20,cex=2)
