### Q2 ###
#1.#

set.seed(123)

# generate training data #
x = runif(100,-1,1)
e = rnorm(100)
y = 1.8*x+2+e

# generate test data #
xt = runif(10000,-1,1)
et = rnorm(10000)
yt = 1.8*xt+2+et

#2.#

plot(x,y,pch=20)
abline(2,1.8,lwd=2)

#3.#

fit = lm(y~x)
summary(fit)
b0 = fit$coefficients[1]
b1 = fit$coefficients[2] 
abline(b0,b1,col="blue",lty=2,lwd=2)

#4.#
library(kknn)
train = data.frame(x=x, y=y)
test = train #test = train to get in sample knn fit#
ind = order(test[,1]) #sorting test makes the plots below easier to make.
test = test[ind,] 
near1 = kknn(y~x,train,test,k=2)
near2 = kknn(y~x,train,test,k=12)

# draw the graph for k=2 and k=12 # 
par(mfrow=c(1,2))
plot(x,y,pch=20,main="k=2")
abline(2,1.8,lwd=2)
lines(test[,1],near1$fitted,col=2,lwd=2)

plot(x,y,pch=20,main="k=12")
abline(2,1.8,lwd=2)
lines(test[,1],near2$fitted,col=2,lwd=2)

#5.#
train = data.frame(x=x,y=y)
test = data.frame(x=xt,y=yt)
out_MSE = NULL
kk = seq(2,15,by=1)

for(i in kk){
  
  near = kknn(y~x,train,test,k=i,kernel = "rectangular")
  aux = mean((test[,2]-near$fitted)^2)
  out_MSE = c(out_MSE,aux)
}


plot(log(1/kk),out_MSE,xlab="Complexity (log(1/k))", 
     ylab="MSE", col=4, lwd=2,type="l", ylim=c(1,1.6))
title(main="MSE",cex.main=2)


fitted_reg = b0 + b1*xt
MSE_reg = mean((yt-fitted_reg)^2)
abline(h=MSE_reg, col="red",lwd=2,lty=2)

#6.#
x = runif(100,-1,1)
e = rnorm(100)
y = tanh(1.1*x)+2+e

# generate test data #
xt = runif(10000,-1,1)
et = rnorm(10000)
yt = tanh(1.1*xt)+2+e

plot(x,y,pch=20)


fit = lm(y~x)
summary(fit)
b0 = fit$coefficients[1]
b1 = fit$coefficients[2] 
abline(b0,b1,col="blue",lty=2,lwd=2)

library(kknn)
train = data.frame(x=x, y=y)
test = train #test = train to get in sample knn fit#
ind = order(test[,1]) #sorting test makes the plots below easier to make.
test = test[ind,] 
near1 = kknn(y~x,train,test,k=2)
near2 = kknn(y~x,train,test,k=12)

# draw the graph for k=2 and k=12 # 
par(mfrow=c(1,2))
plot(x,y,pch=20,main="k=2")
abline(b0,b1,lwd=2)
lines(test[,1],near1$fitted,col=2,lwd=2)

plot(x,y,pch=20,main="k=12")
abline(b0,b1,lwd=2)
lines(test[,1],near2$fitted,col=2,lwd=2)

train = data.frame(x=x,y=y)
test = data.frame(x=xt,y=yt)
out_MSE = NULL
kk = seq(2,50,by=1)

for(i in kk){
  
  near = kknn(y~x,train,test,k=i,kernel = "rectangular")
  aux = mean((test[,2]-near$fitted)^2)
  out_MSE = c(out_MSE,aux)
}

par(mfrow=c(1,1))

plot(log(1/kk),out_MSE,xlab="Complexity (log(1/k))", 
     ylab="MSE", col=4, lwd=2,type="l")
title(main="MSE",cex.main=2)


fitted_reg = b0 + b1*xt
MSE_reg = mean((yt-fitted_reg)^2)
abline(h=MSE_reg, col="red",lwd=2,lty=2)

#7.#
x = runif(100,-1,1)
e = rnorm(100)
y = sin(2*x)+2+e

# generate test data #
xt = runif(10000,-1,1)
et = rnorm(10000)
yt = sin(2*xt)+2+e

plot(x,y,pch=20)


fit = lm(y~x)
summary(fit)
b0 = fit$coefficients[1]
b1 = fit$coefficients[2] 
abline(b0,b1,col="blue",lty=2,lwd=2)

library(kknn)
train = data.frame(x=x, y=y)
test = train #test = train to get in sample knn fit#
ind = order(test[,1]) #sorting test makes the plots below easier to make.
test = test[ind,] 
near1 = kknn(y~x,train,test,k=2)
near2 = kknn(y~x,train,test,k=12)

# draw the graph for k=2 and k=12 # 
par(mfrow=c(1,2))
plot(x,y,pch=20,main="k=2")
abline(b0,b1,lwd=2)
lines(test[,1],near1$fitted,col=2,lwd=2)

plot(x,y,pch=20,main="k=12")
abline(b0,b1,lwd=2)
lines(test[,1],near2$fitted,col=2,lwd=2)

train = data.frame(x=x,y=y)
test = data.frame(x=xt,y=yt)
out_MSE = NULL
kk = seq(2,50,by=1)

for(i in kk){
  
  near = kknn(y~x,train,test,k=i,kernel = "rectangular")
  aux = mean((test[,2]-near$fitted)^2)
  out_MSE = c(out_MSE,aux)
}

par(mfrow=c(1,1))

plot(log(1/kk),out_MSE,xlab="Complexity (log(1/k))", 
     ylab="MSE", col=4, lwd=2,type="l")
title(main="MSE",cex.main=2)


fitted_reg = b0 + b1*xt
MSE_reg = mean((yt-fitted_reg)^2)
abline(h=MSE_reg, col="red",lwd=2,lty=2)

#8.#
x = runif(100,-1,1)
e = rnorm(100)
y = sin(2*x)+2+e

X=data.frame(y,x)

for (p in 2:20){
  X[]=rnorm(100)
  
}

for (i in 1:10) {
  
}

# generate test data #
xt = runif(10000,-1,1)
et = rnorm(10000)
yt = sin(2*xt)+2+e
