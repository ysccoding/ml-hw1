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

