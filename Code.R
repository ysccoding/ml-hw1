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
abline(2,1.8,col="red")

#3.#

fit = lm(y~x)
summary(fit)