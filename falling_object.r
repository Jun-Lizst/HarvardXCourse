
set.seed(1)
N = 10000
h0 = 56.67
v0 = 0
g = 9.8 ## meters per second

n <- 25
ts <- seq(0,3.4,len=n) ## time in secs, t is built-in function
X <-cbind(1,tt,tt^2)

## A = X'X^-1 X'
A <- solve(crossprod(X)) %*% t(X)

betahat<-replicate (
  N,
  {
    y <- h0 + v0*ts  - 0.5*g*ts^2 + rnorm(n,sd=1)
    betahats <- A%*%y
    return(betahats[3])
  }
)

print(head(betahat))

library(rafalib)
mypar(1,2)
hist(betahat)

qqnorm(betahat)
qqline(betahat)

print(round(mean(betahat),1))
print(sd(betahat))
