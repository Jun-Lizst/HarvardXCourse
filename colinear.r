# http://genomicsclass.github.io/book/pages/collinearity.html

x = rbind(
  c(1, 0, 0, 0, 0),
  c(1, 0, 0, 0, 1),
  c(1, 1, 0, 1, 0),
  c(1, 1, 0, 1, 1),
  c(1, 0, 1, 1, 0),
  c(1, 0, 1, 1, 1))

qr(x)$rank

x = rbind(
  c(1, 0, 0, 0),
  c(1, 0, 1, 0),
  c(1, 1, 0, 0),
  c(1, 1, 1, 1))


############## excercise ################
(sex <- factor(rep(c("female","male"),each=4)))
(trt <- factor(c("A","A","B","B","C","C","D","D")))

(X <- model.matrix( ~ sex + trt))

(qr(X)$rank)

(Y <- 1:8)

makeYstar <- function(a,b) 
{
  return(Y - X[,2] * a - X[,5] * b)
}

fitTheRest <- function(a,b) {
  Ystar <- makeYstar(a,b)
  Xrest <- X[,-c(2,5)]
  betarest <- solve(t(Xrest) %*% Xrest) %*% t(Xrest) %*% Ystar
  residuals <- Ystar - Xrest %*% betarest
  sum(residuals^2)
}

fitTheRest(1, 2) 

########################

expand.grid(1:3,1:3)

# We can run fitTheRest on a grid of values, using the following code (the Vectorize() 
# is necessary as outer() requires only vectorized functions):
  
(betas = expand.grid(-2:8,-2:8))
(rss = apply(betas,1,function(x) fitTheRest(x[1],x[2])))
plot(rss)

# we can also visualize the sum of squared residuals over our grid with the imagemat() 
# function from rafalib:

library(rafalib)

## plot the pairs what are minimum
(themin=min(rss))
plot(betas[which(rss==themin),])

# There is clearly not a single beta which optimizes the least squares equation, 
# due to collinearity, but an infinite line of solutions which produce 
# an identical sum of squares values.

##################################
# Excercise on spider datatset 
##################################

url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/spider_wolff_gorb_2013.csv"
filename <- "spider_wolff_gorb_2013.csv"
library(downloader)
if (!file.exists(filename)) download(url, filename)
(spider <- read.csv(filename, skip=1))

fit <- lm(friction ~ type + leg, data=spider)
fit

# The solution we are interested in solving is:
(betahat <- coef(fit))

(Y <- matrix(spider$friction, ncol=1))
(X <- model.matrix(~ type + leg, data=spider))

# R*Beta = Q^T*Y

(QR <- qr(X))
(Q <- qr.Q( QR ))
(R <- qr.R( QR ))
(betahat <- backsolve(R, crossprod(Q,Y) ) )

crossprod(Q,Y)

# 1. Q[1,1]
# 2. R[1,1]
























