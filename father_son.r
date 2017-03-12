###########################################
# Father and son heights model estimation
###########################################

# assume that this is the entire population:
library(UsingR)
x = father.son$fheight
y = father.son$sheight
n = length(y)

# Run a Monte Carlo simulation in which 
# we take a sample size of 50 over and over again.
betahat <- replicate(
  1000,
  {
    index <- sample(n,50)
    sampledat = father.son[index,]
    x = sampledat$fheight
    y = sampledat$sheight
    return(lm(y~x)$coef)
  }
)

# Get estimates in two columns
betahat <- t(betahat) 

mypar(1,2)
qqnorm(betahat[,1])
qqline(betahat[,1])

qqnorm(betahat[,2])
qqline(betahat[,2])

# correlation of 2 estimates betahat[,1] and betahat[,2]
cor(betahat[,1],betahat[,2])
## [1] -0.9992293


# Describe the variance-covariance matrix. 
# The covariance of two random variables is defined as follows:
mean( (betahat[,1]-mean(betahat[,1] ))* (betahat[,2]-mean(betahat[,2])))
## [1] -1.035291


## Estimate the errors
n = nrow(father.son)
N = 50
index = sample(n,N)
sampledat = father.son[index,]

x = sampledat$fheight
y = sampledat$sheight

X = model.matrix(~x)
N = nrow(X)
p = ncol(X)

XtXinv = solve(crossprod(X))
resid = y - X %*% XtXinv %*% crossprod(X,y)

s = sqrt( sum(resid^2)/(N-p))
ses <- sqrt(diag(XtXinv))*s 
print(ses)
## (Intercept)           x 
##   8.3899781   0.1240767

# Compare with the output of lm
summary(lm(y~x))$coef[,2]
## (Intercept)           x 
##   8.3899781   0.1240767
Th# They are identical because they are doing the same thing. 

#Compare with the Monte Carlo results:
apply(betahat,2,sd)
## (Intercept)           x 
##   8.3817556   0.1237362

# CLT and t-distribution
# We have shown how we can obtain standard errors for our estimates. 
# However to perform inference we need to know the distribution 
# of these random variables. The reason we went through the 
# effort to compute the standard errors is because the CLT applies in linear models. 

# If N is large enough, then the LSE will be normally distributed with mean ?? and standard errors as described. 
# For small samples, if the ?? are normally distributed,
# then the ??^????? follow a t-distribution. 
# We do not derive here, but the results are extremely useful since it is how we 
# construct p-values and confidence intervals in the context of linear models.







  

