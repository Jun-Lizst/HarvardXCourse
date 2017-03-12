###########################################
# estimate the standard error of the linear model coefficients.
###########################################

# Take a random sample of the father.son heights data:
library(UsingR)
x = father.son$fheight
y = father.son$sheight

n = length(y)
N = 50

set.seed(1)
index = sample(n,N)
sampledat = father.son[index,]
x = sampledat$fheight
y = sampledat$sheight

betahat = lm(y~x)$coef
print(betahat)

#The formula for the standard error 
# SE(betahat) = sqrt(var(betahat))
# var(betahat) = sigma^2 (X^T X)^-1

# We will estimate or calculate each part of this equation 
# and then combine them.

# First, we want to estimate sigma^2, the variance of Y.


# As we have seen in the previous unit, 
# the random part of Y is only coming from epsilon, 
# because we assume X*beta is fixed. So we can try to estimate 
# the variance of the epsilons from the residuals, 
# the Yi minus the fitted values from the linear model.


# The fitted values (Y-hat) from a linear model can be obtained with:
fit = lm(y ~ x)
print(fit$fitted.values)

# What is the sum of the squared residuals 
# (where residuals are given by r_i = Y_i - Y-hat_i)?
X = model.matrix(~x)
N = nrow(X)
p = ncol(X)

XtXinv = solve(crossprod(X))
resid = y - X %*% XtXinv %*% crossprod(X,y)
ss = sum(resid^2)
print(ss)

# Our estimate of sigma^2 will be the sum of squared residuals 
# divided by (N - p), the sample size minus the number of terms in the model. 
# Since we have a sample of 50 and 2 terms in the model (an intercept and a slope), 
# our estimate of sigma^2 will be the sum of squared residuals divided by 48. 

sigma2 = ss/48
ses <- sqrt(diag(XtXinv)*sigma2)
