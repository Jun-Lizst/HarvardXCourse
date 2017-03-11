# In the father and son height examples we have randomness because 
# we have a random sample of father and son pairs. 
# However assume that this is the entire population:

library(UsingR)
x = father.son$fheight
y = father.son$sheight
n = length(y)

# Let's run a Monte Carlo simulation in which we take a sample of size 50 
# over and over again. 
# Here is how we obtain one sample:
slope = function()
{
  N =  50
  index = sample(n,N)
  sampledat = father.son[index,]
  x = sampledat$fheight
  y = sampledat$sheight
  betahat =  lm(y~x)$coef
  betahat[2]
}

# Use the function replicate to take 10,000 samples.
# What is the standard error of the slope estimate? 
# That is, calculate the standard deviation of the estimate 
# from many random samples. set the seed to 1.
slopes = replicate(10000, slope())
slope_se  = sd(slopes)
print(slope_se)


# The covariance of two lists of numbers X=X1,...,Xn and Y=Y1,...,Yn 
# is mean( (Y - mean(Y))*(X-mean(X) ) ).

# Which of the following is closest to the covariance 
# between father heights and son heights

x = father.son$fheight
y = father.son$sheight
xycovar = mean( (y - mean(y))*(x-mean(x) ) )
print(xycovar)