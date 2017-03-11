g = 9.8 ## meters per second
h0 = 56.67
v0 = 0
n = 25
tt = seq(0,3.4,len=n) ##time in secs, t is a base function

X = cbind(1,tt,tt^2)
A = solve(crossprod(X))%*%t(X)

# Generate random data, y, based on model 
# y = h0 + v0*t + 0.5* g* t^2 + e
# Then compute the estimate of g, name ghat.
ghat = function()
{
  y = h0 + v0 *tt - 0.5* g*tt^2 + rnorm(n,sd=1)
  -2 * (A %*% y)[3]
}

# Use the function ghat() above in conjunction with the 
# function replicate() to generate 100,000 
# Monte Carlo simulated datasets. 
# For each dataset compute an estimate of g 
# (remember to multiply by -2) and set the seed to 1.
set.seed(1)
ghats = replicate(100000, ghat())
ghat_se = sd(ghats)

print(ghat_se)
