g = 9.8
n = 25
ti = seq(0, 3.4, len=n) 
f = 56.67 + 0*ti -0.5*g*ti^2
y = f + rnorm(n , sd=1)
plot(ti, y, xlab="time in secs", ylab="Distance in meters")
lines(ti, f, col=2)

rss = function(beta0, beta1, beta2)
{
  r = y - (beta0 + beta1*ti + beta2*ti^2)
  sum(r^2)
}

beta2s = seq(-10, 0, len=100)
RSS = sapply(beta2s, rss, beta0 = 55, beta1 = 0)
plot(beta2s, RSS, type="l")

RSS = sapply(beta2s, rss, beta0 = 65, beta1 = 0)
lines(beta2s, RSS, type="l",col=3)

ti2 = ti^2
fit = lm(y~ti + ti2)
summary(fit)