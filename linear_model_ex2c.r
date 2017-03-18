
dat <- read.csv("https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/femaleMiceWeights.csv")
stripchart(dat$Bodyweight ~ dat$Diet, vertical=TRUE, method="jitter", main="Bodyweight over Diet")

levels(dat$Diet)


Y <- dat$Bodyweight
X = model.matrix(~ Diet, data=dat)
head(X)

beta = solve(t(X) %*% X) %*% t(X) %*% Y
print(beta)


s <- split(dat$Bodyweight, dat$Diet)
mean(s$"chow") - mean(s$hf)

fit <- lm(Bodyweight ~ Diet, data=dat)
summary(fit)

ttest <- t.test(s$hf, s[["chow"]], var.equal=TRUE)
print(ttest$statistic) # = summary(fit)$coefficients[2,3]



nx = 5
ny = 7
X = cbind(rep(1,nx + ny),rep(c(0,1),c(nx, ny)))

print(t(X)%*%X)

solve((t(X)%*%X))


(1/5 + 1/7)