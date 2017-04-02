# http://genomicsclass.github.io/book/pages/interactions_and_contrasts.html

# Suppose we have an experiment with two species A and B, and two conditions: control and treated.
species <- factor(c("A","A","B","B"))
condition <- factor(c("control","treated","control","treated"))

# And we will use a formula of '~ species + condition'.
# The model matrix is then:

X = model.matrix(~ species + condition)
X

# Contrasts Exercises #1

# Suppose we want to build a contrast of coefficients for the above experimental design.

# You can either figure this question out through logic, by looking at the design matrix, or using the contrast() function from the contrast library. 
# If you have not done so already, you should download the contrast library. The contrast vector is returned as contrast(...)$X.

# What should the contrast vector be, for the contrast of (species=B and condition=control) vs (species=A and condition=treatment)? 
# Assume that the beta vector from the model fit by R is: Intercept, speciesB, conditiontreated.
#
# > X
#   (Intercept) speciesB conditiontreated
# 1           1        0                0
# 2           1        0                1
# 3           1        1                0
# 4           1        1                1

# spider dataset like this:
url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/spider_wolff_gorb_2013.csv"
filename <- "spider_wolff_gorb_2013.csv"
library(downloader)
if (!file.exists(filename)) download(url, filename)
spider <- read.csv(filename, skip=1)

head(spider)


# Suppose we build a model using two variables: ~ type + leg.
X <- model.matrix(~ type + leg, data=spider)
colnames(X)
head(X)
print(X)

library(rafalib)
imagemat(X, main="Model matrix for linear model with one variable")

s <- split(spider$friction, spider$group)
mean(s[["L1pull"]])
mean(s[["L1push"]])

fitTL <- lm(friction ~ type + leg, data=spider)
summary(fitTL)

(coefs <- coef(fitTL))
coefs[1] + coefs[2]

## Estimate beta.hat (i.e. coefs) manualy 

Y <- spider$friction
X <- model.matrix(~ type + leg, data=spider)
beta.hat <- solve(t(X) %*% X) %*% t(X) %*% Y
t(beta.hat)


means <- sapply(s, mean)

class(s)
means

# Push vs. pull estimated coefficient, coefs[2], is a weighted average of the difference of the means for each group
means <- sapply(s, mean)

##the sample size of push or pull groups for each leg pair
ns <- sapply(s, length)[c(1,3,5,7)]

# weights 
(w <- ns/sum(ns))

# weighted average of the difference of the means for each group
sum(w * (means[c(2,4,6,8)] - means[c(1,3,5,7)]))

## weighted average of the difference of the means for each group
coefs[2]


# A contrast is a combination of estimated coefficient: c?????^

# To compare leg pairs L3 and L2, this is equivalent to contrasting two coefficients 
# from the linear model because, in this contrast, the comparison to the reference level L1 cancels out:

#   (L3???L1)???(L2???L1)=L3???L2 

library(contrast)
L3vsL2 <- contrast(fitTL,list(leg="L3",type="pull"),list(leg="L2",type="pull"))
L3vsL2

coefs[4] - coefs[3]


# What is the t-value for the contrast of leg pair L4 vs leg pair L2?
L4vsL2 <- contrast(fitTL,list(leg="L4",type="pull"),list(leg="L2",type="pull"))
L4vsL2


(cT <- L4vsL2$X)
cT %*% coefs

# the t-statistic is the estimate divided by the standard error. 
# The standard error of the contrast estimate is formed by multiplying the contrast vector C 
# on either side of the estimated covariance matrix ?? (our estimate for var(??^))

# se = sqrt(C*??*CT)
#   Where ??=??2(X???X)???1

# We estimate ??2 with the sample estimate ??^2 described above and obtain:
X <- model.matrix(~ type + leg, data=spider)

(sigma.sq.hat = sum(fitTL$residuals^2)/(nrow(X) - ncol(X)))

#??=??2(X???X)???1
(Sigma <- sigma.sq.hat  * solve(t(X) %*% X))

#Our contrast matrix is:
(C <- matrix(c(0,0,-1,0,1),1,5))

# Using Sigma, what is Cov(beta.hat_L4, beta.hat_L2)?
# 0.0006389179
Sigma[3,5]

# Confirm that 
# sqrt(Var(beta.hat_L4 - beta.hat_L2)) = sqrt(Var(beta.hat_L4) + Var(beta.hat_L2) - 2 Cov(beta.hat_L4, beta.hat_L2))
# is equal to
# sqrt(C %*% Sigma %*% t(C))
# is equal to the standard error from the contrast() for the leg L4 vs L2 difference.

sqrt(C %*% Sigma %*% t(C))
# > sqrt(C %*% Sigma %*% t(C))
#            [,1]
# [1,] 0.04462392
 
sqrt(Sigma[5,5] + Sigma[3,3] - 2 * Sigma[5,3])
# > sqrt(Sigma[5,5] + Sigma[3,3] - 2 * Sigma[5,3])
# [1] 0.04462392
  
L4vsL2 <- contrast(fitTL,list(leg="L4",type="pull"),list(leg="L2",type="pull"))
L4vsL2

# > L4vsL2
# lm model parameter contrast
#   Contrast       S.E.      Lower     Upper    t  df Pr(>|t|)
#  0.1094167 0.04462392 0.02157158 0.1972618 2.45 277   0.0148

install.packages('gtools')
#load library
library(gtools)

#########################################################

X <- model.matrix(~ type + leg + type:leg, data=spider)
colnames(X)
imagemat(X, main="Model matrix for linear model with interactions")

fitX <- lm(friction ~ type + leg + type:leg, data=spider)
summary(fitX)


## Contrasts
library(contrast) ##Available from CRAN
L2push.vs.pull <- contrast(fitX,
                   list(leg="L2", type = "push"), 
                   list(leg="L2", type = "pull"))
L2push.vs.pull

#########################################################
### Differences of differences
#########################################################
# The question of whether the push vs. pull difference is different in L2 compared to L1, is answered by a single term in the model: the typepush:legL2 estimated coefficient

# Suppose we want to know if the push vs. pull difference is different in L3 compared to L2.

library(multcomp) ##Available from CRAN
C <- matrix(c(0,0,0,0,0,-1,1,0), 1)
L3vsL2interaction <- glht(fitX, linfct=C)
summary(L3vsL2interaction)


################################################
## Analysis of variance
################################################

# Suppose that we want to know if the push vs. pull difference is different across leg pairs in general. We do not want to compare any two leg pairs in particular, but rather we want to know if the three interaction terms which represent differences in the push vs. pull difference across leg pairs are larger than we would expect them to be if the push vs pull difference was in fact equal across all leg pairs.

anova(fitX)

mu0 <- mean(spider$friction)
(initial.ss <- sum((spider$friction - mu0)^2))

N <- nrow(spider)
(N - 1) * var(spider$friction)

s <- split(spider$friction, spider$type)
after.type.ss <- sum( sapply(s, function(x) {
  residual <- x - mean(x) 
  sum(residual^2)
  }) )


(type.ss <- initial.ss - after.type.ss)
sum(sapply(s, length) * (sapply(s, mean) - mu0)^2)

# Finally, there is a column which lists the F value. The F value is the mean of squares for the inclusion of the terms of interest (the sum of squares divided by the degrees of freedom) divided by the mean squared residuals (from the bottom row):


  

