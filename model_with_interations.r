# Interactions and Contrasts

# Paper: Jonas O. Wolff & Stanislav N. Gorb, Radial arrangement of Janus-like setae 
#        permits friction control in spiders, Scientific Reports, 22 January 2013.

# Abstract: The hunting spider Cupiennius salei possesses hairy attachment pads 
#           (claw tufts) at its distal legs, consisting of directional branched setae. 
#           Friction of claw tufts on smooth glass was measured to reveal 
#           the functional effect of seta arrangement within the pad. 

# Dataset:  https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/spider_wolff_gorb_2013.csv
#           comparing the different frictional coefficients 
#           on the different legs of a spider.


url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/spider_wolff_gorb_2013.csv"
filename <- "spider_wolff_gorb_2013.csv"
install.packages("downloader")
library(downloader)
if (!file.exists(filename)) download(url, filename)

spider <- read.csv(filename, skip=1)
head(spider)

# Each measurement comes from one of our legs while 
# it is either pushing or pulling. So we have two variables:
table(spider$leg,spider$type)

# boxplot summarizing the measurements for each of the eight pairs

boxplot(spider$friction ~ spider$type * spider$leg, 
        col=c("grey90","grey40"), las=2, 
        main="Comparison of friction coefficients of different leg pairs")

# we can immediately see two trends:
# The pulling motion has higher friction than the pushing motion.
# The leg pairs to the back of the spider (L4 being the last) have higher pulling friction.

# the groups have different spread around their average, what we call within-group variance.
# somewhat of a problem for the kinds of linear models 
# assuming that around the population average values, 
# the errors ??[i] are distributed identically.

# The consequence of ignoring the different variances for the different groups 
# is that comparisons between those groups with small variances will be overly "conservative"
# (because the overall estimate of variance is larger than an estimate for just these groups)
# and comparisons between those groups with large variances will be overly confident.


# 1. A possibility is to transform the data with a function such as the log or sqrt.
# 2. Some alternative tests for comparing groups without transforming the values include: 
#     t-tests without the equal variance assumption using a "Welch" or 
#     "Satterthwaite approximation", or the Wilcoxon rank sum test.


# To remind ourselves how the simple two-group linear model looks, 
# we will subset the data to include only the L1 leg pair, and run lm:

spider.sub <- spider[spider$leg == "L1",]
fit <- lm(friction ~ type, data=spider.sub)
summary(fit)

(coefs <- coef(fit))
# > fit <- lm(friction ~ type, data=spider.sub)
# > summary(fit)
# 
# Call:
#   lm(formula = friction ~ type, data = spider.sub)
# 
# Residuals:
#      Min       1Q   Median       3Q      Max 
# -0.33147 -0.10735 -0.04941 -0.00147  0.76853 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
#   (Intercept)  0.92147    0.03827  24.078  < 2e-16 ***
#   typepush    -0.51412    0.05412  -9.499  5.7e-14 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.2232 on 66 degrees of freedom
# Multiple R-squared:  0.5776,	Adjusted R-squared:  0.5711 
# F-statistic: 90.23 on 1 and 66 DF,  p-value: 5.698e-14

# These two estimated coefficients are the mean of the pull observations (the first estimated coefficient) 
# and the difference between the means of the two groups (the second coefficient). We can show this with R code:

s <- split(spider.sub$friction, spider.sub$type)
mean(s[["pull"]])

# > mean(s[["pull"]])
# [1] 0.9214706

mean(s[["push"]]) - mean(s[["pull"]])

# > mean(s[["push"]]) - mean(s[["pull"]])
# [1] -0.5141176

# We can form the design matrix, which was used inside lm:
X <- model.matrix(~ type, data=spider.sub)
colnames(X)
head(X)
tail(X)

# > head(X)
# (Intercept) typepush
# 1           1        0
# 2           1        0
# 3           1        0
# 4           1        0
# 5           1        0
# 6           1        0
# > tail(X)
# (Intercept) typepush
# 63           1        1
# 64           1        1
# 65           1        1
# 66           1        1
# 67           1        1
# 68           1        1

# Make a plot of the XX matrix by putting a black block for the 1's and a white block for the 0's.

library(rafalib)
imagemat(X, main="Model matrix for linear model with one variable")


# Examining the estimated coefficients
set.seed(1) #same jitter in stripchart
stripchart(split(spider.sub$friction, spider.sub$type), 
           vertical=TRUE, pch=1, method="jitter", las=2, xlim=c(0,3), ylim=c(0,2))
a <- -0.25
lgth <- .1
library(RColorBrewer)
cols <- brewer.pal(3,"Dark2")
abline(h=0)

arrows(1+a,0,1+a,coefs[1],lwd=3,col=cols[1],length=lgth)
abline(h=coefs[1],col=cols[1])

arrows(2+a,coefs[1],2+a,coefs[1]+coefs[2],lwd=3,col=cols[2],length=lgth)
abline(h=coefs[1]+coefs[2],col=cols[2])
legend("right",names(coefs),fill=cols,cex=.75,bg="white")


# model with two variables
X = model.matrix(~ type + leg, data = spider)
colnames(X)

# > colnames(X)
# [1] "(Intercept)" "typepush"    "legL2"       "legL3"       "legL4"      

imagemat(X, main="Model matrix for linear model with two factors")

# First column is the intercept, and so it has 1's for all samples. 
# The second column has 1's for the push samples, and there are four groups of them.
# The third, fourth and fifth columns have 1's for the L2, L3 and L4 samples. 
# The L1 samples do not have a column, because L1 is the reference level for leg. 
# There is no pull column, because pull is the reference level for the type variable.

# To estimate coefficients for this model, we use lm with the formula ~ type + leg. 
# We'll save the linear model to fitTL standing for a fit with Type and Leg.

fitTL <- lm(friction ~ type + leg, data=spider)
summary(fitTL)


# > summary(fitTL)
# 
# Call:
#   lm(formula = friction ~ type + leg, data = spider)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -0.46392 -0.13441 -0.00525  0.10547  0.69509 
# 
# Coefficients:
#               Estimate Std. Error t value Pr(>|t|)    
#   (Intercept)  1.05392    0.02816  37.426  < 2e-16 ***
#   typepush    -0.77901    0.02482 -31.380  < 2e-16 ***
#   legL2        0.17192    0.04569   3.763 0.000205 ***
#   legL3        0.16049    0.03251   4.937 1.37e-06 ***
#   legL4        0.28134    0.03438   8.183 1.01e-14 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.2084 on 277 degrees of freedom
# Multiple R-squared:  0.7916,	Adjusted R-squared:  0.7886 
# F-statistic:   263 on 4 and 277 DF,  p-value: < 2.2e-16

(coefs <- coef(fitTL))

# model we are fitting above can be written as
#
# y[i] = b0 + b1*x1[i] + b2*x2[i] + b3*x3[i] + b4*x4[i] + e[i], i = 1,...,N
#
# with the xx all indicator variables denoting push or pull and which leg.
# For example, a push on leg 3 will have x1[i] and x3[i] equal to 1 and the rest would be 0.


# We can now form the matrix X and obtain the least square estimates with:
#
# Betahat = (X^TX)^-1 * X^T*Y

Y <- spider$friction
beta.hat <- solve(t(X) %*% X) %*% t(X) %*% Y
t(beta.hat)

# The values below agree with the output of lm.
# > t(beta.hat)
#      (Intercept)   typepush     legL2     legL3     legL4
#      ----------------------------------------------------
# [1,]    1.053915 -0.7790071 0.1719216 0.1604921 0.2813382


# Examining the estimated coefficients

spider$group <- factor(paste0(spider$leg, spider$type))
stripchart(split(spider$friction, spider$group), 
           vertical=TRUE, pch=1, method="jitter", las=2, xlim=c(0,11), ylim=c(0,2))
cols <- brewer.pal(5,"Dark2")
abline(h=0)
arrows(1+a,0,1+a,coefs[1],lwd=3,col=cols[1],length=lgth)
abline(h=coefs[1],col=cols[1])
arrows(3+a,coefs[1],3+a,coefs[1]+coefs[3],lwd=3,col=cols[3],length=lgth)
arrows(5+a,coefs[1],5+a,coefs[1]+coefs[4],lwd=3,col=cols[4],length=lgth)
arrows(7+a,coefs[1],7+a,coefs[1]+coefs[5],lwd=3,col=cols[5],length=lgth)
arrows(2+a,coefs[1],2+a,coefs[1]+coefs[2],lwd=3,col=cols[2],length=lgth)
segments(3+a,coefs[1]+coefs[3],4+a,coefs[1]+coefs[3],lwd=3,col=cols[3])
arrows(4+a,coefs[1]+coefs[3],4+a,coefs[1]+coefs[3]+coefs[2],lwd=3,col=cols[2],length=lgth)
segments(5+a,coefs[1]+coefs[4],6+a,coefs[1]+coefs[4],lwd=3,col=cols[4])
arrows(6+a,coefs[1]+coefs[4],6+a,coefs[1]+coefs[4]+coefs[2],lwd=3,col=cols[2],length=lgth)
segments(7+a,coefs[1]+coefs[5],8+a,coefs[1]+coefs[5],lwd=3,col=cols[5])
arrows(8+a,coefs[1]+coefs[5],8+a,coefs[1]+coefs[5]+coefs[2],lwd=3,col=cols[2],length=lgth)
legend("right",names(coefs),fill=cols,cex=.75,bg="white")


# The fitted means above for each group, derived from the fitted coefficients, do not line up with 
#  those we obtain from simply taking the average from each of the eight possible groups. 
# The reason is that our model uses five coefficients, instead of eight. 
#  We are assuming that the effects are additive. However, as we demonstrate in more detail below, 
#  this particular dataset is better described with a model including interactions.

s <- split(spider$friction, spider$group)
mean(s[["L1pull"]])
coefs[1]
mean(s[["L1push"]])
coefs[1] + coefs[2]

# > s <- split(spider$friction, spider$group)
# > mean(s[["L1pull"]])
# [1] 0.9214706

# > coefs[1]
# (Intercept) 
# 1.053915 

# > mean(s[["L1push"]])
# [1] 0.4073529

# > coefs[1] + coefs[2]
# (Intercept) 
# 0.2749082 


# Here we can demonstrate that the push vs. pull estimated coefficient, 
#  coefs[2], is a weighted average of the difference of the means for each group.
#  Furthermore, the weighting is determined by the sample size of each group. 
#  The math works out simply here because the sample size is equal for the push and pull 
#  subgroups within each leg pair. If the sample sizes were not equal for push and pull 
#  within each leg pair, the weighting is more complicated but uniquely determined 
#  by a formula involving the sample size of each subgroup, the total sample size, and the number of coefficients.

print(coefs[2])

means <- sapply(s, mean)
print(means)

##the sample size of push or pull groups for each leg pair
ns <- sapply(s, length)[c(1,3,5,7)]
print(ns)

(w <- ns/sum(ns))
print(w)
sum(w * (means[c(2,4,6,8)] - means[c(1,3,5,7)]))

# same as 
print(coefs[2])


#Contrasting coefficients
#
# Sometimes, the comparison we are interested in is represented directly by a single coefficient 
# in the model, such as the push vs pull difference, which was coefs[2] above. 
# Sometimes, we want to make a comparison which is not a single coefficient, 
# but a combination of coefficients, which is called a contrast. 
# To introduce the concept of contrasts, first consider the comparisons which we can read off from 
# the linear model summary:
  
print(coefs)

# Here we have:
#   1. the intercept estimate,
#   2. the push vs. pull estimated effect across all leg pairs
#   3. estimates for the L2 vs. L1 effect, the L3 vs. L1 effect, and the L4 vs. L1 effect.


# What if we want to compare two groups and one of those groups is not L1? 
# The solution to this question is to use contrasts.

# A contrast is a combination of estimated coefficient: c?????^, 
#   where c is a column vector with as many rows as the number of coefficients in the model. 
#   If c has a 0 for one or more of its rows, then the corresponding estimated coefficients 
#   in ??^ are not involved in the contrast.


# If we want to compare leg pairs L3 and L2, this is equivalent to contrasting 
# two coefficients from the linear model because, in this contrast, 
# the comparison to the reference level L1 cancels out:
  
#   (L3???L1)???(L2???L1)=L3???L2 


# A way to make contrasts of two groups is to use the contrast function from the contrast package.
#  We just need to specify which groups we want to compare. 
#  We have to pick one of pull or push types, although the answer will not differ, as we will see below.

# install.packages("contrast") #Available from CRAN

library(contrast) 
L3vsL2 <- contrast(fitTL,list(leg="L3",type="pull"),list(leg="L2",type="pull"))
L3vsL2


## lm model parameter contrast
## 
##     Contrast       S.E.      Lower      Upper     t  df Pr(>|t|)
##  -0.01142949 0.04319685 -0.0964653 0.07360632 -0.26 277   0.7915


# We can show that the least squares estimates of a linear 
# combination of coefficients is the same linear combination of the estimates.
# Thus the effect size estimate is just the difference between two estimated coefficients.

# The contrast vector used by contrast is stored as a variable called X 
# within the resulting object (not to be confused with the design matrix).

coefs[4] - coefs[3]

(cT <- L3vsL2$X)

# > (cT <- L3vsL2$X)
#   (Intercept) typepush legL2 legL3 legL4
#    -------------------------------------
# 1           0        0    -1     1     0
# attr(,"assign")
# [1] 0 1 2 2 2
# attr(,"contrasts")
# attr(,"contrasts")$type
# [1] "contr.treatment"
# 
# attr(,"contrasts")$leg
# [1] "contr.treatment"

cT %*% coefs

# What about the standard error and t-statistic? As before, 
# the t-statistic is the estimate divided by the standard error. 
# The standard error of the contrast estimate is formed by multiplying the contrast 
# vector c on either side of the estimated covariance matrix, ??^, 
# our estimate for var(??^):
#
#  sqrt(c?????^c)
#
# where the covariance of the coefficients is :
#
#   ??=??2(X???X)^???1
#
# We estimate ??2 with the sample estimate ??^2 described above and obtain:
#

Sigma.hat <- sum(fitTL$residuals^2)/(nrow(X) - ncol(X)) * solve(t(X) %*% X)
print(Sigma.hat)

signif(Sigma.hat, 2)

sqrt(cT %*% Sigma.hat %*% t(cT))

L3vsL2$SE


# Linear Model with Interactions

# In previous linear model, we assumed that the push vs. pull effect was the same for all of the leg pairs (the same orange arrow). You can easily see that this does not capture the trends in the data that well. That is, the tips of the arrows did not line up perfectly with the group averages. For the L1 leg pair, the push vs. pull estimated coefficient was too large, and for the L3 leg pair, the push vs. pull coefficient was somewhat too small.
# Interaction terms will help us overcome this problem by introducing additional coefficients to compensate for differences in the push vs. pull effect across the 4 groups. As we already have a push vs. pull term in the model, we only need to add three more terms to have the freedom to find leg-pair-specific push vs. pull differences. As we will see, interaction terms are added to the design matrix by multiplying the columns of the design matrix representing existing terms.

X <- model.matrix(~ type + leg + type:leg, data=spider)
colnames(X)

imagemat(X, main="Model matrix for linear model with interactions")


fitX <- lm(friction ~ type + leg + type:leg, data=spider)

summary(fitX)
(coefs <- coef(fitX))

# Examining the estimated coefficients

# Here is where the plot with arrows really helps us interpret the coefficients. 
# The estimated interaction coefficients (the yellow, brown and silver arrows) allow 
# leg-pair-specific differences in the push vs. pull difference.  The orange arrow now represents 
# the estimated push vs. pull difference only for the reference leg pair, which is L1. 
# If an estimated interaction coefficient is large, this means that the push vs. pull difference for that leg pair 
# is very different than the push vs. pull difference in the reference leg pair.
# 
# The tips of the arrowheads are exactly equal to the group means.

stripchart(split(spider$friction, spider$group), 
           vertical=TRUE, pch=1, method="jitter", las=2, xlim=c(0,11), ylim=c(0,2))
cols <- brewer.pal(8,"Dark2")
abline(h=0)
arrows(1+a,0,1+a,coefs[1],lwd=3,col=cols[1],length=lgth)
abline(h=coefs[1],col=cols[1])
arrows(2+a,coefs[1],2+a,coefs[1]+coefs[2],lwd=3,col=cols[2],length=lgth)
arrows(3+a,coefs[1],3+a,coefs[1]+coefs[3],lwd=3,col=cols[3],length=lgth)
arrows(5+a,coefs[1],5+a,coefs[1]+coefs[4],lwd=3,col=cols[4],length=lgth)
arrows(7+a,coefs[1],7+a,coefs[1]+coefs[5],lwd=3,col=cols[5],length=lgth)
#now the interactions:
segments(3+a,coefs[1]+coefs[3],4+a,coefs[1]+coefs[3],lwd=3,col=cols[3])
arrows(4+a,coefs[1]+coefs[3],4+a,coefs[1]+coefs[3]+coefs[2],lwd=3,col=cols[2],length=lgth)
arrows(4+a,coefs[1]+coefs[2]+coefs[3],4+a,coefs[1]+coefs[2]+coefs[3]+coefs[6],lwd=3,col=cols[6],length=lgth)

segments(5+a,coefs[1]+coefs[4],6+a,coefs[1]+coefs[4],lwd=3,col=cols[4])
arrows(6+a,coefs[1]+coefs[4],6+a,coefs[1]+coefs[4]+coefs[2],lwd=3,col=cols[2],length=lgth)
arrows(6+a,coefs[1]+coefs[4]+coefs[2],6+a,coefs[1]+coefs[4]+coefs[2]+coefs[7],lwd=3,col=cols[7],length=lgth)

segments(7+a,coefs[1]+coefs[5],8+a,coefs[1]+coefs[5],lwd=3,col=cols[5])
arrows(8+a,coefs[1]+coefs[5],8+a,coefs[1]+coefs[5]+coefs[2],lwd=3,col=cols[2],length=lgth)
arrows(8+a,coefs[1]+coefs[5]+coefs[2],8+a,coefs[1]+coefs[5]+coefs[2]+coefs[8],lwd=3,col=cols[8],length=lgth)
legend("right",names(coefs),fill=cols,cex=.75,bg="white")

# Contrasts
#
# To combine estimated coefficients from the model using contrasts. For some simple cases, we can use the contrast package.
# Suppose we want to know the push vs. pull effect for the L2 leg pair samples. We can see from the arrow plot that this is the orange arrow plus the yellow arrow.
# We can also specify this comparison with the contrast function:

library(contrast) ##Available from CRAN
L2push.vs.pull <- contrast(fitX, list(leg="L2", type = "push"), list(leg="L2", type = "pull"))
L2push.vs.pull
coefs[2] + coefs[6] ##we know this is also orange + yellow arrow





























  


  



































































