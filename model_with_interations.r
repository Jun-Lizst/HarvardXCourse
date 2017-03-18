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

