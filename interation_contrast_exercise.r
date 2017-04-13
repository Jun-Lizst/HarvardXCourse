# http://genomicsclass.github.io/book/pages/interactions_and_contrasts.html

url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/spider_wolff_gorb_2013.csv"
filename <- "spider_wolff_gorb_2013.csv"
library(downloader)
if (!file.exists(filename)) download(url, filename)
spider <- read.csv(filename, skip=1)

# Suppose that we notice that the within-group variances for the groups with smaller frictional coefficients are 
# generally smaller, and so we try to apply a transformation to the frictional coefficients to make the 
# within-group variances more constant.

# Add a new variable log2friction to the spider dataframe:
spider$log2friction <- log2(spider$friction)


# The 'Y' values now look like:
boxplot(log2friction ~ type*leg, data=spider)

#Run a linear model of log2friction with type, leg and interactions 
# between type and leg.

fit = lm(log2friction ~ type*leg, data=spider)
summary(fit)

# > summary(fit)
# 
# Call:
#   lm(formula = log2friction ~ type * leg, data = spider)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -1.35902 -0.19193  0.00596  0.16315  1.33090 
# 
# Coefficients:
#                  Estimate Std. Error t value Pr(>|t|)    
#   (Intercept)    -0.16828    0.06613  -2.545 0.011487 *  
#   typepush       -1.20656    0.09352 -12.901  < 2e-16 ***
#   legL2           0.34681    0.11952   2.902 0.004014 ** 
#   legL3           0.48999    0.08505   5.762 2.24e-08 ***
#   legL4           0.64668    0.08995   7.189 6.20e-12 ***
#   typepush:legL2  0.09967    0.16903   0.590 0.555906    
#   typepush:legL3 -0.54075    0.12027  -4.496 1.02e-05 ***
#   typepush:legL4 -0.46920    0.12721  -3.689 0.000272 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

# 1. what is the t-value for the interation of type push and leg 4?
## -3.689
# if this t-value is sufficiently large, we would reject the null hypothesis that  
# the push vs pull effect on log2(friction) is the same in L4 as in L1.


# 2.What is the F-value for all of the type:leg interaction terms, in an analysis of variance?
anova(fit)

# > anova(fit)
# Analysis of Variance Table
# 
# Response: log2friction
#              Df  Sum Sq Mean Sq  F value    Pr(>F)    
#   type        1 164.709 164.709 1107.714 < 2.2e-16 ***
#   leg         3   7.065   2.355   15.838 1.589e-09 ***
#   type:leg    3   4.774   1.591   10.701 1.130e-06 ***
#   Residuals 274  40.742   0.149                       
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1


# If this value is sufficiently large, we would reject the null hypothesis that the push vs pull effect 
# on log2(friction) is the same for all leg pairs.

#3. What is the L2 vs L1 estimate in log2friction for the pull samples?
# 0.34681

# 4. What is the L2 vs L1 estimate in log2friction for the push samples? 
# Remember, because of the interaction terms, this is not the same as the L2 vs L1 difference 
# for the pull samples. If you're not sure use the contrast() function. 
# Another hint: consider the arrows plot for the model with interactions.

library(contrast) ##Available from CRAN
L2push.vs.L1push <- contrast(fit,
                           list(leg="L2", type = "push"), 
                           list(leg="L1", type = "push"))

L2push.vs.L1push



#5. In this last question, we will use Monte Carlo techniques to observe the distribution of the 
# ANOVA's "F-value" under the null hypothesis, that there are no differences between groups.


# Suppose we have 4 groups, and 10 samples per group, so 40 samples overall:
#   
N <- 40
p <- 4
group <- factor(rep(1:p,each=N/p))
X <- model.matrix(~ group)

# We will show here how to calculate the "F-value", and then we will use random number 
# to observe the distribution of the F-value under the null hypothesis.


# The F-value is the mean sum of squares explained by the terms of interest (in our case, the 'group' terms) 
# divided by the mean sum of squares of the residuals of a model including the terms of interest.


# So it is the explanatory power of the terms divided by the leftover variance.


# Intuitively, if this number is large, it means that the group variable explains a lot of the variance in the data, 
# compared to the amount of variance left in the data after using group information. 

# We will calculate these values exactly here:

# 1. First generate some random, null data, where the mean is the same for all groups:

Y <- rnorm(N,mean=42,7)

# The base model we wil compare against is simply Y-hat = mean(Y), which we will call mu0,
# and the initial sum of squares is the Y values minus mu0:

mu0 <- mean(Y)
initial.ss <- sum((Y - mu0)^2)

# We then need to calculate the fitted values for each group, 
# which is simply the mean of each group, and the residuals from this model, 
# which we will call "after.group.ss" for the sum of squares after using the group information:
  
s <- split(Y, group)

sapply(
        s, 
        function(x) 
        {
          sum((x - mean(x))^2)
        }
      )

after.group.ss <- sum(sapply(s, function(x) sum((x - mean(x))^2)))


(group.ss <- initial.ss - after.group.ss)


(group.ms <- group.ss / (p - 1))

(after.group.ms <- after.group.ss / (N - p))

# The F-value is simply the ratio of these mean sum of squares.

(f.value <- group.ms / after.group.ms)


# Set the seed to 1, set.seed(1) then calculate the F-value for 1000 random versions of Y. 
# What is the mean of these F-values?

set.seed(1) 
compute_F = function() 
{
  Y <- rnorm(N,mean=42,7)
  mu0 <- mean(Y)
  initial.ss <- sum((Y - mu0)^2)
  s <- split(Y, group)
  after.group.ss <- sum(sapply(s, function(x) sum((x - mean(x))^2)))
  group.ss <- initial.ss - after.group.ss
  group.ms <- group.ss / (p - 1)
  after.group.ms <- after.group.ss / (N - p)
  f.value <- group.ms / after.group.ms
  return(f.value) 
}

f.values <- replicate(1000, compute_F())
mean(f.values)
# 1.069771

hist(f.values, col="grey", border="white", breaks=50, freq=FALSE)

# Overlay the theoretical F-distribution, with parameters df1=p - 1, df2=N - p.
xs <- seq(from=0,to=6,length=100)
lines(xs, df(xs, df1 = p - 1, df2 = N - p), col="red")
?df

