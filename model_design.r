# Expressing design formula in R
# http://genomicsclass.github.io/book/pages/expressing_design_formula.html

# The Design Matrix
#
# formula and model.matrix are used to produce design matrices 
# aka model matrices.

# Say model Y[i] = beta0 + beta1*x[i] + e, i = 1, 2, ...,n
#
#   Y[i] the weights
#   x[i] = 1 when mouse[i] receives high fat diet 
#   x a indicator variable to indicate 
#     if the experimential subject had a certain characteristics or not.

#        ( y[1] )         ( 1  x[1] )                  ( e[1] ) 
#        ( y[2] )         ( 1  x[2] )   (  beta0  )    ( e[2] ) 
#  Y  =  (  ..  )     =   ( .       ) x (         )  + (  ..  )
#        (  ..  )         ( .       )   (  beta1  )    (  ..  )
#        (  ..  )         ( .       )                  (  ..  )
#        ( y[n] )         ( 1  x[n] )                  ( e[n] ) 

# Or simply: 
#
#                 Y = X*BETA + e
#
# The design matrix is the matrix XX.

# Choice of design
#
# The choice of design matrix encodes which coefficients will be fit in the model,
# as well as the inter-relationship between the samples.

# Suppose we have two groups, control and high fat diet, with two samples each.
# For illustrative purposes, we will code these with 1 and 2 respectively.
# We should first use factor to encode these values so that they dont get interpreted numerically.
# Values get interpreted as different levels of a factor. 

group <- factor( c(1,1,2,2) )

# We can then use the paradigm ~ group to, say, model on the variable group.

# equivalence model.matrix(formula(~ group))
model.matrix(~ group)

# > model.matrix(~ group)
#   (Intercept) group2
# 1           1      0
# 2           1      0
# 3           1      1
# 4           1      1
# attr(,"assign")
# [1] 0 1
# attr(,"contrasts")
# attr(,"contrasts")$group
# [1] "contr.treatment"
#
# We want the second column to have only 0 and 1, indicating group membership.

# Names of the levels are irrelevant to model.matrix and lm. 
# All that matters is the order. 
# The below code produces the same design matrix

group <- factor(c("control","control","highfat","highfat"))
model.matrix(~ group)

# > model.matrix(~ group)
#   (Intercept) grouphighfat
# 1           1            0
# 2           1            0
# 3           1            1
# 4           1            1
# attr(,"assign")
# [1] 0 1
# attr(,"contrasts")
# attr(,"contrasts")$group
# [1] "contr.treatment"

# More groups
# Using the same formula, we can accommodate modeling more groups. 
# Suppose we have a third diet:

group <- factor(c(1,1,2,2,3,3))
model.matrix(~ group)

# > model.matrix(~ group)
#   (Intercept) group2 group3
# 1           1      0      0
# 2           1      0      0
# 3           1      1      0
# 4           1      1      0
# 5           1      0      1
# 6           1      0      1
# attr(,"assign")
# [1] 0 1 1
# attr(,"contrasts")
# attr(,"contrasts")$group
# [1] "contr.treatment"
# 
# Now we have a third column which specifies which samples belong to the third group.  

# An alternate formulation of design matrix is possible by specifying + 0 in the formula:
group <- factor(c(1,1,2,2,3,3))
model.matrix(~ group + 0)

# > model.matrix(~ group + 0)
#   group1 group2 group3
# 1      1      0      0
# 2      1      0      0
# 3      0      1      0
# 4      0      1      0
# 5      0      0      1
# 6      0      0      1
# attr(,"assign")
# [1] 1 1 1
# attr(,"contrasts")
# attr(,"contrasts")$group
# [1] "contr.treatment"
#
# This group now fits a separate coefficient for each group. 

# More variables 
# 
# To perform experiments with more than one variable (diet)
# Assume we are interested in the effect of diet and the difference in sexes. 
# In this case, we have four possible groups:

diet <- factor(c(1,1,1,1,2,2,2,2))
sex <- factor(c("f","f","m","m","f","f","m","m"))
table(diet,sex)

# > table(diet,sex)
#     sex
# diet f m
#    1 2 2
#    2 2 2

# If we assume that the diet effect is the same for males and females 
# (this is an assumption), then our linear model is: 

#   y[i] = b0 + b1*x1[i] + b1*x2[i] + e[i] 


# To fit this model we can simply add the additional variable with a + sign 
# in order to build a design matrix which fits based on the information 
# in additional variables:

diet <- factor(c(1,1,1,1,2,2,2,2))
sex <- factor(c("f","f","m","m","f","f","m","m"))
model.matrix(~ diet + sex)

# > model.matrix(~ diet + sex)
#   (Intercept) diet2 sexm
# 1           1     0    0
# 2           1     0    0
# 3           1     0    1
# 4           1     0    1
# 5           1     1    0
# 6           1     1    0
# 7           1     1    1
# 8           1     1    1
# attr(,"assign")
# [1] 0 1 2
# attr(,"contrasts")
# attr(,"contrasts")$diet
# [1] "contr.treatment"
# 
# attr(,"contrasts")$sex
# [1] "contr.treatment"

# The design matrix includes an intercept, a term for diet and a term for sex.
# This linear model accounts for differences in both the group and condition variables.
# Model assumes that the diet effect is the same for both males and females. 
# We say these are an additive effect.
# For each variable, we add an effect regardless of what the other is. 


# Another model is possible here, 
# which fits an additional term and which encodes the potential interaction 
# of group and condition variables. 

# The interaction model can be written in either of the following two formulas:

model.matrix(~ diet + sex + diet:sex)
# or
model.matrix(~ diet*sex)
  
# > model.matrix(~ diet*sex)
#   (Intercept) diet2 sexm diet2:sexm
# 1           1     0    0          0
# 2           1     0    0          0
# 3           1     0    1          0
# 4           1     0    1          0
# 5           1     1    0          0
# 6           1     1    0          0
# 7           1     1    1          1
# 8           1     1    1          1
# attr(,"assign")
# [1] 0 1 2 3
# attr(,"contrasts")
# attr(,"contrasts")$diet
# [1] "contr.treatment"
# 
# attr(,"contrasts")$sex
# [1] "contr.treatment"

# Where does model.matrix look for the data?
# The model.matrix function will grab the variable from the R global environment, 
# unless the data is explicitly provided as a data frame to the data argument:

group <- 1:4
model.matrix(~ group)
 
# Note how the R global environment variable group is ignored.
model.matrix(~ group, data=data.frame(group=5:8))

# > group <- 1:4
# > model.matrix(~ group)
#   (Intercept) group
# 1           1     1
# 2           1     2
# 3           1     3
# 4           1     4
# attr(,"assign")
# [1] 0 1

# > model.matrix(~ group, data=data.frame(group=5:8))
#   (Intercept) group
# 1           1     5
# 2           1     6
# 3           1     7
# 4           1     8
# attr(,"assign")
# [1] 0 1

# Continuous variables
# In the falling object example, time was a continuous variable in the model 
# and time squared was also included:

tt <- seq(0,3.4,len=25) 
model.matrix(~ tt + I(tt^2))

# > model.matrix(~ tt + I(tt^2))
#    (Intercept)        tt     I(tt^2)
# 1            1 0.0000000  0.00000000
# 2            1 0.1416667  0.02006944
# 3            1 0.2833333  0.08027778
# 4            1 0.4250000  0.18062500
# 5            1 0.5666667  0.32111111
# 6            1 0.7083333  0.50173611
# 7            1 0.8500000  0.72250000
# 8            1 0.9916667  0.98340278
# 9            1 1.1333333  1.28444444
# 10           1 1.2750000  1.62562500
# 11           1 1.4166667  2.00694444
# 12           1 1.5583333  2.42840278
# 13           1 1.7000000  2.89000000
# 14           1 1.8416667  3.39173611
# 15           1 1.9833333  3.93361111
# 16           1 2.1250000  4.51562500
# 17           1 2.2666667  5.13777778
# 18           1 2.4083333  5.80006944
# 19           1 2.5500000  6.50250000
# 20           1 2.6916667  7.24506944
# 21           1 2.8333333  8.02777778
# 22           1 2.9750000  8.85062500
# 23           1 3.1166667  9.71361111
# 24           1 3.2583333 10.61673611
# 25           1 3.4000000 11.56000000
# attr(,"assign")
# [1] 0 1 2



## Expressing Design Formula Exercises
# Suppose we have an experiment with the following design: 
# on three different days, perform an experiment with two treated and two control samples.
# We then measure some outcome y[i] and want to test the effect of condition, 
# while controlling for whatever differences might have occured due to the the different day
# (maybe the temperature in the lab affects the measuring device). 

# Assume that the true condition effect is the same for each day 
# (no interaction between condition and day). 
# We then define factors for 'day' and for 'condition'.

condition <- factor(c("treated","treated","treated","treated","treated","treated","control",
                      "control","control","control","control","control"))
day       <- factor(c("A","A","B","B","C","C","A","A","B","B","C","C"))
table(condition,day)

#         day:  A   B   C
# condition: --------------
# treated    |  2   2   2
# control    |  2   2   2

# Given the factors we have defined above, and not defining any new ones, 
# the following formula will produce a design matrix (model matrix) 
# that let's us analyze the effect of condition, controlling for the different days:

model.matrix(~ condition + day)

# > model.matrix(~ condition + day)
#    (Intercept) conditiontreated dayB dayC
# 1            1                1    0    0
# 2            1                1    0    0
# 3            1                1    1    0
# 4            1                1    1    0
# 5            1                1    0    1
# 6            1                1    0    1
# 7            1                0    0    0
# 8            1                0    0    0
# 9            1                0    1    0
# 10           1                0    1    0
# 11           1                0    0    1
# 12           1                0    0    1
# attr(,"assign")
# [1] 0 1 2 2
# attr(,"contrasts")
# attr(,"contrasts")$condition
# [1] "contr.treatment"
# 
# attr(,"contrasts")$day
# [1] "contr.treatment"

  
