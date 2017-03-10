# Suppose we are analyzing a set of 4 samples. 
# The first two samples are from a treatment group A 
# and the second two samples are from a treatment group B. 
# This design can be represented with a model matrix like so:
X <- matrix(c(1,1,1,1,0,0,1,1),nrow=4)
rownames(X) <- c("a","a","b","b")

 
# X will then look like:
# 
# a 1 0
# a 1 0
# b 1 1
# b 1 1

# Suppose that the fitted parameters for a linear model give us:
beta <- c(5, 2)

# Use the matrix multiplication operator, %*%, in R to answer questions:
# 1. What is the fitted value for the A samples? (The fitted Y values.)
y = X %*% beta
5
#   [,1]
# a    5
# a    5
# b    7
# b    7

# 2. What is the fitted value for the B samples? (The fitted Y values.)
7

# Suppose now we are comparing two treatments B and C to 
# a control group A, each with two samples.
# This design can be represented with a model matrix like so:
X <- matrix(c(1,1,1,1,1,1,0,0,1,1,0,0,0,0,0,0,1,1),nrow=6)
rownames(X) <- c("a","a","b","b","c","c")

# Suppose that the fitted values for the linear model are given by:
beta <- c(10,3,-3)

fit = X %*% beta
