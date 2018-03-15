# This code checks if a function of Multi-variate Normal has the same distribution as predicted by
# Multi-variate delta method. We do this plotting a qqplot of draws from the actual distribution from
# a large random sample, and the asymptotic distribution indicated by delta method

# MVN is 2 dimensional. g(X1,X2)=X1/X2. We are finding distribution of g applied on sample mean
# Sample size, Number of experiments
n = 1000
R = 1000

#MVN parameters

u = c(1,0.1)
sigma = matrix(c(1, 0.5, 0.5, 1), nrow = 2, ncol = 2)

# Delta method
del_u = matrix(c(1/u[2], -u[1]/u[2]**2), nrow = 2)
asym_sigma = sqrt(t(del_u)%*%sigma%*%del_u)
asym_mean = u[1]/u[2]
Y1 = rnorm(R, 0, asym_sigma)

# Finite sample distribution
# First draw multivarite normal random sample
#library(MASS)
#X = mvrnorm(n, u, sigma)
Y2 = rep(NA, R)
for ( i in 1:R){
  Z = matrix(rnorm(2*n, 0, 1), nrow = 2, ncol = n)
  X = u + t(chol(sigma))%*%Z
  Xbar = rowMeans(X)
  Y2[i] = sqrt(n)*(Xbar[1]/Xbar[2]-asym_mean)
}
qqplot(Y2, Y1)
abline(0,1)

# We would find that with the above values, the finite sample distribution has thicker tails. 
# This happens because the second component has values close to zero. We now increase u/sigma to ensure
# the second component is larger than 0

u = c(1,20)
sigma = matrix(c(1, 0.5, 0.5, 1), nrow = 2)

# Delta method
del_u = c(1/u[2], -u[1]/u[2]**2)
asym_sigma = sqrt(t(del_u)%*%sigma%*%del_u)
asym_mean = u[1]/u[2]
Y1 = rnorm(R, 0, asym_sigma)

# Finite sample distribution
# First draw multivarite normal random sample
Y2 = rep(NA, R)
for ( i in 1:R){
  Z = matrix(rnorm(2*n, 0, 1), nrow = 2, ncol = n)
  X = u + t(chol(sigma))%*%Z
  Xbar = rowMeans(X)
  Y2[i] = sqrt(n)*(Xbar[1]/Xbar[2]-asym_mean)
}
qqplot(Y2, Y1)
abline(0,1)