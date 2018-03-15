# We use bootstrap to find the standard error of an estimate of theta = exp(mean) from a random sample drawn from
# a normal distribution with unit variance. 
set.seed(205)
mean = 5
variance = 1
n = 100
R = 1000

# Bootstrapping
rand_sample = rnorm(n, mean, sqrt(variance))
theta_est = rep(NA,R)
for (i in 1:R){
  this_sample = sample(rand_sample, n, replace = T)
  theta_est[i] = exp(mean(this_sample))
}

# Standard error
std_error = sqrt(sum((theta_est-mean(theta_est))^2)/(R-1))
print('Standard Error estimated by bootstrap is')
print(sd(theta_est))

# 95% pivot confidence interval
pivoted = theta_est - mean(theta_est)
quantiles = quantile(pivoted, c(0.025, 0.975))
CI.pivot = 2*mean(theta_est) - c(quantiles[2],quantiles[1])
print('Pivot Confidence Interval is')
print(CI.pivot)

# We now check if bootstrap captures the true distribution by plotting the bootstrap distribution 
# against the true distribution

# true distribution
theta_true = rep(NA,R)
for (i in 1:R){
  this_sample = rnorm(n, mean, sqrt(variance))
  theta_true[i] = exp(mean(this_sample))
}

hist(theta_true, breaks = 100, col = rgb(1,0,0,0.5), 
     main = 'Comparison of true (red) and bootstrap(blue) distribution')
par(new = T)
hist(theta_est, breaks = 100, col = rgb (0, 0, 1, 0.2), main = '')