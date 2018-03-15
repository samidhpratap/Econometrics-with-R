# The following code implements random sampling from exponential distribution given that we have only
# a uniform distribution sampler at our disposal. We then test the effectiveness of this sampler vis-a-vis
# the exponential distributino sampler available in R

# We use probility integarl transform of Uniform to get draws from an exponential

set.seed(2)
lambda = 5
n = 10000

u = runif(n)                # Draw from uniform distribution
x = (-1/lambda) * log(1-u)  # Quantile function of exponential distribution

# qqplot

expDraws = rexp(n, lambda)  
qqplot(x, expDraws)
abline(0, 1)