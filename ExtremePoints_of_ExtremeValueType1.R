# Extreme Value Type 1/ Gumbel distribution
# We can repeat the same exercise for any distribution by chaning evalcdf and evalpdf appropriately
scale = 1
evalpdf = function(x, scale=1){1/scale*exp(-x/scale)*exp(-exp(-x/scale))}
evalcdf = function(x, scale=1){exp(-exp(-x/scale))}

maxpdf = function(x, n, pdf, cdf){n*pdf(x)*(cdf(x)**(n-1))}
minpdf = function(x, n, pdf, cdf){n*pdf(x)*((1-cdf(x))**(n-1))}

x = seq(from=-8, to=8, by=0.1)
par(mfrow=c(2,1))
plot(x, maxpdf(x, 2, evalpdf, evalcdf), type = 'l', ylab='', xlab = 'x', 
     main = 'Pdf of max of random sample of Gumble distribution')
plot(x, minpdf(x, 2, evalpdf, evalcdf), type = 'l', ylab='', xlab = 'x', 
     main = 'Pdf of min of random sample of Gumble distribution')

