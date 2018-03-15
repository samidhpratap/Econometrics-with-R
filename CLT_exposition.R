# Central Limit Theorem in Action
# we test if the distribution of the mean of a random sample drawn from Bernoulli distribution converges to normal
set.seed(205)
sample.draws = 10000
n = c(100, 200, 300, 400, 500, 600, 700, 800, 900, 1000)
x = seq(from=-4, to=4, by=0.01)

p=0.5

ecdf_mean = function(n, x, sample.draws){
               drawn.mat = rbinom(sample.draws, n, p)
               mean.vector = (drawn.mat/n-p)*sqrt(n)/(p*(1-p))
               #print(mean.vector[1:6])
               mean.vector = sort(mean.vector)
               #print(mean.vector[1:6])
               j = 1
               # Note the following steps are unnecessary if we just want qqplot.
               # We could just qqplot(mean.vector,rnorm(sample.draws))
               # The following code allows us the flexibility multiple qqplot on
               # on the same graph. 
               eF = rep(NA, length(x))
               for (i in 1:length(x)){
                   while (j<=length(mean.vector) && mean.vector[j]<x[i])
                       { j = j + 1}
                   eF[i]=(j - 1)/sample.draws
               }
               return(eF)
}
par(mfrow=c(3, 1))
#par(mar=c(1, 1))
for (i in c(1, 5, 10)) {
  y = ecdf_mean(n[i], x, sample.draws)
  z = qnorm(y)
  plot (x, z, type = 'l')
}



