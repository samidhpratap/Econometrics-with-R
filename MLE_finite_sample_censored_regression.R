# This code implements MLE of censored regression by minimizing negative log-likelihood
# We also capture finite sample distribution of beta and sigma estimates

negll_censored=function(para){
  
  beta=para[1]
  sigma=exp(para[2]) # sigma is restricted to be positive.
  
  mu=X%*%beta        
  e=y-mu
  d=-mu/sigma
  y0=as.numeric(y==0)
  ll=y0*log(pnorm(d)) + (1-y0)*(-log(2*pi*sigma**2)/2-e**2/sigma**2/2)
  
  return(-sum(ll))
}

set.seed(205)
#
# common objects shared by every simulation run
#
n=1000
tsigma=.2; tbeta=1
X=matrix(runif(n,min=-1,max=1),ncol=1)
mu=X*tbeta

#
# 10000 simulation
# `convvec` is to check whether each optimization converged or not.
#
R=10000
convvec=rep(NA,R)
betavec=rep(NA,R)
sigmavec=rep(NA,R)
for (r in 1:R){
  #
  # simulate data
  #
  set.seed(r)
  ystar=mu+rnorm(n,sd=tsigma)
  y=ystar
  y[y<0]=0
  
  init=c(0,0)
  out=optim(init, negll_censored)
  
  convvec[r]=out$conv
  betavec[r]=out$par[1]
  sigmavec[r]=exp(out$par[2])
}

par(mfrow = c(2,1))
hist(betavec, breaks=100)
hist(sigmavec, breaks=100)