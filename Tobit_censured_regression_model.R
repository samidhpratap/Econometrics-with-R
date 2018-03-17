# This program finds the likelihood of a Tobit regression. In a tobit regression, values of dependent variable
# below zero are censored and are shown as zero. 

ll_censured=function(X,y,beta,sigma){
  mu=X%*%beta
  e=y-mu
  d=-mu/sigma  # when y=0, e=-mu
  y0=as.numeric(y==0)
  ll=y0*log(pnorm(d)) + (1-y0)*(-log(2*pi*sigma**2)/2-e**2/sigma**2/2)
  return(sum(ll))
}

# simulate data

set.seed(205)
n=1000
tsigma=.2; tbeta=1
X=matrix(runif(n,min=-1,max=1),ncol=1)
mu=X*tbeta
ystar=mu+rnorm(n,sd=tsigma)
y=ystar
y[y<0]=0
plot(X,y,main="simulated observations")

# log-likelihood contours 
ngrid=400
beta=seq(from=-2,to=3.5,length=ngrid)
sigma=seq(from=.10,to=3,length=ngrid)

ll=matrix(nrow=ngrid,ncol=ngrid)
for (i in 1:ngrid){
  for (j in 1:ngrid){
    ll[i,j]=ll_censured(X,y,beta[i],sigma[j])
  }
}

levels=seq(from=quantile(ll,probs=.25),to=max(ll),len=20)
image(beta,sigma,ll,col=topo.colors(19),breaks=levels)
contour(beta,sigma,ll,levels=round(levels,digits=1),add=TRUE,
        lwd=2,drawlabels=FALSE,xlab="")
contour(beta,sigma,ll,levels=round(levels[10:20],digits=1),add=TRUE,
        lwd=2,labcex=1.2)
points(1,.2,pch=20,cex=3,col="blue")
title(main="Censored Regression Likelihood",line=3)
title(main= paste("beta = ",tbeta,", sigma = ",tsigma,", N = ",length(y),sep=""),line=1,cex=.5)