# This code eludcidates Maximum Likelihood estimation of a multinomail logit model. 
# Multinomial logit model is a random utility model ie total utility = deterministic portion + random part
# For mathematicl convenicence random part is extreme value 1 distribution
# We assume deterministic portion is constant(j) + beta * log(price(j)) for the jth choice

### simulate "data"

set.seed(205)
N=100000  # Number of choices made
J=3       # Number of alternatives


# simulate log price

logP=matrix(runif(N*J,min=-1,max=1),nrow=N,ncol=J)


# true parameter values

talpha=c(0,-1,1)
tbeta=-5


# construct choice probabilities for each alternative

V=rep(1,N)%*%t(talpha)+tbeta*logP  # deterministic portion of utility
expV=exp(V)
choiceProb=expV/rowSums(expV)


# random draws of choice ie. we draw a one hot vector, each position is set with it's choiceProb

Y=matrix(0,nrow=N,ncol=J)   # choice matrix initialized as all zeros
draw=runif(N)               # draw a number in [0,1]
Y[draw<choiceProb[,1],1]=1
Y[draw>choiceProb[,1]+choiceProb[,2],3]=1
Y[,2]=1-Y[,1]-Y[,3]

### given the data (logP and Y), we now estimate model parameters (alpha, beta)

negll_logit=function(para){
  
  # alpha1 is normalized to 0.
  # So we parameterize as alpha2-alpha1 and alpha3-alpha1
  
  alpha=c(0,para[1:2])
  beta=para[3]
  
  V=rep(1,N)%*%t(alpha)+beta*logP
  expV=exp(V)
  choiceProb=expV/rowSums(expV)
  ll=sum(log(rowSums(Y*choiceProb)))
  
  return (-ll)
}

init=rep(0,3)
out=optim(init, negll_logit)
print(out)
