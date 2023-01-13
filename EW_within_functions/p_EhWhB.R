p.function = function(y.mean.1, y.mean.2, E, W, B, hb,hw, population, variables, p, n.1, n.2)
{
  if (!require(mvtnorm)) install.packages("mvtnorm")
  
  nc = ns = 1
  n0 = n.1*nc+n.2*ns
  
  means = aggregate(.~population$Item,as.data.frame(population[,variables]),mean)[,-1] 
  if (p==1)  means = matrix(means,ncol=1)
  
  #d.1 = mean(apply(means,1,function(x) dmvnorm(y.mean.1,mean=x,sigma=as.matrix(((n.1*nc)^-1)*(E+hw^2*W)+hb^2*B))))
  #d.2 = mean(apply(means,1,function(x) dmvnorm(y.mean.2,mean=x,sigma=as.matrix(((n.2*ns)^-1)*(E+hw^2*W)+hb^2*B))))
  #d.mean=mean(c(d.1,d.2))
  
  # pnorm.1 = mean(apply(means,1,function(x) pmvnorm(lower=-Inf,upper=as.numeric(-abs(y.mean.1-x)),mean=rep(0,p),sigma=as.matrix(((n.1*nc)^-1)*(E+hw^2*W)+hb^2*B))))
  # pnorm.2 = mean(apply(means,1,function(x) pmvnorm(lower=-Inf,upper=as.numeric(-abs(y.mean.2-x)),mean=rep(0,p),sigma=as.matrix(((n.2*ns)^-1)*(E+hw^2*W)+hb^2*B))))
  # pnorm.mean = mean(c(pnorm.1,pnorm.2))
  
  mah.1 = mean(apply(means,1,function(x) mahalanobis((y.mean.1),center=rep(x,p),cov=as.matrix(((n.1*nc)^-1)*(E+hw^2*W)+hb^2*B))))
  mah.2 = mean(apply(means,1,function(x) mahalanobis((y.mean.2),center=rep(x,p),cov=as.matrix(((n.2*nc)^-1)*(E+hw^2*W)+hb^2*B))))
  mah.mean = mean(c(mah.1,mah.2))
  
  obs.diff= mahalanobis((y.mean.2),center=y.mean.1,cov=as.matrix(n0*(E+hw^2*W)/(n.1*nc*n.2*ns)))
    #sqrt(sum((y.mean.1-y.mean.2)^2))
  #d1 = d.1
  #d2 = d.2
  result = list(d.mean = mah.mean,obs.diff=obs.diff)
  return (result)
}