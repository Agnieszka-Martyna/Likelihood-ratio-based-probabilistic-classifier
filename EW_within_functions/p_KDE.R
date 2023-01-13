p.function = function(y.mean.1, y.mean.2, E, W, B, h, population, variables, p, n.1, n.2)
{
  if (!require(mvtnorm)) install.packages("mvtnorm")
  
  nc = ns = 1
  n0 = n.1*nc+n.2*ns
  
  means = aggregate(.~population$Factor,as.data.frame(population[,variables]),mean)[,-1] 
  if (p==1)  means = matrix(means,ncol=1)
  
  d.1 = mean(apply(means,1,function(x) dmvnorm(y.mean.1,mean=x,sigma=as.matrix(((n.1*nc)^-1)*(E+W)+h^2*B))))
  d.2 = mean(apply(means,1,function(x) dmvnorm(y.mean.2,mean=x,sigma=as.matrix(((n.2*ns)^-1)*(E+W)+h^2*B))))
  
  d.mean=mean(d.1,d.2)
  obs.diff= sqrt(sum((y.mean.1-y.mean.2)^2))
  d1 = d.1
  result = list(d.mean = d.mean,obs.diff=obs.diff,d1=d1)
  return (result)
}