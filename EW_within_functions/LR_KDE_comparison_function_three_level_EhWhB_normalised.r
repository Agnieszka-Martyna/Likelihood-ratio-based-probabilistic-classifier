LR.KDE.function = function(y.mean.1, y.mean.2, y.star,E, W, B, hb,hw, population, variables, p, n.1, n.2)
{
	if (!require(mvtnorm)) install.packages("mvtnorm")

  nc = ns = 1
  n0 = n.1*nc+n.2*ns
  
	density.1.1 = dmvnorm(y.mean.1,mean=y.mean.2,sigma=as.matrix(n0*(E+hw^2*W)/(n.1*nc*n.2*ns)))/dmvnorm(y.mean.2,mean=y.mean.2,sigma=as.matrix(n0*(E+hw^2*W)/(n.1*nc*n.2*ns)))
	
	means = aggregate(.~population$Item,as.data.frame(population[,variables]),mean)[,-1] 
	if (p==1)  means = matrix(means,ncol=1) 

	density.1.2 = mean(apply(means,1,function(x) dmvnorm(y.star,mean=x,sigma=as.matrix((E+hw^2*W)/n0+hb^2*B))/dmvnorm(x,mean=x,sigma=as.matrix((E+hw^2*W)/n0+hb^2*B))))  

	density.2.1 = mean(apply(means,1,function(x) dmvnorm(y.mean.1,mean=x,sigma=as.matrix(((n.1*nc)^-1)*(E+hw^2*W)+hb^2*B))/dmvnorm(x,mean=x,sigma=as.matrix(((n.1*nc)^-1)*(E+hw^2*W)+hb^2*B))))
	density.2.2 = mean(apply(means,1,function(x) dmvnorm(y.mean.2,mean=x,sigma=as.matrix(((n.2*ns)^-1)*(E+hw^2*W)+hb^2*B))/dmvnorm(x,mean=x,sigma=as.matrix(((n.2*ns)^-1)*(E+hw^2*W)+hb^2*B))))
#print(c(density.1.1,density.1.2,density.2.1,density.2.2))
	if (density.1.1 == 0 | density.1.1<10^-300) density.1.1 = 10^-300
	if (density.1.2 == 0 | density.1.2<10^-300) density.1.2 = 10^-300
	if (density.2.1 == 0 | density.2.1<10^-300) density.2.1 = 10^-300
	if (density.2.2 == 0 | density.2.2<10^-300) density.2.2 = 10^-300
	
	LR.KDE = density.1.1*density.1.2/density.2.1/density.2.2

	result = list(LR.KDE = LR.KDE)
	return (result)
}






















