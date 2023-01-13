LR.Nor.function = function(y.mean.1, y.mean.2, y.star, E, W, B, population, variables, p, n.1, n.2)
{
	if (p>1) mean.all = colMeans(population[,variables]) else 
    mean.all = mean(population[,variables])
  
	nc = ns = 1
	n0 = n.1*nc+n.2*ns
	
	density.1.1 = dmvnorm(y.mean.1,mean=y.mean.2,sigma=as.matrix(n0*(E+W)/(n.1*nc*n.2*ns)))
	density.1.2 = dmvnorm(y.star,mean=mean.all,sigma=as.matrix((E+W)/n0+B))
	
	density.2.1 = dmvnorm(y.mean.1,mean=mean.all,sigma=as.matrix(((n.1*nc)^-1)*(E+W)+B))
	density.2.2 = dmvnorm(y.mean.2,mean=mean.all,sigma=as.matrix(((n.2*ns)^-1)*(E+W)+B))
	 
	LR.Nor = density.1.1*density.1.2/density.2.1/density.2.2

	result = list(LR.Nor = LR.Nor)
	return (result)
}

