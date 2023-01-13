
###change of notation from the three level paper of CGGA, GZ and JCurran: 
#between-class B instead of W
#within-class W instead of V
#within-samples (measurement error in paper E instead of U)

UC = function(population,variables)
{
  cats = unique(population$Factor)
  
  whichsmaller = which(cats == names(which.min(table(as.matrix(population$Factor)))))
  cat.list = cat.list.item = rep(list(1),length(cats))
  for (f in 1:length(cats))
  {
    cat.list[[f]] = which(population$Factor==cats[f])
    cat.list.item[[f]] = unique(population[which(population$Factor==cats[f]),"Item"])
  }
  
  no.samples = length(unique(population[cat.list[[whichsmaller]],"Item"]))
  
  
  
  pop.means = aggregate(. ~ population$Item, as.data.frame(population[,variables]), mean)
  # MODEL=NULL
  # for (s in 1:length(cats))
  # {
  #   KS_res = KS(as.matrix(pop.means[which(pop.means[,1] %in% cat.list.item[[s]]),-1]),k=length(cat.list.item[[s]])-0.75*no.samples)
  #   MODEL = c(MODEL,cat.list.item[[s]][KS_res$model])
  # }
  # train = population[which(!population$Item %in% MODEL),]
  
  #instead of the above:
  MODEL=NULL
  for (s in 1:length(cats))
  {
    KS_res = KS(as.matrix(pop.means[which(pop.means[,1] %in% cat.list.item[[s]]),-1]),k=0.75*no.samples)
    MODEL = c(MODEL,cat.list.item[[s]][KS_res$model])
  }
  train = population[which(population$Item %in% MODEL),]
  # print(table(train$Factor))
 
  

  s = length(unique(train$Factor))
  t = length(unique(train$Item))/s 
  q = length(unique(train$Piece))

  
  if (q==1) E=0 else
    {E = Reduce("+",lapply(split(train[,variables], train$Item), function(X) var(X)))
    E = E/(s*t)}
  
    means.item = aggregate(. ~ train$Item, as.data.frame(train[,variables]), mean)[,-1]
    factors = aggregate(. ~ train$Item, as.data.frame(train[,"Factor"]), unique)[,-1]
    
    W = Reduce("+",lapply(split(means.item, matrix(factors,ncol=1)), function(X) var(X)))
    W = W/s - E/q 
    
  means = aggregate(. ~ train$Factor, as.data.frame(train[,variables]), mean)
  B = var(means[,-1]) - W/t - E/(q*t) 
  
  return (list(B = B, W = W, E = E,train=train))
}