balance_train_set = function(population,variables)
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
  cat.samples.list = lapply(cat.list.item,function(x) sample(x,no.samples))
  
  train = population[which(population$Item %in% unlist(cat.samples.list)),]
  
  return (list(train=train))
}