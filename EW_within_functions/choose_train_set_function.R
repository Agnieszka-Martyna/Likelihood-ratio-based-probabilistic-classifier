choose_train_set = function(population,variables)
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
  MODEL=NULL
  for (s in 1:length(cats))
  {
    KS_res = KS(as.matrix(pop.means[which(pop.means[,1] %in% cat.list.item[[s]]),-1]),k=length(cat.list.item[[s]])-0.75*no.samples)
    MODEL = c(MODEL,cat.list.item[[s]][KS_res$model])
  }
  
  train = population[which(!population$Item %in% MODEL),]

  return (list(train=train))
}

