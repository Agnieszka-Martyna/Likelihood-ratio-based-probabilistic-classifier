
###change of notation from the thrre level paper of CGGA, GZ and JCurran: 
#between-class B instead of W
#within-class W instead of V
#within-samples (measirement error in paper E instead of U

UC = function(population,variables,population_lda)
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
  
  cat.samples.list = lapply(cat.list.item,function(x) sample(x,no.samples))
  # print(c(length(cat.samples.list[[1]]),length(cat.samples.list[[2]])))
  
  train = population[which(population$Item %in% unlist(cat.samples.list)),]
  train_lda = population_lda[which(population_lda$Item %in% unlist(cat.samples.list)),]
  

  s = length(unique(train$Factor))
  t = length(unique(train$Item))/s
  q = length(unique(train$Piece))
  

  if (q==1) E=0 else
    {E = Reduce("+",lapply(split(train[,variables], train$Item), function(X) var(X)))
    E = E/(s*t)}
  
   
  
    W = Reduce("+",lapply(split(train[,variables], train$Factor), function(X) var(X)))
    W = W/s - E/q 
    
  means = aggregate(. ~ train$Factor, as.data.frame(train[,variables]), mean)
  B = var(means[,-1]) - W/t - E/(q*t) 
  
  
  
  
  
    s = length(unique(train$Factor))
  t = length(unique(train$Item))
  q = length(unique(train$Piece))
  
  if (q==1) E_lda=0 else
    {E_lda = Reduce("+",lapply(split(train_lda[,variables_lda], train_lda$Item), function(X) var(X)))
    E_lda = E_lda/(s*t)}
  
   
  
    W_lda = Reduce("+",lapply(split(train_lda[,variables_lda], train_lda$Factor), function(X) var(X)))
    W_lda = W_lda/s - E_lda/q 
    
  means_lda = aggregate(. ~ train_lda$Factor, as.data.frame(train_lda[,variables_lda]), mean)
  B_lda = var(means_lda[,-1]) - W_lda/t - E_lda/(q*t) 
  
  
  return (list(B = B, W = W, E = E,train=train,train_lda=train_lda,B_lda=B_lda,W_lda=W_lda,E_lda=E_lda))
}