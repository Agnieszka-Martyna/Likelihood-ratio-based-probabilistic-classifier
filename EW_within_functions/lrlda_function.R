lrlda_function = function(train_set,y.mean.1,variables_lda)
{
  data=train_set[,c(1,variables_lda)]
  nclass = length(unique(data$Factor))
  cats = unique(data$Factor)
  
  mean.data=apply(data[,-1],2,mean)
  data[,-1] = apply(data[,-1],2,scale,center=T,scale=F)
  
  centered.data = apply(data[,-1],2,scale, center=T, scale=F)
  groupmean.data = aggregate(. ~ data$Factor, as.data.frame(data[,-1]), mean)
  centered.groupmean.data = apply(groupmean.data[,-1],2,scale, center=T, scale=F)
  
  B = t(centered.groupmean.data) %*% centered.groupmean.data/nclass
  
  W = Reduce("+",lapply(split(data[,-1], data$Factor), function(X) var(X)))/nclass
  
  eigenvectors = eigen(solve(W) %*% B)$vectors
  scales=NULL
  for (h in 1: min(nrow(groupmean.data)-1,ncol(data[,-1])))
  {
    scales = Re(c(scales,sqrt(t(eigenvectors[,h]) %*% W %*% eigenvectors[,h])))
  }
  
  
  scaled.eigenvectors = Re(eigenvectors[,1:min(nrow(groupmean.data)-1,ncol(data[,-1]))]/matrix(scales,nrow=nrow(eigenvectors),ncol=length(scales),byrow=T))
  scaled.eigenvectors
  
  library("MASS")
  lda = lda(Factor~.,data=data,prior=rep(1,times=length(unique(data$Factor)))/length(unique(data$Factor)))
  
  lda$scaling
  # t(lda$scaling[,1])%*%(lda$scaling[,1])
  scaled.eigenvectors
  
  
  #########prediction
  colnames(y.mean.1)=colnames(data)[-1]
  new.object = as.data.frame(y.mean.1)#as.data.frame(t(c(V2=10,V3=0)))
  # points(new.object,pch=16,col="green",cex=c(1,2,3))
  new.object.transformed = new.object - matrix(mean.data,ncol=length(variables_lda),nrow=nrow(new.object),byrow=T)
  predicted = predict(lda,new.object.transformed) ####new  obejct must be manually transofrmed as training set!!!!!!!
  predicted$x
  (as.matrix(new.object.transformed)) %*% lda$scaling ###thw same as above from predict function
  (as.matrix(new.object.transformed)) %*%scaled.eigenvectors ###similar to the result from predict function with a sign relevance
  
  library(mvtnorm)
  predicted.training = predict(lda,data[,-1])$x
  predicted.mean1 = predict(lda,as.data.frame(t(apply(data[which(data$Factor==cats[1]),-1],2,mean))))$x
  predicted.mean2 = predict(lda,as.data.frame(t(apply(data[which(data$Factor==cats[2]),-1],2,mean))))$x
  
  predicted$posterior
  prior=c(1/nclass,1/nclass)
  s1 = var(predicted.training[which(data$Factor==cats[1]),])
  n1 = length(predicted.training[which(data$Factor==cats[1]),])
  s2 = var(predicted.training[which(data$Factor==cats[2]),])
  n2 = length(predicted.training[which(data$Factor==cats[2]),])
  pooled.cov = ((n1-1)*s1+(n2-1)*s2)/(n1+n2-2)
  post1=prior[1]*dmvnorm(predicted$x,mean=predicted.mean1,sigma=as.matrix(pooled.cov))
  post2=prior[2]*dmvnorm(predicted$x,mean=predicted.mean2,sigma=as.matrix(pooled.cov))
  probs1 = post1/(post1+post2)
  probs2 = post2/(post1+post2)
  
  lrlda = probs1/(1-probs1)
  
  
  return(list(lrlda=lrlda,probabilities=cbind(probs1,probs2)))
  
}

