####LOGISTIC REGRESSSION
# logit = function(LR.matrix_test1,LR.matrix_test2,cat.list_test1)
# {
#   lambda=0.5
#   LR.matrix_test2 =(LR.matrix_test2^lambda-1)/lambda
#   q = matrix(unlist(lapply(cat.list_test1,function(x) range(x))),ncol=2,byrow=T)
#   LR.H1.exp = NULL
#   for (e in 1:nrow(q))
#   {
#     LR.H1.exp = c(LR.H1.exp,LR.matrix_test1[q[e,1]:q[e,2],q[e,1]:q[e,2]])
#   }
#   LR.H1.exp = (LR.H1.exp^lambda-1)/lambda
#   # outliers = which(LR.H1.exp>median(LR.H1.exp)+5/1.4826*mad(LR.H1.exp))
#   # LR.H1.exp = LR.H1.exp[-outliers]
#   weights.H1 = ifelse(LR.H1.exp>0,1,sqrt((lambda*(LR.H1.exp)+1)^(1/lambda)))
#   # weights.H1=weights.H1[-which(LR.H1.exp==output.matrix.Nor_test1[1045])]
#   # LR.H1.exp = LR.H1.exp[-which(LR.H1.exp==output.matrix.Nor_test1[1045])]
#   
#   tmp = LR.matrix_test1
#   for (e in 1:nrow(q))
#   {
#     tmp[q[e,1]:q[e,2],q[e,1]:q[e,2]] = NA
#   }
#   
#   LR.H2.exp = tmp[which(!is.na(tmp))]
#   LR.H2.exp = (LR.H2.exp^lambda-1)/lambda
#   # outliers = which(LR.H2.exp>median(LR.H2.exp)+5/1.4826*mad(LR.H2.exp))
#   weights.H2 = ifelse(LR.H2.exp<0,1,sqrt(1/((lambda*(LR.H2.exp)+1)^(1/lambda))))
#   
#   x = c(LR.H1.exp,LR.H2.exp,0)
#   weights = c(weights.H1,weights.H2,length(x))
#   
#   plot(x,weights,ylim=c(0,2))
#   # plot(density(LR.H1.exp));lines(density(LR.H2.exp),col="red")
#   
#   prob = c(rep(1,times=length(LR.H1.exp)),rep(0,times=length(LR.H2.exp)),0.5)
#   glm.fit = glm(prob ~ x, data = as.data.frame(cbind(x=x,prob=prob)), family = binomial,weights=weights);print(glm.fit)
#   # glm.fit = robust:::glmRob(prob ~ x, data = as.data.frame(cbind(x=x,prob=prob)), family = binomial);print(glm.fit)
#   # p=(1/(exp(-glm.fit$coefficients[1]-glm.fit$coefficients[2]*x)+1))
#   p=(1/(exp(-glm.fit$coefficients[1]-glm.fit$coefficients[2]*LR.matrix_test2)+1))
#   p1=(1/(exp(-glm.fit$coefficients[1]-glm.fit$coefficients[2]*x)+1))
#   plot((x),prob,pch=16)
#   order = order(x)
#   lines((x[order]),p1[order],col="orange",lwd=2)
#   abline(h=0.5,col="red",lwd=2)
#   
#   
#   return(list(p=p))
# }
logit = function(LR.matrix_test1,LR.matrix_test2,cat.list_test1)
{
  # lambda=0.5
  LR.matrix_test2 =log10(LR.matrix_test2)
  q = matrix(unlist(lapply(cat.list_test1,function(x) range(x))),ncol=2,byrow=T)
  LR.H1.exp = NULL
  for (e in 1:nrow(q))
  {
    LR.H1.exp = c(LR.H1.exp,LR.matrix_test1[q[e,1]:q[e,2],q[e,1]:q[e,2]])
  }
  LR.H1.exp = log10(LR.H1.exp)
  # outliers = which(LR.H1.exp>median(LR.H1.exp)+5/1.4826*mad(LR.H1.exp))
  # LR.H1.exp = LR.H1.exp[-outliers]
  weights.H1 = ifelse(LR.H1.exp>0,1,sqrt(10^LR.H1.exp))
  # weights.H1=weights.H1[-which(LR.H1.exp==output.matrix.Nor_test1[1045])]
  # LR.H1.exp = LR.H1.exp[-which(LR.H1.exp==output.matrix.Nor_test1[1045])]
  
  tmp = LR.matrix_test1
  for (e in 1:nrow(q))
  {
    tmp[q[e,1]:q[e,2],q[e,1]:q[e,2]] = NA
  }
  
  LR.H2.exp = tmp[which(!is.na(tmp))]
  LR.H2.exp = log10(LR.H2.exp)
  # outliers = which(LR.H2.exp>median(LR.H2.exp)+5/1.4826*mad(LR.H2.exp))
  weights.H2 = ifelse(LR.H2.exp<0,1,1/sqrt(10^LR.H2.exp))
  
  x = c(LR.H1.exp,LR.H2.exp,0)
  weights = c(weights.H1,weights.H2,length(x))
  
  plot(x,weights,ylim=c(0,2))
  # plot(density(LR.H1.exp));lines(density(LR.H2.exp),col="red")
  
  prob = c(rep(1,times=length(LR.H1.exp)),rep(0,times=length(LR.H2.exp)),0.5)
  glm.fit = glm(prob ~ x, data = as.data.frame(cbind(x=x,prob=prob)), family = binomial,weights=weights);print(glm.fit)
  # glm.fit = robust:::glmRob(prob ~ x, data = as.data.frame(cbind(x=x,prob=prob)), family = binomial);print(glm.fit)
  # p=(1/(exp(-glm.fit$coefficients[1]-glm.fit$coefficients[2]*x)+1))
  p=(1/(exp(-glm.fit$coefficients[1]-glm.fit$coefficients[2]*LR.matrix_test2)+1))
  p1=(1/(exp(-glm.fit$coefficients[1]-glm.fit$coefficients[2]*x)+1))
  plot((x),prob,pch=16)
  order = order(x)
  lines((x[order]),p1[order],col="orange",lwd=2)
  abline(h=0.5,col="red",lwd=2)
  
  
  return(list(p=p))
}
