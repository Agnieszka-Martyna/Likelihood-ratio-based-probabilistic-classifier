####LOGISTIC REGRESSSION
logit = function(LR.matrix_test1,LR.matrix_test2,cat.list_test1)
{
  # lambda=0.5
  # diag(LR.matrix_test2) = NA#;print(LR.matrix)
  # diag(LR.matrix_test1) = NA#;print(LR.matrix)
  LR.matrix_test2 =log10(LR.matrix_test2)
  # LR.matrix_test2 = sign(LR.matrix_test2)*log10(abs(LR.matrix_test2))
  
  q = matrix(unlist(lapply(cat.list_test1,function(x) range(x))),ncol=2,byrow=T)
  LR.H1.exp = NULL
  for (e in 1:nrow(q))
  {
    LR.H1.exp = LR.matrix_test1[q[e,1]:q[e,2],q[e,1]:q[e,2]]
    LR.H1.exp = apply(LR.H1.exp,1,max,na.rm=T)
  }
  LR.H1.exp = log10(LR.H1.exp);print(length(LR.H1.exp))
  # LR.H1.exp = sign(LR.H1.exp)*log10(abs(LR.H1.exp))
  # LR.H1.exp = LR.H1.exp[which(!is.na(LR.H1.exp))] 
  # weights.H1 = ifelse(LR.H1.exp>0,1,sqrt(10^LR.H1.exp))
    
  tmp = LR.matrix_test1
  for (e in 1:nrow(q))
  {
    tmp[q[e,1]:q[e,2],q[e,1]:q[e,2]] = NA
  }
  LR.H2.exp = apply(tmp,1,max,na.rm=T)
  # LR.H2.exp = tmp[which(!is.na(tmp))]
  LR.H2.exp = log10(LR.H2.exp)
  # LR.H2.exp = sign(LR.H2.exp)*log10(abs(LR.H2.exp))
  # LR.H2.exp = sort(LR.H2.exp,decreasing=F)[1:length(LR.H1.exp)];print(length(LR.H2.exp))
  # weights.H2 = ifelse(LR.H2.exp<0,1,1/sqrt(10^LR.H2.exp))
  # if (length(which(LR.H1.exp<min(LR.H2.exp)))>0) LR.H1.exp = LR.H1.exp[-which(LR.H1.exp<min(LR.H2.exp))] else LR.H1.exp=LR.H1.exp
  # if (length(which(LR.H2.exp>max(LR.H1.exp)))>0) LR.H2.exp = LR.H2.exp[-which(LR.H2.exp>max(LR.H1.exp))] else LR.H2.exp=LR.H2.exp
    #x = c(LR.H1.exp,LR.H2.exp,0)
  x = c(LR.H1.exp,LR.H2.exp)
  # weights = c(weights.H1,weights.H2)
  
  #plot(x,weights,ylim=c(0,2))
  # plot(density(LR.H1.exp));lines(density(LR.H2.exp),col="red")
  par(mar=c(5,5,4,2))
  plot(density(LR.H1.exp),type="l",col="red")
  lines(density(LR.H2.exp),type="l",col="green")
  #prob = c(rep(1,times=length(LR.H1.exp)),rep(0,times=length(LR.H2.exp)),0.5)
  prob = c(rep(1,times=length(LR.H1.exp)),rep(0,times=length(LR.H2.exp)))
  ##LINEAR FUNCTION OF x
  #glm.fit = glm(prob ~ x, data = as.data.frame(cbind(x=x,prob=prob)), family = binomial,weights=weights);print(glm.fit)
  glm.fit = glm(prob ~ I(x-0.5), data = as.data.frame(cbind(x=x,prob=prob)), family = binomial(link = "logit"));print(glm.fit)
  p=(1/(exp(-glm.fit$coefficients[1]-glm.fit$coefficients[2]*LR.matrix_test2)+1))
  p1=(1/(exp(-glm.fit$coefficients[1]-glm.fit$coefficients[2]*x)+1))
  plot((x),prob,pch=16,main="",cex=0.1,xlab=expression(paste(LR[t],sep="")),cex.lab=2,ylab="probability",cex.axis=1.7)
  order = order(x)
  lines((x[order]),p1[order],col="black",lwd=3)
  abline(h=0.5,v=0,col="gray",lwd=2,lty=3)
  points(0,0.5,pch=16,col="green")
  par(mar=c(5.1,4.1,4.1,2.1))
  #NORMAL SPLINES OF x
  #glm.fit = glm(prob ~ ns(x,df=4), data = as.data.frame(cbind(x=x,prob=prob)), family = binomial,weights=weights);print(glm.fit)
  #glm.fit = glm(prob ~ ns(x,df=4), data = as.data.frame(cbind(x=x,prob=prob)), family = binomial);print(glm.fit)
  #p=(1/(exp(-glm.fit$coefficients[1]-glm.fit$coefficients[2]*LR.matrix_test2)+1))
  #p1=(1/(exp(-glm.fit$coefficients[1]-glm.fit$coefficients[2]*x)+1))
  #plot((x),prob,pch=16,main="spline",xlim=c(-5,5),cex=0.1)
  #order = order(x)
  #lines((x[order]),p1[order],col="orange",lwd=2)
  #abline(h=0.5,col="red",lwd=2)
  #points(0,0.5,pch=16,col="green")
  
  return(list(p=p))
}
