###calibrating LRs according to Daniel
calibratingLR_function = function(LR.matrix_model,LR.matrix_test1,cat.list_model)
{
  require("isotone")
  q = matrix(unlist(lapply(cat.list_model,function(x) range(x))),ncol=2,byrow=T)
  LR.H1.exp = NULL
  for (e in 1:nrow(q))
  {
    LR.H1.exp = c(LR.H1.exp,LR.matrix_model[q[e,1]:q[e,2],q[e,1]:q[e,2]])
  }
  
  tmp = LR.matrix_model
  for (e in 1:nrow(q))
  {
    tmp[q[e,1]:q[e,2],q[e,1]:q[e,2]] = NA
  }
  
  LR.H2.exp = tmp[which(!is.na(tmp))]
  
  LRs = c(LR.H1.exp,LR.H2.exp)
  LRs[which(is.na(LRs))]=1
  N.H1 = length(LR.H1.exp)
  N.H2 = length(LR.H2.exp)
  indices = order(LRs)
  LRs.sorted=sort(LRs)
  posterior.prob = c(rep(1, times=N.H1), rep(0, times=N.H2))
  posterior.prob.sorted = posterior.prob[indices]
  
  ##identifying outliers
  #outliers = which(LRs.sorted>median(LRs.sorted)+5/1.4826*mad(LRs.sorted))
  #weights = rep(1,times=length(LRs.sorted))
  #weights[outliers] = 0.1;print(table(weights))
  ######
  
  #u=gpava(LRs.sorted,posterior.prob.sorted,weights= weights)
  u=gpava(LRs.sorted,posterior.prob.sorted,solver = weighted.mean)
  PAV = u$x
  LR.H1.H2.cal = PAV/(1-PAV)/(N.H1/N.H2)
  LR.H1.cal = LR.H1.H2.cal[which(indices %in% c(1:N.H1))]
  LR.H2.cal = LR.H1.H2.cal[which(indices %in% (N.H1+1):(N.H1+N.H2))]
  
  
   plot(u)
   plot(sort(u$z),u$x,type="s",col="red",xlim=c(0,median(LRs.sorted)+5/1.4826*mad(LRs.sorted)))
  # ECE_plot(LR.H1.cal,LR.H2.cal)
  # length(which(LR.same.KDE<1))/length(LR.same.KDE)
  # length(which(LR.different.KDE>1))/length(LR.different.KDE)
  
  # length(which(LR.H1.cal<1))/length(LR.H1.cal)
  # length(which(LR.H2.cal>1))/length(LR.H2.cal) 
  
  # plot(log10(lr.sorted),log10(LR.H1.H2.cal))
  calibrated = matrix(NA, nrow=nrow(LR.matrix_test1),ncol=ncol(LR.matrix_test1))
  for (r in 1:nrow(LR.matrix_test1))
  {
    for (col in 1:ncol(LR.matrix_test1))
    {
	if (is.na(LR.matrix_test1[r,col])) calibrated[r,col] = NA else
      {PAV_rcol = PAV[which.min(abs(LR.matrix_test1[r,col]-LRs.sorted))]
      calibrated[r,col] = PAV_rcol/(1-PAV_rcol)/(N.H1/N.H2)}
    }
  }
  calibrated[which(calibrated==Inf)]=max(calibrated[is.finite(calibrated)])
  calibrated[which(calibrated==-Inf)]=min(calibrated[is.finite(calibrated)])
  
  return(list(calibrated=calibrated))
}