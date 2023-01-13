classification.outcomes = function(LR.matrix,p.matrix,cats,cat.list,variant)
{
  image.matrix = matrix(0,nrow=nrow(LR.matrix),ncol=ncol(p.matrix))
  image.matrix[LR.matrix>1] = 100 
  image(1:nrow(image.matrix),1:ncol(image.matrix),image.matrix,asp=1,xlab="control samples",ylab="recovered samples")
  abline(v=lapply(cat.list,max),h=lapply(cat.list,max),col="darkgreen",lwd=2)
  dev.copy(png,paste(variable.name,"classification_image_",variant,"_.png",sep=""));dev.off()
  dev.copy(postscript,paste(variable.name,"classification_image_",variant,"_.eps",sep=""));dev.off()
  
  
  #fuzzy.classification = t(apply(p.matrix,1,function(x) {FC=NULL;for (f in 1:length(cat.list))
  #{
  #  FC = c(FC,c(length(which(x[cat.list[[f]]]>1))/length(cat.list[[f]])))
  #}
  #FC }))
  #colnames(fuzzy.classification) = cats
  
  fuzzy.classification = t(apply(cbind(p.matrix,LR.matrix),1,function(x) {FC=NULL;for (f in 1:length(cat.list))
  {
	###y = x[cat.list[[f]]]
###ylr1 = x[ncol(LR.matrix)+cat.list[[f]]]
###ylr2 = ylr1
###ylr2[which(ylr1<1)] =  NA
#### ylr2 = ylr1[which(ylr1>1)]
####t1 = 10^(2.5*quantile(log10(ylr2),0.75,na.rm = T)-1.5*quantile(log10(ylr2),0.25,na.rm = T))
###t1 = (2.5*quantile((ylr2),0.75,na.rm = T)-1.5*quantile((ylr2),0.25,na.rm = T))
	####t2 = 10^(2.5*quantile(log10(ylr2),0.25)-1.5*quantile(log10(ylr2),0.75))
	###if (length(c(y[which(ylr2>t1)]))!=0) {amendment = mean(c(y[which(ylr2>t1)]))
	###mod = c(median(y,na.rm=T))+amendment-0.5} else
    ###mod = c(median(y,na.rm=T))+mean(c(y[which(y<quantile(y,0.05))],y[which(y>quantile(y,0.95))]))-0.5
    
    ###if (mod>1) mod=1 else mod=mod
	###if (mod<0) mod=0 else mod=mod
    ###FC = c(FC,mod)
	y = x[cat.list[[f]]]
ylr1 = x[ncol(LR.matrix)+cat.list[[f]]]
ylr2 = ylr1
ylr2[which(ylr1<1)] =   1/ylr1[which(ylr1<1)]

weights = log10(ylr2)
mod = sum(y*weights)/sum(weights)
FC = c(FC,mod)
  }
  FC }))
  colnames(fuzzy.classification) = cats
  
  fuzzy.classification0 = t(apply(p.matrix,1,function(x) {FC0=NULL;for (f in 1:length(cat.list))
  {
    #FC = c(FC,c(mean(x[cat.list[[f]]],na.rm=T)))
	y = x[cat.list[[f]]]
    mod0 = c(median(y,na.rm=T))
    
    if (mod0>1) mod0=1 else mod0=mod0
	if (mod0<0) mod0=0 else mod0=mod0
    FC0 = c(FC0,mod0)
  }
  FC0 }))
  colnames(fuzzy.classification0) = cats
  
  #hard.classification = t(apply(p.matrix,1,function(x) {HC=NULL;for (f in 1:length(cat.list))
  # {
  #     HC = c(HC,c(prod(((x[cat.list[[f]]])))))
  #     HC[which(HC==Inf)]=10^300
  #}
  #HC }))
  # colnames(hard.classification) = cats
  
  
  probabilities = t(apply(fuzzy.classification,1,prop.table)*100)
  
  fuzzy.classification = cbind(fuzzy.classification,probabilities)
  
  
  
  pred_cat_fuzzy = apply(fuzzy.classification[,1:length(cats)],1,function(x) colnames(fuzzy.classification)[which.max(x)])
  true_predicted_fuzzy = cbind(fuzzy.classification,predicted = pred_cat_fuzzy,true =as.character(true))
  #pred_cat_hard = apply(hard.classification[,1:length(cats)],1,function(x) colnames(hard.classification)[which.max(x)])
  #true_predicted_hard = cbind(hard.classification,predicted = pred_cat_hard,true =as.character(true))
  
  confusion.table.fuzzy = table(true_predicted_fuzzy[,c("true")],true_predicted_fuzzy[,c("predicted")])
  #confusion.table.hard = table(true_predicted_hard[,c("true")],true_predicted_hard[,c("predicted")])
  
  accuracy.fuzzy = (confusion.table.fuzzy/rowSums(confusion.table.fuzzy))                     
  #accuracy.hard = (confusion.table.hard/rowSums(confusion.table.hard))                     
  
  
  probabilities0 = t(apply(fuzzy.classification0,1,prop.table)*100)
  fuzzy.classification0 = cbind(fuzzy.classification0,probabilities0)
  pred_cat_fuzzy0 = apply(fuzzy.classification0[,1:length(cats)],1,function(x) colnames(fuzzy.classification0)[which.max(x)])
  true_predicted_fuzzy0 = cbind(fuzzy.classification0,predicted = pred_cat_fuzzy0,true =as.character(true))
confusion.table.fuzzy0 = table(true_predicted_fuzzy0[,c("true")],true_predicted_fuzzy0[,c("predicted")])
  accuracy.fuzzy0 = (confusion.table.fuzzy0/rowSums(confusion.table.fuzzy0))                     

  ###classical ECE from all LR values
  q = matrix(unlist(lapply(cat.list,function(x) range(x))),ncol=2,byrow=T)
  LR.same = NULL
  for (e in 1:nrow(q))
  {
    LR.same = c(LR.same,LR.matrix[q[e,1]:q[e,2],q[e,1]:q[e,2]])
  }
  fn = length(which(LR.same<1))
  fn.all = sum(apply(q,1,function(x) (x[2]-x[1]+1)^2))
  
  tmp = LR.matrix
  for (e in 1:nrow(q))
  {
    tmp[q[e,1]:q[e,2],q[e,1]:q[e,2]] = NA
  }
  
  LR.different = tmp[which(!is.na(tmp))]
  fp = length(which(LR.different>1))
  fp.all = m.all.analysed^2-fn.all
  
  error_rates_array = matrix(0, nrow = 2, ncol = 1,dimnames=list(c("fp", "fn"),c()))
  error_rates_array[1,1] = fp/fp.all*100
  error_rates_array[2,1] = fn/fn.all*100
  write.table(signif(error_rates_array, digits = 3), file = paste(variable.name,"comparison_research_error_rate_",variant,".txt", sep=""), quote = FALSE, sep = "\t", col.names = TRUE, row.names = TRUE, dec = ".")
  
  
  write.table(fuzzy.classification, file = paste(variable.name,"fuzzy_classification_",variant,".txt", sep=""), quote = FALSE, sep = "\t", col.names = TRUE, row.names = TRUE, dec = ".")
  #write.table(hard.classification, file = paste(variable.name,"hard_classification_",variant,".txt", sep=""), quote = FALSE, sep = "\t", col.names = TRUE, row.names = TRUE, dec = ".")
  write.table(round(accuracy.fuzzy*100,0), file = paste(variable.name,"accuracy_fuzzy_",variant,".txt", sep=""), quote = FALSE, sep = "\t", col.names = TRUE, row.names = TRUE, dec = ".")
  #write.table(round(accuracy.hard*100,0), file = paste(variable.name,"accuracy_hard_",variant,".txt", sep=""), quote = FALSE, sep = "\t", col.names = TRUE, row.names = TRUE, dec = ".")
  write.table(signif(p.matrix, digits = 4), file = paste(variable.name,"comparison_research_",variant,".txt", sep=""), quote = FALSE, sep = "\t", col.names = TRUE, row.names = TRUE, dec = ".")
  
  
  source("../../EW_within_functions/ECE_function.R")
  ECE_plot(LR.same[which(!is.na(LR.same))],LR.different[which(!is.na(LR.different))])
  dev.copy(postscript, paste(variable.name,"_ECE_",variant,".eps", sep=""));dev.off()
  
  # 
  # ECE_H1=ECE_H2=NULL
  # for (k in 1:nrow(p.matrix))
  # {
  #   fc = fuzzy.classification[k,1:length(cats)]
  #   whichmax = which.max(fc)
  #   if (cats[whichmax] == true[k]) ##if true and predicted categories match
  #   {
  #     sizeH1 = ceiling(max(fc[-whichmax])*length(cat.list[[whichmax]]))
  #     LR_H1 = p.matrix[k,cat.list[[whichmax]]]
  #     LR_H1above1 = LR_H1[which(LR_H1>1)] 
  #     eceH1_k = sample(LR_H1above1,sizeH1)
  #     
  #     
  #     remainingCats = (1:length(fc))[-whichmax]
  #     eceH2_k = NULL
  #     for (g in 1:length(remainingCats))
  #     {
  #       sizeH2 = ceiling((1-fc[whichmax])*length(cat.list[[remainingCats[g]]]))
  #       LR_H2 = p.matrix[k,cat.list[[remainingCats[g]]]]
  #       LR_H2below1 = LR_H2[which(LR_H2<1)]
  #       eceH2_kg = sample(LR_H2below1,sizeH2)
  #       eceH2_k = c(eceH2_k,eceH2_kg)
  #       
  #     }
  #   }
  #   
  #   if (cats[whichmax] != true[k]) ##if true and predicted categories DO NOT match
  #   {
  #     truecat = which(cats==true[k])
  #     sizeH1 = ceiling(max(fc[-truecat])*length(cat.list[[truecat]]))
  #     if (sizeH1>(length(cat.list[[truecat]])-1)) sizeH1 = length(cat.list[[truecat]])-1
  #     LR_H1 = p.matrix[k,cat.list[[truecat]]]
  #     LR_H1above1 = LR_H1[which(LR_H1>1)] 
  #     LR_H1below1 = LR_H1[which(LR_H1<1)] 
  #     eceH1_k = c(LR_H1above1,sample(LR_H1below1,sizeH1-length(LR_H1above1)))
  #     
  #     
  #     remainingCats = (1:length(fc))[-truecat]
  #     eceH2_k = NULL
  #     for (g in 1:length(remainingCats))
  #     {
  #       sizeH2 = ceiling((1-fc[truecat])*length(cat.list[[remainingCats[g]]]))
  #       if(sizeH2>length(cat.list[[remainingCats[g]]])) sizeH2 = length(cat.list[[remainingCats[g]]]) 
  #       LR_H2 = p.matrix[k,cat.list[[remainingCats[g]]]]
  #       LR_H2below1 = LR_H2[which(LR_H2<1)]
  #       LR_H2above1 = LR_H2[which(LR_H2>1)]
  #       
  #       if (length(LR_H2below1)>sizeH2) eceH2_kg = sample(LR_H2below1,sizeH2) else
  #         eceH2_kg = c(LR_H2below1,sample(LR_H2above1,sizeH2-length(LR_H2below1)))
  #       eceH2_k = c(eceH2_k,eceH2_kg)
  #       
  #     }
  #   }
  #   
  #   ECE_H1 = c(ECE_H1,eceH1_k)
  #   ECE_H2 = c(ECE_H2,eceH2_k)
  # }
  # source("../../EW_within_functions/ECE_function.R")
  # ECE_plot(ECE_H1,ECE_H2)
  # dev.copy(png, paste(variable.name,"_ECE_fuzzyIdea_",variant,".png", sep=""));dev.off()
  # 
  # 
  
  return(list(fuzzy.classification=fuzzy.classification,accuracy.fuzzy=accuracy.fuzzy,error_rates_array,fuzzy.classification0=fuzzy.classification0,accuracy.fuzzy0=accuracy.fuzzy0))
}
