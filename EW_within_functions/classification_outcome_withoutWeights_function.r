classification.outcomes = function(LR.matrix,p.matrix,cats,cat.list,variant)
{
  par(mfrow=c(1,1),mar=c(5,15,4,2))
  image.matrix = matrix(NA,nrow=nrow(LR.matrix),ncol=ncol(p.matrix))
  image.matrix[LR.matrix>1] = 100 
  image.matrix[LR.matrix<1] = 0
  image(1:nrow(image.matrix),1:ncol(image.matrix),image.matrix,xlab="control samples",ylab="recovered samples",col = hcl.colors(12),cex.lab=2,cex.axis=2)
  abline(v=as.numeric(lapply(cat.list,max))+0.5,h=as.numeric(lapply(cat.list,max))+0.5,col="darkgrey",lwd=4)
  dev.copy(png,paste(variable.name,"_classification_image",".png",sep=""));dev.off()
  dev.copy(postscript,paste(variable.name,"_classification_image",".eps",sep=""),width=500,height=500);dev.off()
  par(mar=c( 5.1 ,4.1 ,4.1 ,2.1))
  
  #fuzzy.classification = t(apply(p.matrix,1,function(x) {FC=NULL;for (f in 1:length(cat.list))
  #{
  #  FC = c(FC,c(length(which(x[cat.list[[f]]]>1))/length(cat.list[[f]])))
  #}
  #FC }))
  #colnames(fuzzy.classification) = cats
  
  # fuzzy.classification = t(apply(p.matrix,1,function(x) {ord = order(x);c(x[ord])[-c(1:length(x)/2)];num = c(c(1:ncol(p.matrix))[ord])[-c(1:length(x)/2)];
  # FC=NULL;for (f in 1:length(cat.list))
  # {
  #   y = length(which(num %in% cat.list[[f]]))/floor((length(x)/2))
  #   
  #   # mod = sort(y)#10^(mean(log10(y)))#mean(y)#mean(c(sort(y)[-c((length(y)-4):length(y))],sort(y)[c((length(y)-4):length(y))]))#exp(mean(log10(y)))#mean(y)
  #   FC = c(FC,y)
  # }
  # FC }))
  # colnames(fuzzy.classification) = cats
  
#     fuzzy.classification = t(apply(cbind(p.matrix,LR.matrix),1,function(x) {FC=NULL;for (f in 1:length(cat.list))
#     {
#   	y = x[cat.list[[f]]]
#   	lr = x[cat.list[[f]]+ncol(p.matrix)]
#   	z = y-median(y) #(x[ncol(p.matrix)+cat.list[[f]]])
# # plot(z)
#   	z = ifelse(z<0,0,(lr))
#   mod = sum(y*(z))/sum((z)) 
#   FC = c(FC,mod)
#     }
#     FC }))
#     colnames(fuzzy.classification) = cats


  fuzzy.classification = t(apply(cbind(p.matrix,LR.matrix),1,function(x) {FC=NULL;for (f in 1:length(cat.list))
  {
    y = x[cat.list[[f]]]
    # lr = x[cat.list[[f]]+ncol(p.matrix)]
    # z = y-median(y) #(x[ncol(p.matrix)+cat.list[[f]]])
    # plot(z)
    # z = ifelse(z<0,0,z)
    mod = max(y,na.rm=T)#;print(mod)#sum(y*(z))/sum((z)) 
    FC = c(FC,mod)
  }
  FC }))
  colnames(fuzzy.classification) = cats

#   fuzzy.classification0 = t(apply(p.matrix,1,function(x) {FC0=NULL;for (f in 1:length(cat.list))
#   {
#     #FC = c(FC,c(mean(x[cat.list[[f]]],na.rm=T)))
# 	y = x[cat.list[[f]]]
#     mod0 = c(median(y,na.rm=T))
#     
#     if (mod0>1) mod0=1 else mod0=mod0
# 	if (mod0<0) mod0=0 else mod0=mod0
#     FC0 = c(FC0,mod0)
#   }
#   FC0 }))
#   colnames(fuzzy.classification0) = cats
#   
  probabilities = t(apply(fuzzy.classification,1,prop.table)*100)
  
  fuzzy.classification = cbind(fuzzy.classification,probabilities)
  
  
  
  pred_cat_fuzzy = apply(fuzzy.classification[,1:length(cats)],1,function(x) colnames(fuzzy.classification)[which.max(x)])
  true_predicted_fuzzy = cbind(fuzzy.classification,predicted = pred_cat_fuzzy,true =as.character(true))
  #pred_cat_hard = apply(hard.classification[,1:length(cats)],1,function(x) colnames(hard.classification)[which.max(x)])
  #true_predicted_hard = cbind(hard.classification,predicted = pred_cat_hard,true =as.character(true))
  
  confusion.table.fuzzy = table(true_predicted_fuzzy[,c("true")],true_predicted_fuzzy[,c("predicted")]);print(confusion.table.fuzzy)
  #confusion.table.hard = table(true_predicted_hard[,c("true")],true_predicted_hard[,c("predicted")])
  
  accuracy.fuzzy = (confusion.table.fuzzy/rowSums(confusion.table.fuzzy))                     
  #accuracy.hard = (confusion.table.hard/rowSums(confusion.table.hard))                     
  
  
#   probabilities0 = t(apply(fuzzy.classification0,1,prop.table)*100)
#   fuzzy.classification0 = cbind(fuzzy.classification0,probabilities0)
#   pred_cat_fuzzy0 = apply(fuzzy.classification0[,1:length(cats)],1,function(x) colnames(fuzzy.classification0)[which.max(x)])
#   true_predicted_fuzzy0 = cbind(fuzzy.classification0,predicted = pred_cat_fuzzy0,true =as.character(true))
# confusion.table.fuzzy0 = table(true_predicted_fuzzy0[,c("true")],true_predicted_fuzzy0[,c("predicted")])
#   accuracy.fuzzy0 = (confusion.table.fuzzy0/rowSums(confusion.table.fuzzy0))                     

  # ###classical ECE from all LR values
  # q = matrix(unlist(lapply(cat.list,function(x) range(x))),ncol=2,byrow=T)
  # LR.same = NULL
  # for (e in 1:nrow(q))
  # {
  #   LR.same = c(LR.same,LR.matrix[q[e,1]:q[e,2],q[e,1]:q[e,2]])
  # }
  # fn = length(which(LR.same<1))
  # fn.all = sum(apply(q,1,function(x) (x[2]-x[1]+1)^2))
  # 
  # tmp = LR.matrix
  # for (e in 1:nrow(q))
  # {
  #   tmp[q[e,1]:q[e,2],q[e,1]:q[e,2]] = NA
  # }
  # 
  # LR.different = tmp[which(!is.na(tmp))]
  # fp = length(which(LR.different>1))
  # fp.all = m.all.analysed^2-fn.all
  # 
  # error_rates_array = matrix(0, nrow = 2, ncol = 1,dimnames=list(c("fp", "fn"),c()))
  # error_rates_array[1,1] = fp/fp.all*100
  # error_rates_array[2,1] = fn/fn.all*100
  # write.table(signif(error_rates_array, digits = 3), file = paste(variable.name,"comparison_research_error_rate_",variant,".txt", sep=""), quote = FALSE, sep = "\t", col.names = TRUE, row.names = TRUE, dec = ".")
  
  
  # write.table(fuzzy.classification, file = paste(variable.name,"fuzzy_classification_",variant,".txt", sep=""), quote = FALSE, sep = "\t", col.names = TRUE, row.names = TRUE, dec = ".")
  # #write.table(hard.classification, file = paste(variable.name,"hard_classification_",variant,".txt", sep=""), quote = FALSE, sep = "\t", col.names = TRUE, row.names = TRUE, dec = ".")
  # write.table(round(accuracy.fuzzy*100,0), file = paste(variable.name,"accuracy_fuzzy_",variant,".txt", sep=""), quote = FALSE, sep = "\t", col.names = TRUE, row.names = TRUE, dec = ".")
  #write.table(round(accuracy.hard*100,0), file = paste(variable.name,"accuracy_hard_",variant,".txt", sep=""), quote = FALSE, sep = "\t", col.names = TRUE, row.names = TRUE, dec = ".")
  # write.table(signif(p.matrix, digits = 4), file = paste(variable.name,"comparison_research_",variant,".txt", sep=""), quote = FALSE, sep = "\t", col.names = TRUE, row.names = TRUE, dec = ".")
  
  
  
  # ECE_plot(LR.same[which(!is.na(LR.same))],LR.different[which(!is.na(LR.different))])
  # dev.copy(postscript, paste(variable.name,"_ECE_",variant,".eps", sep=""));dev.off()
  # 
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
  
  return(list(fuzzy.classification=fuzzy.classification,accuracy.fuzzy=accuracy.fuzzy))
}
