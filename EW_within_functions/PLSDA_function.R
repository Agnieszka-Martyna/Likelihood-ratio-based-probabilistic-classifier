PLSDA_function = function(train_set=train_set,y.mean.1=y.mean.1,variables_PLSDA=variables,y.1.2=y.1.2,complexity)
{
  library(pls)
class = rep(c(0,1),each=nrow(train_set)/2)
train_plsda = cbind(class=class,train_set[,variables_PLSDA])
means = apply(train_plsda,2,mean)
train_plsda.sc = apply(train_plsda,2,scale,scale=F,center=T)

class_test=unique(y.1.2$Factor)
if (class_test==cats[1]) class_test=0 else class_test=1

test_plsda = cbind(class=class_test,y.mean.1)
test_plsda.sc = cbind(test_plsda[,1],test_plsda[,-1] - means[-1])

plsda_results = plsr(class~.,data=as.data.frame(train_plsda.sc), scale = F, center=F,validation = "none")

###for TEST
predicted_test = predict(plsda_results,newdata=t(test_plsda.sc[,-1]),ncomp=complexity) 

predicted_test_01 = predicted_test[,,1]+means[1]

return(list(predicted_test_01=predicted_test_01))
}


# 
# 
# 
