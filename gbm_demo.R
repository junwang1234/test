library('gbm')
load('XData.RData')
   set.seed(1) 
  names_target <- colnames(XData)[1]
  model<-gbm(as.formula(paste(names_target,"~ .")),
             data=XData,
             distribution=list(name="pairwise",group="qid",metric="ndcg",max.rank=38),
             n.trees=3000,
             shrinkage=0.05,
             interaction.depth=49,
             bag.fraction=1,
             train.fraction=0.9,
             n.minobsinnode=nrow(XData)*0.9*0.0015,
             verbose=T)
   best.iter <- gbm.perf(model,method="test")
   print(best.iter)
   save(model,file='gbm.49.9M.0.05.RData')
   print(model$valid.error[best.iter])
