
rm(list=ls())
#library
source('ndcgEvaluation.R')

load('test.RData')
prediction<-read.table('ensemble.2500.300.0.02.800.prediction')

print('writing final prediction')
write_prediction(test[,'srch_id'],test[,'prop_id'],data.matrix(prediction))
