#library
library('pracma')
library('randomForest')
library('e1071')
library('glmnet')
library('DMwR')
library('kernlab')
library('RSofia')
source('ndcgEvaluation.R')

main_test_sofia<-function()
{
  data(irismod)
  set.seed(1)
  qid<-sample(1:3,nrow(irismod),replace = T)
  names(qid)<-c('qid')
 

  
  labels<-irismod[,'Is.Virginica']
  index_names<-setdiff(colnames(irismod),'Is.Virginica')
  irismod_rest<-irismod[,index_names]
  iris_new<-cbind(labels,qid)
  iris_new<-cbind(iris_new,irismod_rest)
  rownames(iris_new)<-c(1:nrow(iris_new))
  
  
  i.TRAIN <- sample(1:nrow(iris_new), 100)
  
 for (ii in seq(0,1,0.1))
 {
  model <- sofia(labels ~ ., data=iris_new[i.TRAIN,], iterations=1e5,
                 random_seed=1,learner_type="pegasos",
                 loop_type="combined-ranking",rank_step_probability = ii)
 
  d <- predict(model, newdata=iris_new[-1*i.TRAIN,], prediction_type = "linear")
  print(ii)
  print(table(predicted=d>0, actual=iris_new[-1*i.TRAIN,]$labels))
 }
  
}