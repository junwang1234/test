

#nohup java -jar RankLib.jar -train svm_light_train_data1 
#-ranker 6 -mls 100 -shrinkage 0.05 -tc 10 -leaf 10 -estop 300 
#-validate svm_light_test_data1 -metric2t NDCG@38 
#-save mymodel1.txt & 


# [ -tree <t> ]  Number of trees (default=1000)
# [ -leaf <l> ]	Number of leaves for each tree (default=10)
# [ -shrinkage <factor> ]	Shrinkage, or learning rate (default=0.1)
# [ -tc <k> ]	Number of threshold candidates for tree spliting. -1 to use all feature values (default=256)
# [ -mls <n> ]	Min leaf support -- minimum #samples each leaf has to contain (default=1)
# [ -estop <e> ]	Stop early when no improvement is observed on validaton data in e consecutive rounds (default=100)

main_lamdaMART<-function()
{
  tree_para<-c(3000)
  leaf_para<-c(200)
  shrinkage_para<-c(0.05)
  tc_para<-c(256)
  mls_para<-c(3e3)
  estop_para<-c(50)
  ranker<-6
  for (i_leaf in leaf_para)
  {
    for (i_shrinkage in shrinkage_para)
    {
      for(i_tc in tc_para)
      {
        for (i_mls in mls_para)
        {
          for (i_estop in estop_para)
          {
            for (i_tree in tree_para)
            {
              cmd_text<- paste(
                'nohup java -jar RankLib.jar -train train_data1', 
                '-ranker', ranker,'-mls', i_mls ,'-shrinkage',i_shrinkage,' -tc',i_tc,
                '-leaf',i_leaf, 
                '-estop', i_estop, '-tree',i_tree,
                '-validate validation_data1 -metric2t NDCG@38i -test validation_final_data -metric2T NDCG@38', 
                
                '-save ',
                sep=' '
              )
              
              model<-paste(ranker,i_mls,i_shrinkage,i_tc,i_leaf,i_estop,i_tree,
                           'model1',
                           sep='.')
              
              cmd_text<-paste(cmd_text,model,
                              
                              '   >','leaf',i_leaf,
                              'shrinkage',i_shrinkage,
                              'tc',i_tc,
                              'mls',i_mls,'tree',i_tree,'estop',i_estop,
                              '.out1 &',
                              sep=''
              )
              #write(cmd_text,file='job.tex',append=T)
              print(cmd_text)
             main_prediction(model,'validation_final_data')
            }
          }
        }
      }
    }
  }
}

main_prediction<-function(model,testdata)
{
  
  cmd_text<-paste('java -jar RankLib.jar -load',model,  '-test', testdata,' -metric2T NDCG@38',                  
                  sep=' ')
  print(cmd_text)
  
  cmd_text<-paste('java -jar RankLib.jar -load',model,  '-rank', 'test_data',' -metric2T NDCG@38',
                  '-score', paste(model,'test_data','prediction',sep='.'),
                  sep=' ')
  print(cmd_text)
}
