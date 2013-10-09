data_check<-function()
{
  load('train.RData')
  col_names<-colnames(train)
  for (i in 1:ncol(train))
  {
    print(col_names[i])    
    print(summary(train[,i]))
  }
}