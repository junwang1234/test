#ndcg computation
#library
library(lubridate)

ndcg <- function(prediction, target) {
  optimal <-dcg(sort(target,decreasing=T))
  pre<-dcg(prediction)
  score<-pre/optimal
  
  if (optimal<=0){
    if (pre==optimal)
    {
      score<-1
    }
    else
    {
      score<-0
    }
  }
  score
}
# dcg computation
dcg<-function(rel_ranked){
  sum((2**rel_ranked-1)/log2((1:length(rel_ranked))+1))
}

# indexing training id
indexTrain<-function(id,start){
  value<-id[start]
  end<-start
  numUser<-length(id)
  while (id[end]==value )
  {
    end<-end+1
    if (end>=numUser)
     { end<-end+1
      break
    }
  }
  start:(end-1)
}

wrapper_ndcg<-function(filename,srchID,propID,relevance)
{
  
  predicted_rank<-read.csv(filename,header=T,sep=",");
  searchid<-predicted_rank$SearchId
  Propertyid<-predicted_rank$PropertyId

  userid<-unique(searchid)
 
  score<-0
  index1<-0
  start<-index1[length(index1)]+1
  numUser<-length(searchid)
  # ndcg of each query 
  while(start<=numUser)
  {
    
    index1<-indexTrain(searchid,start)
    
    target<-relevance[index1]
    
    prediction<-Propertyid[index1]
    propIDbenchmark<-propID[index1]
    pred_score<-vector()
    for (i in 1:length(prediction))
    {
      pred_score[i]<-target[prediction[i]==propIDbenchmark]
    }
    
    score<-score+ndcg(pred_score,target)
    
    start<-index1[length(index1)]+1
   
  }
  # final score
  print(score/length(userid))
}


# ndcg_wrapper_test function
ndcg_wrapper_test<-function(){
  load('subTrain.RData')
  attach(subTrain)
  
  
  userid<-unique(srch_id)
  score<-0
  index1<-0
  start<-index1[length(index1)]+1
  numUser<-length(srch_id)
  proprank<-vector()
  while(start<=numUser)
  {
    
    index1<-indexTrain(srch_id,start)  
    proprank<-c(proprank,prop_id[start-1+sort(position[index1],index.return=T)$ix])
    start<-index1[length(index1)]+1
    
  }
  test<-cbind(srch_id,proprank)
  colnames(test)<-c('SearchId','PropertyId')
  write.csv(test,file='result_test.csv')
  wrapper_ndcg('result_test.csv',srch_id,prop_id,booking_bool*4+click_bool)
  
}

ndcg_wrapper2<-function(id, prediction,yTe)
{
  queryid<-unique(id)
  score<-0
  for (i in queryid)
  {
    indexQuery<-which(id==i)
    target<-yTe[indexQuery]
  
    pred_score<-target[sort(prediction[indexQuery],decreasing =T, index.return=T)$ix]
   
    score<-score+ndcg(pred_score,target)
  }
  score/length(queryid)
}

ndcg_wrapper3<-function(id, prediction,yTe)
{
  queryid<-unique(id)
  score<-0  
  indexQuery<-0
  start<-indexQuery[length(indexQuery)]+1
  numUser<-length(id)

  while(start<=numUser)
  {
    
    indexQuery<-indexTrain(id,start)
    
    target<-yTe[indexQuery]
    
    pred_score<-target[sort(prediction[indexQuery],decreasing =T, index.return=T)$ix]
    
    score<-score+ndcg(pred_score,target)
    
    start<-indexQuery[length(indexQuery)]+1
  }
  score/length(queryid)
}

write_prediction<-function(srch_id,prop_id,prediction)
{
  userid<-unique(srch_id)
  
  prop_final<-vector(mode="list",length=length(srch_id))
 # pred_final<-vector(mode="list",length=length(srch_id))
  
  index_prop<-0
  start<-index_prop[length(index_prop)]+1
  numUser<-length(srch_id)
  
  while(start<=numUser)
  {
    
    index_prop<-indexTrain(srch_id,start)
    prop_query<-prop_id[index_prop]
    prop_pred<-prediction[index_prop] 
    prop_query_sorted<-prop_query[sort(prop_pred,index.return=T,decreasing=T)$ix]
    #pred_query_sorted<-sort(prop_pred,decreasing=T)
    for(idx in 1:length(index_prop)) 
    {
      prop_final[[index_prop[idx]]] <- prop_query_sorted[idx]
      #pred_final[[index_prop[idx]]] <- pred_query_sorted[idx]
    }
    #prop_final<-c(prop_final,prop_query[sort(prop_pred,index.return=T,decreasing=T)$ix])
    start<-index_prop[length(index_prop)]+1
    
  }
  
  output<-cbind(srch_id,prop_final)
  colnames(output)<-c('SearchId','PropertyId')
  print(class(output))
  write.csv(output,file='result_new.csv',row.names = F)
}

local_scale<-function(XData,srch_id)
{
  index_prop<-0
  start<-index_prop[length(index_prop)]+1
  numUser<-length(srch_id)
  
  XNew<-matrix(nrow=nrow(XData),ncol=ncol(XData))
  colnames(XNew)<-colnames(XData)

  while(start<=numUser)
  {
    
    index_prop<-indexTrain(srch_id,start)
    XNew[index_prop,]<-apply(XData[index_prop,],2,robustStandardization)
    start<-index_prop[length(index_prop)]+1
  }
  XNew
}

local_scale2<-function(XData,srch_id)
{
  index_prop<-0
  start<-index_prop[length(index_prop)]+1
  numUser<-length(srch_id)
  
  numID<-length(unique(srch_id))
  XNew<-vector('list',length=numID)
  
  i <- 1
  while(start<=numUser)
  {
    
    index_prop<-indexTrain(srch_id,start)
    XNew[[i]]<-as.vector(t(apply(XData[index_prop,],2,robustStandardization)))
    start<-index_prop[length(index_prop)]+1
    i=i+1
  }
  XNew<-matrix(unlist(XNew),byrow=T,ncol=ncol(XData))
  colnames(XNew)<-colnames(XData)
  XNew
}

robustStandardization<-function(x)
{
  #u<-quantile(x,probs=0.95)
  #l<-quantile(x,probs=0.05)
  #x[x>u]<-u
  #x[x<l]<-l
 #std<-(IQR(x)/1.349)+1e-11
 #m<-median(x)
  if (length(x)>0)
  {
    std<-sd(x)+1e-11
    m<-mean(x)
    z<-(x-m)/std
    #z<-(x-l)/((u-l)+1e-11)

  }
  else
  {
  #  z<-0
    z<-0
  }
  
  z
}

date_fea_extr<-function(x)
{
  x_datetime<-strptime(as.character(x),format="%Y-%m-%d %H:%M:%S")
  m <- as.numeric(format(as.Date(x_datetime), "%m"))
  x_date<-weekdays(as.Date(x_datetime))
  x_hour<-hour(x_datetime)
  z <- vector('list',length=length(x))
  names(z)<-names(x)
  for (i in 1:length(x))
  {
    if (x_date[i]=='Sunday' |x_date[i]=='Saturday')
    {
      z[i]<-1
    }
    else
    {
      if (x_hour[i]>7&x_hour[i]<19)
      {
        z[i]<-0
      }
      else
      {
        z[i]<-1
      }
    }
  }
 z <- as.numeric(z)
 list(month=m,date_time=z)
}

polyFea<-function(fea,order)
{
  if (order==1)
  {
    fea
  }
  else
  {
    temp<-fea
    for (i in 2:order)
    {
      temp <- as.vector(temp%o%temp)
    }
    temp
  }
  
}

local_central<-function(XData,srch_id)
{
  index_prop<-0
  start<-index_prop[length(index_prop)]+1
  numUser<-length(srch_id)
  
  XNew<-matrix(nrow=nrow(XData),ncol=ncol(XData))
  colnames(XNew)<-colnames(XData)
  
  while(start<=numUser)
  {
    
    index_prop<-indexTrain(srch_id,start)
    if (dim(XData)[2]==1)
    {
      XNew[index_prop,]<-robustCentral(XData[index_prop,])
    }
    else
    {
    XNew[index_prop,]<-apply(XData[index_prop,],2,robustCentral)
    }
    start<-index_prop[length(index_prop)]+1
    
    
  }
  XNew
}

robustCentral<-function(x)
{
  if (length(x)>0)
  {
    m<-mean(x)
    z<-x-m
  }
  else
  {
    z<-0
  }
  z
}

 mean_comp<-function(x) {
    if(all(is.na(x))==T)
    {
	x<-rep(0,length(x))
    }
    else
    {
	x[is.na(x)]<-mean(x,na.rm=T)
    }
    x
}

