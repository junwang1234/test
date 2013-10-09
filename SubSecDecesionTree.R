rm(list=ls())
#library

library(randomForest)
source('ndcgEvaluation.R')



# main function
main_DT<-function(fold=1,train_portion=0.5,testcode=0,traincode=0){
  load('train.RData')
  subTrain<-train[1:5e5,]
  rm(train)
  NA_tm<-vector()
  for (i in 1:ncol(subTrain))
  {
    if (length(which(is.na(subTrain[,i])))>0.5*nrow(subTrain))
    {
      NA_tm<-c(NA_tm,colnames(subTrain)[i])
    }
  }
  #   NA_tm<-grep('comp',colnames(subTrain))
  #   NA_tm<-c(colnames(subTrain)[NA_tm],'visitor_hist_adr_usd','visitor_hist_starrating')
  
  NA_tm<-setdiff(NA_tm,c('visitor_hist_starrating','visitor_hist_adr_usd','srch_query_affinity_score',
                         'comp1_rate','comp2_rate','comp3_rate','comp4_rate',
                         'comp5_rate','comp6_rate','comp7_rate','comp8_rate',
                         'comp1_rate_percent_diff','comp2_rate_percent_diff','comp3_rate_percent_diff',
                         'comp4_rate_percent_diff','comp5_rate_percent_diff','comp6_rate_percent_diff',
                         'comp7_rate_percent_diff','comp8_rate_percent_diff',
                         'comp1_inv', 'comp2_inv','comp3_inv' ,'comp4_inv' ,'comp5_inv' ,
                         'comp6_inv' ,'comp7_inv' ,'comp8_inv' 
  ))
  print(dim(subTrain))
  print(NA_tm)
  
  subTrain<-subTrain[,setdiff(colnames(subTrain),NA_tm)]
  subTrain2<-subTrain
  subTrain2[,'date_time']<-date_fea_extr(subTrain2[,'date_time'])
  attach(subTrain2)
  #************history records********************
  # historical_price
  
  
  subTrain2$price_usd<-log( subTrain2$price_usd)
  
  subTrain2$prop_log_historical_price[ subTrain2$prop_log_historical_price==0]<- subTrain2$price_usd[ subTrain2$prop_log_historical_price==0]
  subTrain2$prop_log_historical_price<- subTrain2$prop_log_historical_price
  
  # visitor_hist_starrating
  
  subTrain2$visitor_hist_starrating[is.na( subTrain2$visitor_hist_starrating)]<- subTrain2$prop_starrating[is.na( subTrain2$visitor_hist_starrating)]
  subTrain2$visitor_hist_starrating<- subTrain2$visitor_hist_starrating
  # visitor_hist_adr_usd
  subTrain2$visitor_hist_adr_usd<-log( subTrain2$visitor_hist_adr_usd)
  subTrain2$visitor_hist_adr_usd[is.na( subTrain2$visitor_hist_adr_usd)]<- subTrain2$price_usd[is.na( subTrain2$visitor_hist_adr_usd)]
  subTrain2$visitor_hist_adr_usd<- subTrain2$visitor_hist_adr_usd
  
  #competitor
  subTrain2$comp1_rate<- subTrain2$comp1_rate_percent_diff* subTrain2$comp1_rate
  subTrain2$comp2_rate<- subTrain2$comp2_rate_percent_diff* subTrain2$comp2_rate
  subTrain2$comp3_rate<- subTrain2$comp3_rate_percent_diff* subTrain2$comp3_rate
  subTrain2$comp4_rate<- subTrain2$comp4_rate_percent_diff* subTrain2$comp4_rate
  subTrain2$comp5_rate<- subTrain2$comp5_rate_percent_diff* subTrain2$comp5_rate
  subTrain2$comp6_rate<- subTrain2$comp6_rate_percent_diff* subTrain2$comp6_rate
  subTrain2$comp7_rate<- subTrain2$comp7_rate_percent_diff* subTrain2$comp7_rate
  subTrain2$comp8_rate<- subTrain2$comp8_rate_percent_diff* subTrain2$comp8_rate
  #srch_query_affinity_score
  
  subTrain2[is.na(subTrain2)]<-0
  subTrain2$comp1_rate<- subTrain2$comp1_rate/100
  subTrain2$comp1_rate[abs( subTrain2$comp1_rate)>1]<-0
  subTrain2$comp2_rate<- subTrain2$comp2_rate/100
  subTrain2$comp2_rate[abs( subTrain2$comp2_rate)>1]<-0
  subTrain2$comp3_rate<- subTrain2$comp3_rate/100
  subTrain2$comp3_rate[abs( subTrain2$comp3_rate)>1]<-0
  subTrain2$comp4_rate<- subTrain2$comp4_rate/100
  subTrain2$comp4_rate[abs( subTrain2$comp4_rate)>1]<-0
  subTrain2$comp5_rate<- subTrain2$comp5_rate/100
  subTrain2$comp5_rate[abs(subTrain2$comp5_rate)>1]<-0
  subTrain2$comp6_rate<-subTrain2$ comp6_rate/100
  subTrain2$comp6_rate[abs(subTrain2$comp6_rate)>1]<-0
  subTrain2$comp7_rate<-subTrain2$comp7_rate/100
  subTrain2$comp7_rate[abs(subTrain2$comp7_rate)>1]<-0
  subTrain2$comp8_rate<-subTrain2$comp8_rate/100
  subTrain2$comp8_rate[abs(subTrain2$comp8_rate)>1]<-0
  
  
  
  
  
  
  remove_feature<-c('site_id', 'random_bool'                  
                    'position','srch_id','prop_id',
                    'booking_bool','gross_bookings_usd','click_bool',
                    'comp1_rate_percent_diff','comp2_rate_percent_diff','comp3_rate_percent_diff',
                    'comp4_rate_percent_diff','comp5_rate_percent_diff','comp6_rate_percent_diff',
                    'comp7_rate_percent_diff','comp8_rate_percent_diff'
  )
  feature_names<-setdiff(colnames(subTrain2),remove_feature)
  YData<-booking_bool*4+click_bool
  target<-booking_bool*4+click_bool
  #  YData<-factor(YData)
  XData<-subTrain2[,feature_names]
  
  index_distance<-grep('orig_destination_distance',colnames(XData))
  XData[,index_distance]<-abs(local_central(data.matrix(XData[,index_distance]),srch_id))
  
  names_discrete<-c('srch_booking_window','srch_saturday_night_bool',
                    'srch_adults_count','srch_children_count','date_time',
                    'srch_length_of_stay','srch_room_count',
                    'visitor_location_country_id','prop_country_id','srch_destination_id'
  )
  #names_discrete<-c('prop_starrating','prop_review_score')
  #,'prop_starrating','prop_review_score'
  
  names_remained<-c('prop_brand_bool','promotion_flag','srch_query_affinity_score',
                    'comp1_rate','comp2_rate','comp3_rate','comp4_rate',
                    'comp5_rate','comp6_rate','comp7_rate','comp8_rate',
                    
                    'comp1_inv', 'comp2_inv','comp3_inv' ,'comp4_inv' ,'comp5_inv' ,
                    'comp6_inv' ,'comp7_inv' ,'comp8_inv' 
  )
  names_numeric<-setdiff(setdiff(feature_names,names_discrete),names_remained)
  print(names_numeric)
  
  names_positive<-c("prop_location_score1","prop_location_score2","prop_starrating" ,
                    "prop_review_score","prop_log_historical_price",
                    'visitor_hist_starrating','visitor_hist_adr_usd',"price_usd"     )
  names_negative<-c(     
    "orig_destination_distance"              
  )
  
  
  
  #************preprocessing discrete feature*************
  quan<-list()
  
  names_discrete1<-c('srch_saturday_night_bool', 'srch_children_count','date_time',
                     
                     
  )
  names_discrete2<-c('srch_booking_window','srch_adults_count',
                     'srch_length_of_stay','srch_room_count')
  names_discrete3<- c('visitor_location_country_id','prop_country_id','srch_destination_id')
  for (str in names_discrete1)
  {
    if (length(unique(XData[,str]))==2)
    {
      quan[[str]]<-sort(unique(XData[,str]))
    }
    else
    {
      quan[[str]]<- unique(quantile(XData[,str],probs =0.5))
    }
  }
  
  for (str in names_discrete2)
  {
    if (length(unique(XData[,str]))==2)
    {
      quan[[str]]<-sort(unique(XData[,str]))
    }
    else
    {
      quan[[str]]<- unique(quantile(XData[,str],probs =c(0.25,0.5,0.75)))
    }
  }
  
  for (str in names_discrete3)
  {
    if (length(unique(XData[,str]))==2)
    {
      quan[[str]]<-sort(unique(XData[,str]))
    }
    else
    {
      quan[[str]]<- unique(quantile(XData[,str],probs =c(0.2,0.4,0.6,0.8)))
    }
  }
  
  for (str in names_discrete)
  {
    cur_quan<-unlist(quan[str])
    num_dis_quan<-length(cur_quan)
    
    temp<-XData[,str]
    index1<-which(XData[,str]<=cur_quan[1])
    temp[index1]<-0
    for (i in c(1:num_dis_quan))
    {
      if (i==num_dis_quan)
      {
        index1<-which(XData[,str]>cur_quan[i])
      }
      else
      {
        index1<-which(XData[,str]>cur_quan[i]&XData[,str]<=cur_quan[i+1])
      }
      temp[index1]<-i
    }
    XData[,str]<-temp
  }
  
  
  
  
  level_dis<-list()
  for (i in 1:length(names_discrete))
  {
    idx=grep(names_discrete[i],colnames(XData))
    cc<-factor(XData[,idx])
    print(names_discrete[i])
    print(levels(cc))
    level_dis[[i]]<-unique(as.numeric(levels(cc)[cc]))
  }
  
  #XData<-scale(XData)
  #*******discrete features***********
  XData_Dis<-XData[,names_discrete]
  
  XData_whole<-cbind(XData[,names_remained])
  XData_whole[,c('prop_brand_bool','promotion_flag')]<-XData_whole[,c('prop_brand_bool','promotion_flag')]-0.5
  XData[,names_negative]<- XData[,names_negative]*-1
  XData<-cbind(XData_whole,local_scale(XData[,names_numeric],srch_id))
  
  
  
  
  for (i in 1:ncol(XData_Dis))
  {
    
    temp_data<-matrix(0,nrow=nrow(XData_Dis),ncol=length(level_dis[[i]]))
    current_level<-unlist(level_dis[[i]])
    colnames(temp_data)<-paste(names_discrete[i],current_level,sep='')
    
    for (k in 1:length(current_level))
    {
      temp_data[XData_Dis[,i]==current_level[k],k] <- 1
    }
    XData<-cbind(XData,temp_data)
  }
  
  print(dim(XData))
  #****************Query feature************
  
  names_query <- c('srch_length_of_stay','srch_booking_window',
                   'srch_adults_count','srch_children_count','date_time',
                   'srch_room_count','srch_saturday_night_bool',
                   'visitor_location_country_id','prop_country_id','srch_destination_id'
  )
  names_query_index<-vector()
  for(str in names_query)
  {
    names_query_index<-c(names_query_index,grep(str,colnames(XData)))
  }
  XData<-data.matrix(XData)
  names_prop_index<-setdiff(c(1:ncol(XData)),names_query_index)
  
  names_2order<-c("prop_location_score1","prop_location_score2","prop_starrating" ,
                  "price_usd",'prop_brand_bool','visitor_hist_starrating',
                  'prop_log_historical_price'
  )
  names_2order_index<-vector()
  for(str in names_2order)
  {
    names_2order_index<-c(names_2order_index,grep(str,colnames(XData)))
  }
  
  XData_sim<-matrix(nrow=nrow(XData),ncol=( length(names_2order_index)*length(names_2order_index)+length(names_prop_index))*length(names_query_index))
  for (i in 1:nrow(XData))
  {
    XData_sim[i,]<-as.vector(XData[i,names_query_index]%o%                              
                               c(polyFea(XData[i,names_prop_index],1),polyFea(XData[i,names_2order_index],2))
    )
  }
  col_names<-colnames(XData)[names_prop_index]
  print(col_names)
  row_names<-colnames(XData)[names_query_index]
  print(row_names)
  
  XData<-XData_sim
  
  
  labels<-YData
  qid<-srch_id
  XData<-cbind(qid,XData)
  print(dim(XData))
  XData<-data.frame(XData)
  
  search_id<-unique(srch_id)
  set.seed(1)
  if (testcode==0)
  {
    for (i in 1:fold)
    {
      index_search_id<-randperm(length(search_id),floor(length(search_id)*train_portion))
      train_id<-search_id[index_search_id]
      test_id<-setdiff(search_id,train_id)
      
      train_index<-vector()
      for (j in train_id)
      {
        train_index<-c(train_index,which(srch_id==j)) 
      }
      
      test_index<-setdiff(1:nrow(XData),train_index)
      
      xTr<-XData[train_index,]
      yTr<-YData[train_index]
      xTe<-XData[test_index,]
      yTe<-YData[test_index]
      
      qid_index<-sort(qid[train_index],index.return=T)$ix
      train_index<-train_index[qid_index]      
      qid_index<-sort(qid[test_index],index.return=T)$ix
      test_index<-test_index[qid_index]
      
      write.svmlight(labels[train_index],data.matrix(XData[train_index,]),file='svm_light_train_data')
      write.svmlight(labels[test_index],data.matrix(XData[test_index,]),file='svm_light_test_data')
      
      for (i in c(1e-3,0.01,0.1,1,10))
      {
        print(i)
        train_cmd<-paste('./svm_rank_learn -v 0  -l 2 -e 1e-4 -c', length(search_id)/2*i, 'svm_light_train_data model1.dat',sep=' ')
        test_cmd1<-paste('./svm_rank_classify  svm_light_train_data model1.dat  prediction.train1')
        test_cmd2<-paste('./svm_rank_classify  svm_light_test_data model1.dat  prediction.test1')
        system(train_cmd)
        system(test_cmd1)
        system(test_cmd2)
        prediction.train<-read.table('prediction.train1')
        prediction.test<-read.table('prediction.test1')
        
        print('Computing the training ndcg score')
        print(ndcg_wrapper3(srch_id[train_index], data.matrix(prediction.train),target[train_index]))
        
        print('Computing the test ndcg score')
        print(ndcg_wrapper3(srch_id[test_index], data.matrix(prediction.test),target[test_index]))
      }
    }
    
  }
  
}
