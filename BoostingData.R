rm(list=ls())
#library
library('RSofia')
library('pracma')
library('DMwR')
source('ndcgEvaluation.R')



# main function
main_boosting_data<-function(fold=1,train_portion=0.5,testcode=0,traincode=0){
  load('train.RData')
  subTrain<-train
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
  
  subTrain2$srch_query_affinity_score[is.na(srch_query_affinity_score)]<-
    min(srch_query_affinity_score,na.rm=T)
  
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
  
  
  
  
  
  
  remove_feature<-c(                   
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
  
  names_discrete<-c('site_id','srch_booking_window','srch_saturday_night_bool','random_bool',
                    'srch_adults_count','srch_children_count','date_time',
                    'srch_length_of_stay','srch_room_count',
                    'visitor_location_country_id','prop_country_id','srch_destination_id'
  )
  #names_discrete<-c('prop_starrating','prop_review_score')
  #,'prop_starrating','prop_review_score'
  
  names_remained<-c('prop_brand_bool','promotion_flag','orig_destination_distance',
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
  
  
  
 
  
  XData_Dis<-XData[,names_discrete]
  
  XData_whole<-cbind(XData[,names_remained])
  XData<-cbind(XData_Dis,XData_whole,local_scale(XData[,names_numeric],srch_id))
  
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
      index_search_id<-sample(length(search_id))
      numT<-floor(length(search_id)/3)
      
      train_id<-search_id[index_search_id[1:numT]]
      validation_id<-search_id[index_search_id[(numT+1):(2*numT)]]      
      test_id<-setdiff(setdiff(search_id,train_id),validation_id)
      
      train_index<-vector('list',length=nrow(XData))
      validation_index<-vector('list',length=nrow(XData))
      test_index<-vector('list',length=nrow(XData))
      num_train<-1;
      num_test<-1;
      num_validate<-1;
      for (j in 1:length(srch_id))
      {
        if (srch_id[j] %in% train_id)
        {
          train_index[[num_train]] <-j
          num_train<-num_train+1
        }
        else
        {
          if (srch_id[j] %in% validation_id)
          {
            validation_index[[num_validate]] <-j
            num_validate<-num_validate+1
          }
          else
          {
            test_index[[num_test]] <-j
            num_test<-num_test+1
          }
        }
      }
      train_index<-unlist(train_index[[1:num_train]])
      validation_index<-unlist(train_index[[1:num_validate]])
      test_index<-unlist(train_index[[1:num_test]])
     
      xTr<-XData[train_index,]
      yTr<-YData[train_index]
      xTe<-XData[test_index,]
      yTe<-YData[test_index]
      
      xVa<-XData[validation_index,]
      yVa<-YData[validation_index]
      
      qid_index<-sort(qid[train_index],index.return=T)$ix
      train_index<-train_index[qid_index]      
      qid_index<-sort(qid[test_index],index.return=T)$ix
      test_index<-test_index[qid_index]
      
      qid_index<-sort(qid[validation_index],index.return=T)$ix
      validation_index<-validation_index[qid_index]
      
      write.svmlight(labels[train_index],data.matrix(XData[train_index,]),file='svm_light_train_data1')
      write.svmlight(labels[test_index],data.matrix(XData[test_index,]),file='svm_light_test_data1')
      write.svmlight(labels[validation_index],data.matrix(XData[validation_index,]),file='svm_light_validation_data1')
      
      
    }
    
  }
  
}

# java -jar RankLib.jar -train svm_light_train_data1  -ranker 6 -validate svm_light_test_data1 -metric2t NDCG@38 -save mymodel1.txt  
