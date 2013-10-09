rm(list=ls())

#library
library('RSofia')
library('pracma')
library('DMwR')
source('ndcgEvaluation.R')
source('sofia.R')


# main function
main_rank<-function(fold=1,train_portion=0.5,testcode=0,traincode=0){
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
  
  
  
  
  
  
  remove_feature<-c('site_id', 'random_bool',                  
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
  
  names_discrete1<-c('srch_saturday_night_bool', 'srch_children_count','date_time'
                     
                     
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
  XData<-cbind(labels,qid,XData)
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
      
      
      

      for (k in c(0.01))
      {      
        
        model <- sofia(labels ~ ., data=XData[train_index,],iterations=1e9,
                       lambda=k,
                       random_seed=1,learner_type="sgd-svm",
                       loop_type="combined-rank",rank_step_probability=0.7)
        prediction <- predict(model, newdata=XData[test_index,], prediction_type = "linear")

        print(k)
        
        print('Computing the ndcg score')
        print(ndcg_wrapper3(srch_id[test_index], prediction,target[test_index]))
      }
      
    }
  }
  
  load('index_Fel_Sel_nonNormalized.RData')
  for (i_index in 1)
  {
    #cur_index<-as.numeric(index_Fea_Sel[[i_index]])+2
    cur_index<-c(1:ncol(XData))
    
    if (TRUE)
    {
  
  if (testcode==1)
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
#       
#       xTr<-XData[train_index,]
#       yTr<-YData[train_index]
#       xTe<-XData[test_index,]
       yTe<-YData[test_index]
      
      for( k in c(1e-2,5e-2,1e-1,5e-1,1))
      {
        model <- sofia(labels ~ ., data=XData[train_index,cur_index],iterations=1e9,
                       lambda=k,
                       random_seed=1,learner_type="sgd-svm",
                       loop_type="query-norm-rank",rank_step_probability=0.7)
      print(k)
      
      prediction <- predict(model, newdata=XData[train_index,cur_index], prediction_type = "linear")
      print(length(cur_index))
      print('Computing the training ndcg score')
      print(ndcg_wrapper3(srch_id[train_index], prediction,target[train_index]))
      
      
      prediction <- predict(model, newdata=XData[test_index,cur_index], prediction_type = "linear")
        print(length(cur_index))
        print('Computing the testing ndcg score')
       print(ndcg_wrapper3(srch_id[test_index], prediction,target[test_index]))
        
        
#         model.matrix<-matrix(model$weight[2:(length(cur_index)-1)],ncol=length(names_prop_index),byrow=T)
#         colnames(model.matrix)<-col_names
#         rownames(model.matrix)<-row_names
#       
#         print(model.matrix)
            
      
      }
      
     

      if (traincode==1)
      {
        load('train.RData')
        test<-train[(1e6+1):3.25e6,]
        target<-test[,'booking_bool']*4+test[,'click_bool']
      }
      else
        {
        load('test.RData')
        }
      detach(subTrain2)
      attach(test)
      print(dim(test))
      
      
      
      print('Modeling fitting finish')

#       test<-test[,setdiff(colnames(test),NA_tm)]
#       temp<-test[,'visitor_hist_starrating']
#       index1<-which(is.na(temp))
#       temp[index1]<-test[index1,'prop_starrating']
#       temp<-abs(temp-test[,'prop_starrating'])
#       test[,'visitor_hist_starrating']<-temp

      test<-test[,setdiff(colnames(test),NA_tm)]
      
      test[is.na(test)]<-0
      xTe<-test[,feature_names] 
      
      for (str in names_discrete)
      {
        cur_quan<-unlist(quan[str])
        num_dis_quan<-length(cur_quan)
        
        temp<-xTe[,str]
        index1<-which(xTe[,str]<=cur_quan[1])
        temp[index1]<-0
        for (i in c(1:num_dis_quan))
        {
          if (i==num_dis_quan)
          {
            index1<-which(xTe[,str]>cur_quan[i])
          }
          else
          {
            index1<-which(xTe[,str]>cur_quan[i]&xTe[,str]<=cur_quan[i+1])
          }
          temp[index1]<-i
        }
        xTe[,str]<-temp
      }
      
      
 
      #*******discrete features***********
      xTe_Dis<-xTe[,names_discrete]
     
       xTe_whole<-xTe[,names_remained]
 
      xTe<-cbind(xTe_whole,local_scale(xTe[,names_numeric],test[,'srch_id']))
      index_distance<-grep('orig_destination_distance',names_numeric)
      xTe[,(ncol(xTe_whole)+index_distance)] <- abs(xTe[,(ncol(XData_whole)+index_distance)])
      
    
      print(dim(xTe))
      
     
      
      start=1
      limit=100000
      total<-nrow(xTe)
      prediction<-vector(mode="list",length=total)
      while ((start+limit)<= total)
      {   
        xTe_cur<-xTe[start:(start+limit-1),] 
        
        xTe_Dis_cur<-xTe_Dis[start:(start+limit-1),]              
      
        for (i in 1:ncol(xTe_Dis_cur))
        {
          
          temp_data<-matrix(0,nrow=nrow(xTe_Dis_cur),ncol=length(level_dis[[i]]))
          current_level<-unlist(level_dis[[i]])
          colnames(temp_data)<-paste(names_discrete[i],current_level,sep='')
          
          for (k in 1:length(current_level))
          {     
            temp_data[xTe_Dis_cur[,i]==current_level[k],k] <- 1
          }
          xTe_cur<-cbind(xTe_cur,temp_data)
        }
        print(dim(xTe_cur))
        names_query_index<-vector()
        for(str in names_query)
        {
          names_query_index<-c(names_query_index,grep(str,colnames(xTe_cur)))
        }
        
        xTe_cur<-data.matrix(xTe_cur)
        names_prop_index<-setdiff(c(1:ncol(xTe_cur)),names_query_index)
        
        xTe_sim<-matrix(nrow=nrow(xTe_cur),ncol=(length(names_prop_index)+1)*length(names_prop_index)*length(names_query_index))
        for (i in 1:nrow(xTe_cur))
        {
   
          xTe_sim[i,]<-as.vector(xTe_cur[i,names_query_index]%o%
                                   c(as.vector(xTe_cur[i,names_prop_index]%o%xTe_cur[i,names_prop_index]),xTe_cur[i,names_prop_index])
                                 )
        }
        xTe_cur<-xTe_sim
        
        labels<-c(1:nrow(xTe_cur))
        
        qid<-rep(0,nrow(xTe_cur))
        junk<-cbind(labels,qid)
        xTe_cur<-cbind(junk,xTe_cur)
        
        xTe_cur<-data.frame(xTe_cur)
        colnames(xTe_cur)<- colnames(XData)
        
        print(dim(xTe_cur))
        print(any(is.na(xTe_cur)))
 
        prediction_temp <- predict(model, newdata=xTe_cur[,cur_index], prediction_type = "linear")

        for (ii in c(start:(start+limit-1)))
        {
          prediction[[ii]]<-prediction_temp[ii+1-start]
        }
        
        
        if (traincode==1)
        {
          print('Computing the ndcg score')
          print(ndcg_wrapper3(test[1:(start+limit-1),'srch_id'], as.numeric(prediction[1:(start+limit-1)]),
                              target[1:(start+limit-1)]))
        }
      
        
        start<-start+limit
        print(start)
        
        
      }
      
      

      xTe_cur<-xTe[start:total,] 
      
      xTe_Dis_cur<-xTe_Dis[start:total,]              
      
      for (i in 1:ncol(xTe_Dis_cur))
      {
        
        temp_data<-matrix(0,nrow=nrow(xTe_Dis_cur),ncol=length(level_dis[[i]]))
        current_level<-unlist(level_dis[[i]])
        colnames(temp_data)<-paste(names_discrete[i],current_level,sep='')
        
        for (k in 1:length(current_level))
        {     
          temp_data[xTe_Dis_cur[,i]==current_level[k],k] <- 1
        }
        xTe_cur<-cbind(xTe_cur,temp_data)
      }
      
      names_query_index<-vector()
      for(str in names_query)
      {
        names_query_index<-c(names_query_index,grep(str,colnames(xTe_cur)))
      }
      
      
      xTe_cur<-data.matrix(xTe_cur)
      names_prop_index<-setdiff(c(1:ncol(xTe_cur)),names_query_index)
      
      xTe_sim<-matrix(nrow=nrow(xTe_cur),ncol=(length(names_prop_index)+1)*length(names_prop_index)*length(names_query_index))
      for (i in 1:nrow(xTe_cur))
      {
        
        xTe_sim[i,]<-as.vector(xTe_cur[i,names_query_index]%o%
                                 c(as.vector(xTe_cur[i,names_prop_index]%o%xTe_cur[i,names_prop_index]),xTe_cur[i,names_prop_index])
                               )
      }
      xTe_cur<-xTe_sim
      
      labels<-c(1:nrow(xTe_cur))
      qid<-rep(0,nrow(xTe_cur))
      junk<-cbind(labels,qid)
      xTe_cur<-cbind(junk,xTe_cur)
      
      xTe_cur<-data.frame(xTe_cur)
      colnames(xTe_cur)<- colnames(XData)
      print(any(is.na(xTe_cur)))
      prediction_temp <- predict(model, newdata=xTe_cur[,cur_index], prediction_type = "linear")
      
      for (ii in start:total)
      {
        prediction[[ii]]<-prediction_temp[ii+1-start]
      }
      
      if (traincode==1)
      {
        print('Computing the ndcg score')
        print(ndcg_wrapper3(test[,'srch_id'], as.numeric(prediction),
                            test[,'click_bool']+4*test[,'booking_bool']))
      }
      else
      {
      print('writing final prediction')
      write_prediction(test[,'srch_id'],test[,'prop_id'],as.numeric(prediction))
      }
      
  }
    }
  }
}
         
args <- commandArgs(TRUE)
print(args)
main_rank(train_portion=0.5,testcode=1,traincode=1)

