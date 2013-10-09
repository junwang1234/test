
#library
library('RSofia')
library('pracma')
library('glmnet')
source('ndcgEvaluation.R')



# main function
main_FS<-function(fold=5,train_portion=0.5,testcode=0,traincode=0){
  load('train.RData')
  subTrain<-train[1:3e5,]
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
  # NA_tm<-setdiff(NA_tm,c('visitor_hist_starrating','comp1_rate','comp2_rate','comp3_rate','comp4_rate'))
  print(dim(subTrain))
  print(NA_tm)
  
  subTrain<-subTrain[,setdiff(colnames(subTrain),NA_tm)]
  
  #************history records********************
  
#     temp<-subTrain[,'visitor_hist_starrating']
#     index1<-which(is.na(temp))
#     temp[index1]<-subTrain[index1,'prop_starrating']
#     temp<-abs(temp-subTrain[,'prop_starrating'])/3
#     subTrain[,'visitor_hist_starrating']<-temp
#   
  print(dim(subTrain))
  subTrain[is.na(subTrain)]<-0
  # subTrain<-knnImputation(subTrain,k=10)
  subTrain2<-subTrain
  subTrain2[,'date_time']<-date_fea_extr(subTrain2[,'date_time'])
  #  subTrain2<-centralImputation(subTrain3)
  attach(subTrain2)
  
  remove_feature<-c('visitor_location_country_id','site_id',
                    'prop_country_id','srch_destination_id',
                    'position','srch_id','prop_id',
                    'booking_bool','gross_bookings_usd','click_bool')
  feature_names<-setdiff(colnames(subTrain2),remove_feature)
  YData<-booking_bool*4+click_bool
  target<-booking_bool*4+click_bool
  #  YData<-factor(YData)
  XData<-subTrain2[,feature_names]
  #
  names_discrete<-c('srch_booking_window','srch_saturday_night_bool','random_bool',
                    'srch_adults_count','srch_children_count',
                    'srch_length_of_stay','srch_room_count','date_time'
  )
  #names_discrete<-c('prop_starrating','prop_review_score')
  #,'prop_starrating','prop_review_score'
  #names_numeric_normalization_whole<-c()

  names_remained<-c('prop_brand_bool','promotion_flag'
  )
  names_numeric<-setdiff(setdiff(feature_names,names_discrete),names_remained)
  
  
  #************preprocessing discrete feature*************
  quan<-list()
  for (str in names_discrete)
  {
    if (length(unique(XData[,str]))==2)
    {
      quan[[str]]<-sort(unique(XData[,str]))
    }
    else
    {
      quan[[str]]<- unique(quantile(XData[,str])[c(3)])
    }
  }
  #   quan[['srch_children_count']]<-c(0,1)
  #   quan[['srch_adults_count']]<-c(1,2,3)
  #   quan[['srch_room_count']]<-c(1,2,3,4)
  #   quan[['srch_length_of_stay']]<-c(1,3,5,10)
  #   quan[['srch_booking_window']]<-c(3,10,30)
  #   print(quan)
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
  
  #       XData[XData[,'srch_children_count']>1,'srch_children_count']<-1
  #       XData[XData[,'srch_adults_count']>3,'srch_adults_count']<-3
  #       XData[XData[,'srch_room_count']>2,'srch_room_count']<-2
  #       XData[XData[,'srch_length_of_stay']>3,'srch_length_of_stay']<-3
  #       XData[XData[,'srch_booking_window']>3,'srch_booking_window']<-3
  
  
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
  
  XData_whole<-XData[,names_remained]
  XData<-cbind(XData_whole,local_scale(XData[,names_numeric],srch_id))
  index_distance<-grep('orig_destination_distance',names_numeric)
  XData[,(ncol(XData_whole)+index_distance)] <- abs(XData[,(ncol(XData_whole)+index_distance)])
  
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
  #   names_query <- c('site_id','visitor_location_country_id',
  #                    'visitor_hist_starrating','visitor_hist_adr_usd',
  #                    'srch_length_of_stay','srch_booking_window',
  #                    'srch_adults_count','srch_children_count',
  #                    'srch_room_count','srch_saturday_night_bool',
  #                    'srch_query_affinity_score','orig_destination_distance')
  
  names_query <- c('srch_length_of_stay','srch_booking_window',
                   'srch_adults_count','srch_children_count',
                   'srch_room_count','srch_saturday_night_bool','date_time'
  )
  names_query_index<-vector()
  for(str in names_query)
  {
    names_query_index<-c(names_query_index,grep(str,colnames(XData)))
  }
  XData<-data.matrix(XData)
  names_prop_index<-setdiff(c(1:ncol(XData)),names_query_index)
  
  XData_sim<-matrix(nrow=nrow(XData),ncol=(length(names_prop_index)+1)*length(names_prop_index)*length(names_query_index))
  for (i in 1:nrow(XData))
  {
    XData_sim[i,]<-as.vector(XData[i,names_query_index]%o%                              
                               c(
                                 polyFea(XData[i,names_prop_index],2),
                                 polyFea(XData[i,names_prop_index],1)
                                 )
    )
  }
  
  #   
  #     XData_sim<-matrix(nrow=nrow(XData),ncol=ncol(XData)*ncol(XData))
  #     for (i in 1:nrow(XData))
  #     {
  #       XData_sim[i,]<-as.vector(XData[i,]%o%XData[i,])
  #     }
  
  XData<-XData_sim
  
  labels<-YData
  qid<-srch_id
  #XData<-cbind(labels,qid,XData)
  print(dim(XData))
  XData<-data.matrix(XData)
  
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
      
      instance_importance<-rep(1,length(yTr))
      
      instance_importance[yTr==5]<-100
      instance_importance[yTr==1]<-5
      lam_para<-c(0.005,0.01,0.05,0.1,0.5)
      alpha_para<-c(0,0.1,0.3,0.5,0.7)
      index_Fea_Sel <- list()

        for (k in 1:length(alpha_para))
        {
          model<-glmnet(x=xTr,y=yTr,weights=instance_importance, 
                        lambda=lam_para,alpha=alpha_para[k],standardize=F,
                        intercept=T)
          # print(model$beta)
          cc<-model$beta
          for (m in 1:ncol(cc))
          {
            print(length(which(abs(cc[,m])>=1e-4)))
            index_Fea_Sel[[(k-1)*ncol(cc)+m]]<-which(abs(cc[,m])>=1e-4)
          }
          
          prediction<-predict(model,xTe)
          print(alpha_para[k])
          print('Computing the ndcg score')
          for (j in 1:length(lam_para))
          {
            print(ndcg_wrapper3(srch_id[test_index], prediction[,j],yTe))
          }
          
          cc[abs(cc[,m])<1e-4] <- 0
          model$beta <-cc
          
          prediction<-predict(model,xTe)
          print(alpha_para[k])
          print('Computing the ndcg score')
          for (j in 1:length(lam_para))
          {
            print(ndcg_wrapper3(srch_id[test_index], prediction[,j],yTe))
          }
          
        }
      #browser()
       # save(index_Fea_Sel,file='index_Fel_Sel_nonNormalized.RData')

      
  
    }
  }
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
      temp <- as.vector(temp%o%fea)
    }
    temp
  }
  
}
args <- commandArgs(TRUE)
print(args)
main_FS(train_portion=0.5,testcode=0,traincode=0,fold=1)
