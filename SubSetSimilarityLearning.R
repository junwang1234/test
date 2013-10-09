
#library
library('pracma')
library('randomForest')
library('e1071')
library('glmnet')
library('DMwR')
library('kernlab')
source('ndcgEvaluation.R')
# main function

main_similarity<-function(fold=3,train_portion=0.5,testcode=0){
  load('subTrain2.RData')  
 #  subTrain2<-centralImputation(subTrain3)
  attach(subTrain2)
  
  remove_feature<-c('visitor_location_country_id','site_id',
                    'prop_country_id','srch_destination_id',
                    'position','srch_id','date_time','prop_id',
                    'booking_bool','gross_bookings_usd','click_bool')
  feature_names<-setdiff(colnames(subTrain2),remove_feature)
  YData<-booking_bool*4+click_bool
  #  YData<-factor(YData)
  XData<-subTrain2[,feature_names]

  names_discrete<-c('prop_starrating','prop_review_score',
                    'srch_adults_count','srch_children_count',
                    'srch_length_of_stay','srch_room_count')
  #names_discrete<-c('prop_starrating','prop_review_score')
  
  names_numeric<-setdiff(feature_names,names_discrete)

  #XData<-scale(XData)
  #*******discrete features***********
  XData_Dis<-XData[,names_discrete]
  XData<-scale( XData[,names_numeric])
  for (i in 1:ncol(XData_Dis))
  {
    level_dis<-as.numeric(levels(factor(XData_Dis[,i])))
    temp_data<-matrix(0,nrow=nrow(XData_Dis),ncol=length(level_dis))
    colnames(temp_data)<-paste(names_discrete[i],level_dis,sep='')
    for (k in 1:length(level_dis))
    {
      temp_data[XData_Dis[,i]==level_dis[k],k] <- 1
    }
    XData<-cbind(XData,temp_data)
  }
  
  #****************Query feature************
  names_query <- c('site_id','visitor_location_country_id',
                   'visitor_hist_starrating','visitor_hist_adr_usd',
                   'srch_length_of_stay','srch_booking_window',
                   'srch_adults_count','srch_children_count',
                   'srch_room_count','srch_saturday_night_bool',
                   'srch_query_affinity_score','orig_destination_distance')
  names_query_index<-vector()
  for(str in names_query)
  {
    names_query_index<-c(names_query_index,grep(str,colnames(XData)))
  }
  names_prop_index<-setdiff(c(1:ncol(XData)),names_query_index)
  
  XData_sim<-matrix(nrow=nrow(XData),ncol=length(names_prop_index)*length(names_query_index))
  for (i in 1:nrow(XData))
  {
    XData_sim[i,]<-as.vector(XData[i,names_query_index]%o%XData[i,names_prop_index])
  }
  XData<-XData_sim
  
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
      
      print(dim(XData))
      
      #rf<-randomForest(x=xTr,y=yTr,ntree=50)
      instance_importance<-rep(1,length(yTr))
      
      instance_importance[yTr==5]<-100
      instance_importance[yTr==1]<-5
      lam_para<-c(0.005,0.01,0.05,0.1,0.5,1,5,10,100)
      alpha_para<-c(0.1,0.5,1)
      for (k in 1:length(alpha_para))
      {
        # svm_model<-svm(x=xTr,y=yTr,type='eps-regression',kernel='linear',cost=lam_para[k],scale=F)#,alpha=1,nlambda=lam_para[k])
        # ********working with data frame***********
        #       train_data<-as.data.frame(cbind(xTr,yTr))    
        #       test_data<-as.data.frame(cbind(xTe,yTe))
        # 
        #       colnames(train_data)[ncol(train_data)]<-'labels'
        #       colnames(test_data)[ncol(test_data)]<-'labels'
        #       model<-lm(formula=labels ~ . , data=train_data,weights=instance_importance)
        #       prediction<-predict(model,test_data)
        #       
        model<-glmnet(x=xTr,y=yTr,weights=instance_importance, 
                      lambda=lam_para,alpha=alpha_para[k],
                      intercept=T)
        # print(model$beta)
        #print(summary(model))
        # plot(model)
        
        prediction<-predict(model,xTe)
        #     print(length(which(prediction==yTe))/length(yTe))
        #     print(table(prediction,yTe))
        print(alpha_para[k])
        
        
        print('Computing the ndcg score')
        for (j in 1:length(lam_para))
        {
          print(ndcg_wrapper3(srch_id[test_index], prediction[,j],yTe))
        }
      }
      
    }
  }
  ##********final testing code********************
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
    
    xTr<-XData[train_index,]
    yTr<-YData[train_index]
    xTe<-XData[test_index,]
    yTe<-YData[test_index]
    
    #rf<-randomForest(x=xTr,y=yTr,ntree=50)
    instance_importance<-rep(1,length(yTr))
    
    instance_importance[yTr==5]<-10
    instance_importance[yTr==1]<-5
    lam_para<-c(0.1)
    alpha_para<-seq(0.1,1,0.2)
    
    model<-glmnet(x=xTr,y=yTr,weights=instance_importance, lambda=lam_para)
    load('test_filled.RData')
    
    remove_feature<-c('random_bool','position','srch_id','date_time','prop_id','booking_bool','gross_bookings_usd','click_bool')
    feature_names<-setdiff(colnames(subTrain2),remove_feature)
    xTe<-test_filled[,feature_names]    
    xTe<-scale(xTe)    
    prediction<-predict(model,xTe)
    print('writing final prediction')
    write_prediction(test_filled$srch_id,test_filled$prop_id,prediction)
  }
}
