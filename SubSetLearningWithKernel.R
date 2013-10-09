
#library
library('pracma')
library('randomForest')
library('e1071')
library('glmnet')
library('DMwR')
library('kernlab')
source('ndcgEvaluation.R')
# main function

main_Kernel<-function(fold=3,train_portion=0.5,testcode=0){
  load('subTrain2.RData')  
   #subTrain2<-centralImputation(subTrain3)
  attach(subTrain2)
  
  remove_feature<-c('visitor_location_country_id',
                    'prop_country_id','srch_destination_id',
                    'position','srch_id','date_time','prop_id',
                    'booking_bool','gross_bookings_usd','click_bool')
  feature_names<-setdiff(colnames(subTrain2),remove_feature)
  YData<-booking_bool*4+click_bool
  YData<-YData
  XData<-subTrain2[,feature_names]
  
  
  names_discrete<-c('prop_starrating','prop_review_score','site_id',
                    'srch_adults_count','srch_children_count',
                    'srch_length_of_stay','srch_room_count')
 
  names_numeric<-setdiff(feature_names,names_discrete)
  
  level_dis<-list()
  for (i in 1:length(names_discrete))
  {
    idx=grep(names_discrete[i],colnames(XData))
    level_dis[[i]]<-as.numeric(levels(factor(XData[,idx])))
  }
  
  #XData<-scale(XData)
  #*******discrete features***********
  XData_Dis<-XData[,names_discrete]
  XData<-scale( XData[,names_numeric])
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
      
      #rf<-randomForest(x=xTr,y=yTr,ntree=50)
      
      class_importance<-c(1,5,20)
      names(class_importance)<-c('0','1','5')
     
      lam_para<-c(0.05,0.1,0.5)
      C_para<-c(0.01,0.1,1,10,100)
      for (k in 1:length(C_para))
      {
       
        #model<-glmnet(x=xTr,y=yTr,weights=instance_importance, lambda=lam_para,alpha=alpha_para[k])
        
        
#         model<-ksvm(x=xTr,y=yTr,type='eps-svr',kernel ="rbfdot",
#                     class.weights=class_importance,
#                     prob.model = T, C=C_para[k])
        model<-ksvm(x=xTr,y=yTr,type='eps-svr',kernel ="polydot",
                    kpar=list(degree=2,offset=1),C=C_para[k],
                    epsilon=0.5)
        prediction<-predict(model,xTe)
        
        print(C_para[k])
        
        print('Computing the ndcg score')
        print(ndcg_wrapper3(srch_id[test_index], prediction,yTe))
#          target<-as.numeric(yTe)
#         target_new<-target
#         target_new[target==3]<-5
#         target_new[target==2]<-1
#         target_new[target==1]<-0
#    
#         print(ndcg_wrapper3(srch_id[test_index], as.numeric(prediction),target_new))
      
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
    class_importance<-c(1,5,10)
    names(class_importance)<-c('0','1','5')
    
   
    model<-ksvm(x=xTr,y=yTr,type='spoc-svc',kernel ="polydot",
                kpar = list(degree=2,offset=1),class.weights=class_importance,
                prob.model = F, C=0.01)
    
#     prediction<-predict(model,xTe)
#     
#    
#     
#     print('Computing the ndcg score')
#     
#     print(ndcg_wrapper2(srch_id[test_index], as.numeric(prediction),as.numeric(yTe)))
    
    
    load('test_filled.RData')
    #test_filled<-train_filled
#     detach(subTrain2)
#     attach(test_filled)
    
#      YData<-booking_bool*4+click_bool
#      YData<-factor(YData)
    

    
    feature_names<-setdiff(colnames(test_filled),remove_feature)
    names_discrete<-c('prop_starrating','prop_review_score','site_id',
                      'srch_adults_count','srch_children_count',
                      'srch_length_of_stay','srch_room_count')
    
    names_numeric<-setdiff(feature_names,names_discrete)
    
    test_scaled<-scale(test_filled[,names_numeric])
  
    start=1
    limit=100000
    total<-nrow(test_filled)
    prediction<-vector(mode="list",length=total)
    while ((start+limit)<= total)
    {    
    xTe<-test_filled[start:(start+limit-1),feature_names]              
    #*******discrete features***********
    xTe_Dis<-xTe[,names_discrete]
    xTe<-test_scaled[start:(start+limit-1),names_numeric]
    for (i in 1:ncol(xTe_Dis))
    {
      
      temp_data<-matrix(0,nrow=nrow(xTe_Dis),ncol=length(level_dis[[i]]))
      current_level<-unlist(level_dis[[i]])
      colnames(temp_data)<-paste(names_discrete[i],current_level,sep='')

      for (k in 1:length(current_level))
      {     
        temp_data[xTe_Dis[,i]==current_level[k],k] <- 1
      }
      xTe<-cbind(xTe,temp_data)
    }

    prediction_temp<-predict(model,xTe)
    for (ii in start:(start+limit-1))
    {
    prediction[[ii]]<-prediction_temp[ii+1-start]
    }
     if ((start%%1000000)==1)
     {
#     print('Computing the ndcg score')
#     
#     print(ndcg_wrapper3(srch_id[1:start], as.numeric(prediction[1:start]),as.numeric(YData[1:start])))
       write_prediction(test_filled$srch_id[1:start],test_filled$prop_id[1:start],as.numeric(prediction[1:start]))
     }
    start<-start+limit
    print(start)
    
     
    }
    
    
    xTe<-test_filled[start:total,feature_names]              
    #*******discrete features***********
    xTe_Dis<-xTe[,names_discrete]
    xTe<-test_scaled[start:total,names_numeric]
    for (i in 1:ncol(xTe_Dis))
    {
      
      temp_data<-matrix(0,nrow=nrow(xTe_Dis),ncol=length(level_dis[[i]]))
      current_level<-unlist(level_dis[[i]])
      colnames(temp_data)<-paste(names_discrete[i],current_level,sep='')
      
      for (k in 1:length(current_level))
      {     
        temp_data[xTe_Dis[,i]==current_level[k],k] <- 1
      }
      xTe<-cbind(xTe,temp_data)
    }
    
    prediction_temp<-predict(model,xTe)
    for (ii in start:total)
    {
      prediction[[ii]]<-prediction_temp[ii+1-start]
    }
#      print('Computing the final ndcg score')
#      
#      print(ndcg_wrapper3(srch_id, as.numeric(prediction),as.numeric(YData)))
    
   # print('writing final prediction')
    write_prediction(test_filled$srch_id,test_filled$prop_id,as.numeric(prediction))
  }
}
