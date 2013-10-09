	rm(list=ls())
	#library
	library('RSofia')
	library('pracma')
	library('DMwR')
	source('ndcgEvaluation.R')
	dyn.load('Poly2.so')
	dyn.load('index_retrieval.so')
	dyn.load('local_scale_c.so')
	dyn.load('local_zero_one_c.so')

	# main function
	main_boosting_data<-function(fold=1,train_portion=0.5,testcode=0,traincode=0){
	  load('train.RData')
	  subTrain2<-train
	  rm(train)
          attach(subTrain2)
	  #date_time -->binary value, working time and non-working time
	  date_fea<-date_fea_extr(subTrain2[,'date_time'])
	  subTrain2[,'date_time'] <-date_fea$date_time
	 #*************************************************************************************
	 # visitor_hist_starrating--> emphasize the strong match signal
         
         subTrain2$visitor_hist_starrating_bool <- rep(0,nrow(subTrain2))
         subTrain2$visitor_hist_starrating_bool[is.na(subTrain2$visitor_hist_starrating)] <- 1

         subTrain2$visitor_hist_starrating <- subTrain2$prop_starrating-subTrain2$visitor_hist_starrating
         subTrain2$visitor_hist_starrating[is.na(subTrain2$visitor_hist_starrating)] <-max(subTrain2$visitor_hist_starrating,na.rm=T)+1
         


          #*****************************************************************************************
	  # visitor_hist_adr_usd --> emphasize the strong match signal
	  subTrain2$visitor_hist_adr_usd<-log10( subTrain2$visitor_hist_adr_usd+1e-8)
	  subTrain2$price_usd<-log10( subTrain2$price_usd+1e-8)  
         
          #added feature
          subTrain2$visitor_hist_adr_usd_bool <- rep(0,nrow(subTrain2))
          subTrain2$visitor_hist_adr_usd_bool[is.na(subTrain2$visitor_hist_adr_usd)] <- 1

          subTrain2$visitor_hist_adr_usd<-subTrain2$price_usd- subTrain2$visitor_hist_adr_usd


          subTrain2$visitor_hist_adr_usd[is.na(subTrain2$visitor_hist_adr_usd)]<-
                                                               max(subTrain2$visitor_hist_adr_usd,na.rm=T)+1


           #**********************************************************************visitor_hist_adr_usd*******************8
           #prop_review_score<- min of socres of differnt stars of hotels--->treat new hotels doubtfully
	   subTrain2$prop_review_score2 <- subTrain2$prop_review_score
           # added feature
           review_star_diff<-max(subTrain2$prop_starrating[!is.na(subTrain2$prop_review_score2)]-
                                 subTrain2$prop_review_score2[!is.na(subTrain2$prop_review_score2)])
           subTrain2$prop_review_score2[is.na( subTrain2$prop_review_score2)]<-
           subTrain2$prop_starrating[is.na( subTrain2$prop_review_score2)]- (review_star_diff+1)

           subTrain2$prop_review_score2 <- subTrain2$prop_starrating-subTrain2$prop_review_score2


           #old feature
           subTrain2$prop_review_score[is.na(subTrain2$prop_review_score)]<-
                                           min(prop_review_score,na.rm=T)-1
           #********************************************************************************************* 
           #prop_location_score2<- min of socres of differnt stars of hotels--->treat new hotels doubtfully
           subTrain2$prop_location_score2[is.na(subTrain2$prop_location_score2)]<-
                                           min(prop_location_score2,na.rm=T)-1
          #********************************************************************************************
	  #srch_booking_window
           subTrain2$srch_booking_window=log10(subTrain2$srch_booking_window+1e-8)
          #*******************************************************************************************
          #srch_query_affinity_score--->treat new hotels doubtfully
          subTrain2$srch_query_affinity_score[is.na(srch_query_affinity_score)]<-
                                           min(srch_query_affinity_score,na.rm=T)-1
  
          #*************************************************************************************************
          #prop_log_historical_price--> hotel with unknown value should be peanlized. (treat new hotels doubtfully)
          #added feature
          subTrain2$prop_log_historical_price2 <- subTrain2$prop_log_historical_price
          price_diff<- min(subTrain2$prop_log_historical_price2[subTrain2$prop_log_historical_price2!=0]
                          -subTrain2$price_usd[subTrain2$prop_log_historical_price2!=0])

          subTrain2$prop_log_historical_price2[ subTrain2$prop_log_historical_price2==0]<-
                           subTrain2$price_usd[ subTrain2$prop_log_historical_price2==0]+price_diff-1
          
          subTrain2$prop_log_historical_price2 <-subTrain2$prop_log_historical_price2-subTrain2$price_usd
           
	  #old feature
           subTrain2$prop_log_historical_price[is.na(subTrain2$prop_log_historical_price)]<-
                                           min(subTrain2$prop_log_historical_price,na.rm=T)-1

  
          #orig_destination_distance--->missing at random
          subTrain2$orig_destination_distance<-log10(subTrain2$orig_destination_distance+1e-8)
          subTrain2$orig_destination_distance[is.na(subTrain2$orig_destination_distance)]<-
                    min(subTrain2$orig_destination_distance,na.rm=T)-1
    

 #***************************************old missing value handling*******************************
 subTrain2[is.na(subTrain2)]<-0
  
  #competitor
   subTrain2$comp1_rate_percent_diff[ subTrain2$comp1_rate_percent_diff>100] <- 100
 subTrain2$comp1_rate_percent_diff<- subTrain2$comp1_rate_percent_diff* subTrain2$comp1_rate/100

   subTrain2$comp2_rate_percent_diff[ subTrain2$comp2_rate_percent_diff>100] <- 100
 subTrain2$comp2_rate_percent_diff<- subTrain2$comp2_rate_percent_diff* subTrain2$comp2_rate/100
 
  subTrain2$comp3_rate_percent_diff[ subTrain2$comp3_rate_percent_diff>100] <- 100
 subTrain2$comp3_rate_percent_diff<- subTrain2$comp3_rate_percent_diff* subTrain2$comp3_rate/100

   subTrain2$comp4_rate_percent_diff[ subTrain2$comp4_rate_percent_diff>100] <- 100
 subTrain2$comp4_rate_percent_diff<- subTrain2$comp4_rate_percent_diff* subTrain2$comp4_rate/100
  
 subTrain2$comp5_rate_percent_diff[ subTrain2$comp5_rate_percent_diff>100] <- 100
 subTrain2$comp5_rate_percent_diff<- subTrain2$comp5_rate_percent_diff* subTrain2$comp5_rate/100

   subTrain2$comp6_rate_percent_diff[ subTrain2$comp6_rate_percent_diff>100] <- 100
 subTrain2$comp6_rate_percent_diff<- subTrain2$comp6_rate_percent_diff* subTrain2$comp6_rate/100

   subTrain2$comp7_rate_percent_diff[ subTrain2$comp7_rate_percent_diff>100] <- 100
 subTrain2$comp7_rate_percent_diff<- subTrain2$comp7_rate_percent_diff* subTrain2$comp7_rate/100

   subTrain2$comp8_rate_percent_diff[ subTrain2$comp8_rate_percent_diff>100] <- 100
 subTrain2$comp8_rate_percent_diff<- subTrain2$comp8_rate_percent_diff* subTrain2$comp8_rate/100

 #************************************************************************************************ 
 print('Competitor feature  finished')

  remove_feature<-c(                   
   'position', 'srch_id','prop_id',
    'booking_bool','gross_bookings_usd','click_bool'
  )
  
  feature_names<-setdiff(colnames(subTrain2),remove_feature)
  YData<-booking_bool*4+click_bool

  XData<-subTrain2[,feature_names]
  print(any(is.na(XData)))
  names_discrete<-c('site_id','srch_booking_window','srch_saturday_night_bool','random_bool',
                    'srch_adults_count','srch_children_count','date_time',#'date_fea$month',
                    'srch_length_of_stay','srch_room_count',
                    'visitor_location_country_id','prop_country_id','srch_destination_id'
  )

  
  names_remained<-c('prop_brand_bool','promotion_flag','orig_destination_distance',
		    'comp1_rate','comp2_rate','comp3_rate','comp4_rate',
                    'comp5_rate','comp6_rate','comp7_rate','comp8_rate',
                    'comp1_inv', 'comp2_inv','comp3_inv' ,'comp4_inv' ,'comp5_inv' ,
                    'comp6_inv' ,'comp7_inv' ,'comp8_inv' ,
                    'comp1_rate_percent_diff','comp2_rate_percent_diff','comp3_rate_percent_diff',
                    'comp4_rate_percent_diff','comp5_rate_percent_diff','comp6_rate_percent_diff',
                    'comp7_rate_percent_diff','comp8_rate_percent_diff',
                    'prop_location_score1','prop_location_score2',
		   'prop_starrating','prop_review_score','prop_review_score2', 
                    'visitor_hist_starrating','visitor_hist_starrating_bool',
                     'visitor_hist_adr_usd','visitor_hist_adr_usd_bool',
		   'prop_log_historical_price', 'prop_log_historical_price2',
                    'price_usd','srch_query_affinity_score'

                  )   
  
  
  names_context_diff<-c('prop_brand_bool','promotion_flag','orig_destination_distance',
                    'prop_location_score1','prop_location_score2',
                   'prop_starrating','prop_review_score','prop_review_score2',
                    'visitor_hist_starrating',
                     'visitor_hist_adr_usd',
                   'prop_log_historical_price', 'prop_log_historical_price2',
                    'price_usd','srch_query_affinity_score'

			)

  XData_Dis<-XData[,c(names_discrete,names_remained)]
  #xTemp <- local_scale(XData[,names_context_diff],srch_id)
  xTemp <- matrix(0,ncol=length(names_context_diff),nrow=nrow(XData))
  for (i in 1:length(names_context_diff))
  { 
  out<-rep(0,nrow(XData))
  xTemp[,i]<-.C('local_scale_c',as.double(out),as.double(XData[,names_context_diff[i]]),as.double(srch_id),as.integer(length(srch_id)))[[1]]
  }

  print('local_scale finished')


  xTemp2 <- matrix(0,ncol=length(names_context_diff),nrow=nrow(XData))
  for (i in 1:length(names_context_diff))
  {
  out<-rep(0,nrow(XData))
  xTemp2[,i]<-.C('local_zero_one_c',as.double(out),as.double(XData[,names_context_diff[i]]),as.double(srch_id),as.integer(length(srch_id)))[[1]]
  }

  print('local_zero_one_norm finished')


  browser()
  XData<-cbind(XData_Dis,xTemp,xTemp2)
  rm(xTemp)
  rm(xTemp2)  
  labels<-YData
  qid<-srch_id
  XData<-cbind(qid,XData)
  print(dim(XData))
  XData<-data.frame(XData)
 
   search_id<-unique(srch_id)
	set.seed(1)


      index_search_id<-sample(length(search_id))
      numT<-floor(4*length(search_id)/5)
      numT2<-floor(length(search_id)/10)
      
      train_id<-search_id[index_search_id[1:numT]]
      validation_id<-search_id[index_search_id[(numT+1):(numT2+numT)]]
      
      test_id<-setdiff(setdiff(search_id,train_id),validation_id)
      
	index_final <- rep(0,length(srch_id))
	index_final <- .C('index_retrieval',as.double(index_final),as.double(srch_id),as.integer(length(srch_id)),
 	as.double(train_id),as.integer(length(train_id)),as.double(validation_id),as.integer(length(validation_id)),
	as.double(test_id),as.integer(length(test_id)))[[1]]
	train_index<-which(index_final==1)
	validation_index<-which(index_final==2)
	test_index<-which(index_final==3)
      
      print('sampling finished')
      
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
      
      train_data <- cbind(labels[c(train_index,test_index)], XData[c(train_index,test_index),])
      validation_data <-cbind(labels[validation_index],XData[validation_index,])
      
     #  XData <-XData[c(train_index,test_index,validation_index),]
     #  save(XData,file='XData.RData')  
  
#    save(validation_data,file='validation_data.RData')
  #    save(train_data,file='train_data.RData')
      write.svmlight(labels[c(train_index,test_index)],data.matrix(XData[c(train_index,test_index),]),file='train_data')
      #write.svmlight(labels[test_index],data.matrix(XData[test_index,]),file='validation_final_data')
      write.svmlight(labels[validation_index],data.matrix(XData[validation_index,]),file='validation_data')
      
  
}
main_boosting_data()
# java -jar RankLib.jar -train svm_light_train_data1  -ranker 6 -validate svm_light_test_data1 -metric2t NDCG@38 -$save mymodel1.txt  
