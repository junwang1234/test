rm(list=ls())
#library
library('glmnet')
library('gbm')
source('ndcgEvaluation.R')
dyn.load('index_retrieval.so')
dyn.load('local_scale_c.so') 
dyn.load('local_zero_one_c.so')

#main function

main_fake_data <- function() {
 load('train.RData')
  train<-train[1:1e6,]
   
  remove_feature<-c(
   'position', 'srch_id','prop_id',
    'booking_bool','gross_bookings_usd','click_bool'
  )
  prop_id <- train[,'prop_id']
  booking_bool <- train[, 'booking_bool' ]
  click_bool <- train[,'click_bool' ]
  feature_names<-setdiff(colnames(train),remove_feature)
  train <- train[,feature_names]
 
  print( dim(train))
  date_fea<-date_fea_extr(train[,'date_time'])
  train[,'date_time'] <-date_fea$date_time  
  
  subTrain2 <-train
  rm(train) 
  attach(subTrain2)
  # visitor_hist_starrating--> emphasize the strong match signal


         star_diff<-mean(abs(subTrain2$prop_starrating[!is.na(subTrain2$visitor_hist_starrating)]-
                             subTrain2$visitor_hist_starrating[!is.na( subTrain2$visitor_hist_starrating)]))

         subTrain2$visitor_hist_starrating[is.na( subTrain2$visitor_hist_starrating)]<-
           subTrain2$prop_starrating[is.na( subTrain2$visitor_hist_starrating)]+star_diff

         subTrain2$visitor_hist_starrating<-abs(subTrain2$visitor_hist_starrating-subTrain2$prop_starrating)


        # visitor_hist_adr_usd --> emphasize the strong match signal
          subTrain2$visitor_hist_adr_usd<-log10( subTrain2$visitor_hist_adr_usd+1e-8)
          subTrain2$price_usd<-log10( subTrain2$price_usd+1e-8)

         hist_usd_diff<-mean(abs(subTrain2$price_usd[!is.na(subTrain2$visitor_hist_adr_usd)]-
                                 subTrain2$visitor_hist_adr_usd[!is.na(subTrain2$visitor_hist_adr_usd)]))


          subTrain2$visitor_hist_adr_usd[is.na( subTrain2$visitor_hist_adr_usd)]<-
            subTrain2$price_usd[is.na( subTrain2$visitor_hist_adr_usd)]+ hist_usd_diff

          subTrain2$visitor_hist_adr_usd<-abs(subTrain2$visitor_hist_adr_usd-subTrain2$price_usd)

	
          #prop_review_score<- min of socres of differnt stars of hotels--->treat new hotels doubtfully
          index_null_review_score<- which(is.na(subTrain2$prop_review_score))
           for (i in unique(subTrain2$prop_starrating[index_null_review_score]))
           {
             subTrain2$prop_review_score[index_null_review_score[prop_starrating[index_null_review_score]==i]]<-
             min(prop_review_score[prop_starrating==i],na.rm=T)
                print(min(prop_review_score[prop_starrating==i],na.rm=T))
           }


         #prop_location_score2<- min of socres of differnt stars of hotels--->treat new hotels doubtfully
          index_null_location_score2<- which(is.na(subTrain2$prop_location_score2))

           for (i in unique(subTrain2$prop_starrating[index_null_location_score2]))
           {
             subTrain2$prop_location_score2[index_null_location_score2[prop_starrating[index_null_location_score2]==i]]<-
               min(prop_location_score2[prop_starrating==i],na.rm=T)
              print(min(prop_location_score2[prop_starrating==i],na.rm=T))
           }


  #srch_booking_window
  subTrain2$srch_booking_window=log10(subTrain2$srch_booking_window+1e-8)

  #srch_query_affinity_score--->treat new hotels doubtfully
  subTrain2$srch_query_affinity_score[is.na(srch_query_affinity_score)]<-
  min(srch_query_affinity_score,na.rm=T)


  #prop_log_historical_price--> hotel with unknown value should be peanlized. (treat new hotels doubtfully)

 price_diff<- max(subTrain2$prop_log_historical_price[subTrain2$prop_log_historical_price!=0]
                -subTrain2$price_usd[subTrain2$prop_log_historical_price!=0])

  subTrain2$prop_log_historical_price[ subTrain2$prop_log_historical_price==0]<-
    subTrain2$price_usd[ subTrain2$prop_log_historical_price==0]+price_diff
  subTrain2$prop_log_historical_price<-subTrain2$prop_log_historical_price-subTrain2$price_usd

  #orig_destination_distance--->missing at random
  subTrain2$orig_destination_distance<-log10(subTrain2$orig_destination_distance+1e-8)
  subTrain2$orig_destination_distance[is.na(subTrain2$orig_destination_distance)]<-
    mean(subTrain2$orig_destination_distance,na.rm=T)


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



   print('Competitor feature  finished')
   
   names_discrete<-c('site_id','srch_booking_window','srch_saturday_night_bool','random_bool',
                    'srch_adults_count','srch_children_count','date_time',
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
                   'prop_starrating','prop_review_score',
                    'visitor_hist_starrating','visitor_hist_adr_usd',
                   'prop_log_historical_price','price_usd','srch_query_affinity_score'
                   )

  subTrain2 <- data.matrix(subTrain2)
  
  set.seed(1)
  for (fea in names_remained)
  {
  
    y <- subTrain2[,fea]
    x <- subTrain2[, setdiff(c(names_discrete,names_remained),fea)]
    
    index_whole <- sample(length(y))
    index_train <- index_whole[1:floor(length(y)/2)]
    index_test  <- setdiff(index_whole,index_train)
    

   #  y_pred <- glmnetWrapper(x[index_train,],y[index_train],x[index_test,])
    y_pred <- gbmWrapper(subTrain2,fea,index_train,index_test)
    #error
    print(fea)
    print( mean(abs(y_pred - y[index_test]))/mean(abs(mean(y[index_test])-y[index_test])))
  }




}

glmnetWrapper <- function(x_train,y_train,x_test)
{
   #k-fold CV select lambda parameter

    cvparamer<-cv.glmnet(x_train, y_train,
                         lambda=c(0.01,0.1,1,10,100),
                         type.measure="deviance",
                         nfolds=10 ,
                         parallel=T)

    # train final feature prediction model
     model<-glmnet(x_train, y_train,
            family=c("gaussian"),
            lambda = cvparamer$lambda.min
           )
     y_pred <- predict(model,newx=x_test)

}

gbmWrapper <- function(data,names_target,index_train,index_test)
{
  data <-data.frame(data)
  model<-gbm(as.formula(paste(names_target,"~ .")),
             data=data[index_train,],
	     distribution="gaussian",
             n.trees=1000,
             shrinkage=0.1,
             interaction.depth=1,
             bag.fraction=0.5,
             train.fraction=0.7,
             n.minobsinnode=length(index_train)*0.7*0.015,
	     verbose=F)
   best.iter <- gbm.perf(model,method="test") 
   print(best.iter)
   predict <- predict(model,data[index_test,],best.iter)
}

main_fake_data()


