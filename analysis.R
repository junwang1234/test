 FeaCount <- read.table('output.txt')
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
                   'prop_starrating','prop_review_score',
                    'visitor_hist_starrating','visitor_hist_adr_usd',
                   'prop_log_historical_price','price_usd','srch_query_affinity_score'

  )


  names_context_diff<-c('prop_brand_bool2','promotion_flag2','orig_destination_distance2',
                    'prop_location_score12','prop_location_score22',
                   'prop_starrating2','prop_review_score2',
                    'visitor_hist_starrating2','visitor_hist_adr_usd2',
                   'prop_log_historical_price2','price_usd2','srch_query_affinity_score2'

                        )

 names_context_diff2<-c('prop_brand_bool1','promotion_flag1','orig_destination_distance1',
                    'prop_location_score11','prop_location_score21',
                   'prop_starrating1','prop_review_score1',
                    'visitor_hist_starrating1','visitor_hist_adr_usd1',
                   'prop_log_historical_price1','price_usd1','srch_query_affinity_score1'

                        )

FeaNames<-c(names_discrete,names_remained,names_context_diff,names_context_diff2)
rownames(FeaCount) <- FeaNames
browser()
