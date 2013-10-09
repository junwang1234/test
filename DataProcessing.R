
  load('train.RData')
  subTrain<-train
  rm(train)
  index_his_star_non_missing <- which(!is.na(subTrain$visitor_hist_starrating))
  index_book <-index_his_star_non_missing[ which((subTrain$booking_bool[
				index_his_star_non_missing]+
                                subTrain$click_bool[index_his_star_non_missing
                                ])==1)]
  stardiff_book <- subTrain$prop_starrating[index_book]-subTrain$visitor_hist_starrating[index_book]
  print(quantile(stardiff_book,probs=seq(0,1,0.1)))

  index_non_book<-setdiff( index_his_star_non_missing, index_book)
  stardiff_non_book <- subTrain$prop_starrating[index_non_book]-subTrain$visitor_hist_starrating[index_non_book]
  print(quantile(stardiff_non_book,probs=seq(0,1,0.1)))
  browser()
