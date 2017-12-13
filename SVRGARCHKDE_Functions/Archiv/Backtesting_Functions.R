#Estimation data
createData <- function(){
  
  library(quantmod)
  
  #Get NYSE data and convert to log returns
  id     <- "^NYA" # "DTE.DE"#"^GDAXI" #"^STOXX50E" #"
  data   <- getSymbols(id, source = "yahoo", auto.assign = FALSE,
                       from = "1986-07-01", to = "2016-06-30")
  series_raw <- data[,6]  #Get adjusted closing prices
  series <- na.omit(diff(log(series_raw)))*100  #Compute log returns
  
  x      <- na.omit(cbind(series, lag(series)))
  test_n <- 100
  
  features_tr <- x[1:(nrow(x) - test_n),-1]
  target_tr   <- x[1:(nrow(x) - test_n),1]
  
  test_data   <- x[(nrow(x) - test_n + 1):nrow(x),]
  
  # features_ts <- x[(nrow(x) - test_n + 1):nrow(x),-1]
  # target_ts   <- x[(nrow(x) - test_n + 1):nrow(x),1]
  # 
  result <- list(feat_tr = features_tr, 
                 targ_tr = target_tr,
                 test_data = test_data,
                 data = x,
                 original = series,
                 Trend = data
                 )
 return(result)
   
}


inSampleFit <- function(df, int = 0.05, plot = TRUE){
  
  fcast_upper <- df[,2] + quantile(df[,4], 1 - int)*df[,3] 
  fcast_lower <- df[,2] + quantile(df[,4], int)*df[,3]
  
  fcast <- data.frame(fcast_upper, fcast_lower)
  
  prop_upper <- sum(fcast_upper < df[,1])/nrow(df)  #
  prop_lower <- sum(fcast_lower > df[,1])/nrow(df)  #Downside risk: if greater 5% -> BAD
  
  prop       <- data.frame(prop_upper, prop_lower)
  
  result <- list(FCast = fcast, Prop = prop)
  
  if(plot){
    
    #Plot in-sample results
    org <- df[,1]
    
    ylim_max <- max(abs(c(org, fcast_upper, fcast_lower)))
    ylim_min <- -ylim_max
    
    plot(org, type = "p", col = "green", lwd = 1, ylim = c(ylim_min, ylim_max), 
         main = paste("In-Sample Fit", 1-2*int))
    lines(fcast_upper, col = "red", lwd = 2)
    lines(fcast_lower, col = "red", lwd = 2)
    errors <- data.frame(Index = 1:length(org), Real = org)
    out    <- errors$Real > fcast_upper | errors$Real < fcast_lower
    points(errors$Index[out], errors$Real[out], col = "black", pch = 16)
    
  }
  
  return(result)
}


####################################
###############################################################################
#Outof Sample Forecast - No reestimation

# #Mean prediction
# mean_pred <- predict(rnn_m, newdata = features_ts)
# 
# #Variance prediction
# mean_pred_res <- target_ts - mean_pred
# u             <- na.omit(cbind(mean_pred_res, lag(mean_pred_res)))^2
# 
# rnn_vv1       <- predict(rnn_v1, u[,-1])
# garch_res_vv1 <- na.omit(lag(rnn_vv1 - mean_pred_res[-1]))
# 
# rnn_vv2       <- predict(rnn_v2, cbind(u[-1,-1], garch_res_vv1))
# 
# fcast_95_out <- quantile(sc_eps, 0.95)*rnn_vv2 + mean_pred_res[-(1:2)]
# fcast_05_out <- quantile(sc_eps, 0.05)*rnn_vv2 + mean_pred_res[-(1:2)]
# 
# sum(fcast_95_out < as.numeric(target_ts[-(1:2)]))/length(fcast_95_out)  #
# sum(fcast_05_out > as.numeric(target_ts[-(1:2)]))/length(fcast_05_out)  #Downside risk: if greater 5% -> BAD

