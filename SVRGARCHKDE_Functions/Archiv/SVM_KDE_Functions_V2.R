svm_kde <- function(x, y, q){
  
  #####################################
  #Set parameters
  svm_eps   <- 0.5
  svm_cost  <- 10^(-1)
  sigma     <- 0.02
  svm_gamma <- 1/(2*sigma^2)
  
  results        <- data.frame(matrix(NA, nrow = 1, ncol = 4))
  names(results) <- c("Mean", "Upper", "Lower", "Vola")
  
  ################################################
  
  features_tr <- x
  target_tr   <- y
  
  #SVM for mean
  svm     <- svm(x = features_tr, y = target_tr, scale = FALSE,
                 type = "eps-regression", kernel = "radial",
                 gamma = svm_gamma, cost = 0.01, epsilon = svm_eps)
  
  mean <- xts(x = svm$fitted, order.by = index(target_tr))
  eps  <- svm$residuals
  
  
  mean_pred <- predict(svm, newdata = target_tr[nrow(target_tr)])
  
  svm_var_eps <- quantile(scale(eps)^2, 0.65)#abs(quantile(eps, 0.5))#quantile(eps^2, 0.25)
  
  #RNN: Fit Variance
  u        <- na.omit(cbind(eps, lag(eps)))^2
  
  rnn_v1   <- svm(x = u[,-1], y = u[,1], scale = TRUE,
                  type = "eps-regression", kernel = "radial",
                  gamma = svm_gamma, cost = svm_cost, epsilon = svm_var_eps)
  
  #w           <- xts(x = rnn_v1$fitted, order.by = index(rnn_v1$residuals))
  #w[w<=0] <- NA
  #w <- na.locf(w)
  
  #w           <- rnn_v1$residuals
  w           <- xts(x = rnn_v1$residuals, order.by = index(u))
  garch_input <- na.omit(cbind(u, lag(w)))
  
  rnn_v2  <- svm(x = garch_input[,2:3], y = garch_input[,1], scale = TRUE, 
                 type = "eps-regression", kernel = "radial",
                 gamma = svm_gamma, cost = svm_cost, epsilon = svm_var_eps)
  
  fcast_vola                <- rnn_v2$fitted
  fcast_vola[fcast_vola<=0] <- NA
  fcast_vola                <- na.locf(fcast_vola)
  
  vola_new_dat <- cbind(u[nrow(u),1], w[length(w)]) #cbind(eps[nrow(eps)], w[nrow(w)])#
  vola_pred    <- predict(rnn_v2, newdata = vola_new_dat)
  
  
  # Fit quantile with peaks over threshold
  u_sc  <- eps[-(1:2)]/sqrt(fcast_vola)
  u_sc  <- scale(u_sc)

  q_upper <- QKDE(q, c(0, max(u_sc)*1.5), data = u_sc)
  q_lower <- -QKDE(q, c(0, max(-u_sc)*1.5), data = -u_sc)
  
  #q_upper <- quantile(u_sc, 0.95)
  #q_lower <- -quantile(-u_sc, 0.95)
  
  #Gather results
  results$Mean  <- mean_pred
  results$Upper <- q_upper
  results$Lower <- q_lower
  results$Vola  <- ifelse(vola_pred <= 0, NA, sqrt(vola_pred))
  #results$Epsilon  <- svm_var_eps
  results$RealUpper <- QKDE(q, c(0, max(y)*1.5), data = y)
  results$RealLower <- -QKDE(q, c(0, max(-y)*1.5), data = -y)
  
  return(results)
  
}


###############################################################################
svm_kde_vola <- function(x, y, q){
  
  #####################################
  #Set parameters
  svm_eps   <- 0.5
  svm_cost  <- 10^(-1)
  sigma     <- 0.02
  svm_gamma <- 1/(2*sigma^2)
  
  results        <- data.frame(matrix(NA, nrow = 1, ncol = 4))
  names(results) <- c("Mean", "Upper", "Lower", "Vola")
  
  ################################################
  
  features_tr <- x
  target_tr   <- y
  
  #SVM for mean
  svm     <- svm(x = features_tr, y = target_tr, scale = FALSE,
                 type = "eps-regression", kernel = "radial",
                 gamma = svm_gamma, cost = 0.01, epsilon = svm_eps)
  
  mean <- xts(x = svm$fitted, order.by = index(target_tr))
  eps  <- svm$residuals
  
  
  mean_pred <- predict(svm, newdata = target_tr[nrow(target_tr)])
  
  svm_var_eps <- quantile(scale(eps)^2, 0.65)#abs(quantile(eps, 0.5))#quantile(eps^2, 0.25)
  
  #RNN: Fit Variance
  u        <- na.omit(cbind(eps, lag(eps)))^2
  
  rnn_v2   <- svm_variance(x = u[,-1], y = u[,1], m = "ARMA",
                           gamma = svm_gamma, cost = svm_cost, epsilon = svm_var_eps)
  
  fcast_vola                <- rnn_v2$model$fitted
  fcast_vola[fcast_vola<=0] <- NA
  fcast_vola                <- na.locf(fcast_vola)
  w <- rnn_v2$lagRes
  vola_new_dat <- cbind(u[nrow(u),1], w[length(w)]) #cbind(eps[nrow(eps)], w[nrow(w)])#
  vola_pred    <- predict(rnn_v2$model, newdata = vola_new_dat)
  
  
  # Fit quantile with peaks over threshold
  u_sc  <- eps[-(1:2)]/sqrt(fcast_vola)
  u_sc  <- scale(u_sc)
  
  q_upper <- QKDE(q, c(0, max(u_sc)*1.5), data = u_sc)
  q_lower <- -QKDE(q, c(0, max(-u_sc)*1.5), data = -u_sc)
  
  #q_upper <- quantile(u_sc, 0.95)
  #q_lower <- -quantile(-u_sc, 0.95)
  
  #Gather results
  results$Mean  <- mean_pred
  results$Upper <- q_upper
  results$Lower <- q_lower
  results$Vola  <- ifelse(vola_pred <= 0, NA, sqrt(vola_pred))
  #results$Epsilon  <- svm_var_eps
  results$RealUpper <- QKDE(q, c(0, max(y)*1.5), data = y)
  results$RealLower <- -QKDE(q, c(0, max(-y)*1.5), data = -y)
  
  return(results)
  
}



###############################################################################
svm_variance <- function(x, y, m, gamma, cost, epsilon){
  
  # if(type == "AR"){
  # 
  #   model   <- svm(x = x, y = y, scale = TRUE,
  #                   type = "eps-regression", kernel = "radial",
  #                   gamma = gamma, cost = cost, epsilon = epsilon)
  # 
  # }
  
  if(m == "ARMA"){
    
    rnn_v1   <- svm(x = x, y = y, scale = TRUE,
                    type = "eps-regression", kernel = "radial",
                    gamma = gamma, cost = cost, epsilon = epsilon)
    
    #w           <- xts(x = rnn_v1$fitted, order.by = index(rnn_v1$residuals))
    #w[w<=0] <- NA
    #w <- na.locf(w)
    
    #w           <- rnn_v1$residuals
    w           <- xts(x = rnn_v1$residuals, order.by = index(y))
    garch_input <- na.omit(cbind(y, x, lag(w)))
    
    model  <- svm(x = garch_input[,2:3], y = garch_input[,1], scale = TRUE, 
                   type = "eps-regression", kernel = "radial",
                   gamma = gamma, cost = cost, epsilon = epsilon)
    
  }
  
  result <- list(model = model, lagRes = w)
  
  return(result)
  
}



###############################################################################
plotResults <- function(results, allData){
  
  
  results$True <- allData[(nrow(allData) - test_n + 1):nrow(allData),1]
  
  #Analyze Results
  
  results$FCast_Upper <- results$Mean + results$Upper*results$Vola
  results$FCast_Lower <- results$Mean + results$Lower*results$Vola
  
  prop_upper <- sum(results$FCast_Upper < results$True)/nrow(results)  #
  prop_lower <- sum(results$FCast_Lower > results$True)/nrow(results)  #Downside risk: if greater 5% -> BAD
  
  #Plot in-sample results
  org         <- as.numeric(results$True)
  fcast_upper <- results$FCast_Upper
  fcast_lower <- results$FCast_Lower
  
  ylim_max <- max(abs(c(org, fcast_upper, fcast_lower)))
  ylim_min <- -ylim_max
  
  plot(org, type = "p", col = "green", lwd = 1, ylim = c(ylim_min, ylim_max), ylab = "Value")
  
  lines(fcast_upper, col = "red", lwd = 2)
  lines(fcast_lower, col = "red", lwd = 2)
  errors <- data.frame(Index = 1:length(org), Real = org)
  out    <- errors$Real > fcast_upper | errors$Real < fcast_lower
  points(errors$Index[out], errors$Real[out], col = "blue", pch = 16)
  title("99.5% and 0.5% VaR-Forecast for NYSE")

  print(prop_upper)
  print(prop_lower)
  
  return(results)
  
}
