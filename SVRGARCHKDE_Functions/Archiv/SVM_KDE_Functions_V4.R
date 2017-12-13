svm_kde_dopar <- function(data, q, block, test, variance_mdl){
  
  if(nrow(data) < block + test) stop("Number of rows in 'data' is less than block + test")
  
  source("C:/Users/Marius/Desktop/Simulation/SVRGARCHKDE_Functions/Source_File/Source_SVRGARCHKDE.R")
  library(doParallel)
  library(quantmod)
  
  allData    <- data
  test_n     <- test 
  block_size <- block
  loop_start <- nrow(allData) - test_n
  loop_end   <- nrow(allData) - 1
  
  results    <- data.frame(Mean = numeric(),
                           Upper = numeric(),
                           Lower = numeric(),
                           Vola = numeric())
  
  cl <- makeCluster(4)
  registerDoParallel(cl)
  
  results <- foreach(i = loop_start:loop_end, .combine = "rbind") %dopar% {
    
    source("C:/Users/Marius/Desktop/Simulation/SVRGARCHKDE_Functions/Source_File/Source_SVRGARCHKDE.R")
    library(quantmod)
    library(e1071)
    library(zoo)
    
    x            <- allData[(i-block_size+1):i,-1]
    y            <- allData[(i-block_size+1):i,1]
    variance.mdl <- variance_mdl 
    
    svm_kde(x = x, y = y, q = q, variance.mdl = variance.mdl, kernel = "gaussian")
    
    #KDE_VaR(y, q)
    
  }
  stopCluster(cl)
  
  results$Vola <- na.locf(results$Vola)
  
  results <- plotResults(results, allData, test_n, q)
  
  return(results)
}





svm_kde <- function(x, y, q, variance.mdl, kernel){
  
  #############################################################################
  # 1. Fit mean model
  #############################################################################
  
  # Set parameters
  svm_mean_eps     <- 0.5
  svm_mean_cost    <- 10^(-2)
  sigma            <- 0.02
  svm_mean_gamma   <- 1/(2*sigma^2)
  
  # SVM for mean
  svm_mean         <- svm(x = x, y = y, type = "eps-regression", kernel = "radial",
                          gamma = svm_mean_gamma,
                          cost = svm_mean_cost,
                          epsilon = svm_mean_eps)
  
  # Get fitted values and residual (format as xts for lagging in variance fitting)
  mean             <- xts(x = svm_mean$fitted, order.by = index(y))
  eps              <- xts(x = svm_mean$residuals, order.by = index(y))
  
  # Prediction of next periods unseen return
  mean_pred        <- predict(svm_mean, newdata = y[nrow(y)])
  
 
  #############################################################################
  # 2. Fit variance model
  #############################################################################
  
  # Set paramters
  svm_var_eps       <- quantile(scale(eps)^2, variance.mdl$eps_quantile)
  svm_var_cost      <- 10^(-1)
  sigma             <- 0.02
  svm_var_gamma     <- 1/(2*sigma^2)
  
  
  # Get data
  u                 <- na.omit(cbind(eps, lag(eps)))^2
  
  # Estimate model 
  svm_var           <- svm_variance(x = u[,-1], y = u[,1], m = variance.mdl,
                                   gamma = svm_var_gamma, 
                                   cost = svm_var_cost, 
                                   epsilon = svm_var_eps)
  
  # Get fitted variances to standardize residuals
  if(variance.mdl$model == "AR")   fcast_vola <- svm_var$fitted
  if(variance.mdl$model == "ARMA") fcast_vola <- svm_var$model$fitted
  
  # Replace negativ variances by last positiv value
  fcast_vola[fcast_vola<=0] <- NA 
  fcast_vola                <- na.locf(fcast_vola)
  
  # Prediction of next periods unseen variance
  if(variance.mdl$model == "AR")   vola_new_dat <- u[nrow(u),1]
  if(variance.mdl$model == "ARMA") vola_new_dat <- cbind(u[nrow(u),1], svm_var$lagRes[length(svm_var$lagRes)]) 
  
  if(variance.mdl$model == "AR")   vola_pred  <- predict(svm_var, newdata = vola_new_dat)
  if(variance.mdl$model == "ARMA") vola_pred  <- predict(svm_var$model, newdata = vola_new_dat)
  
  
  #############################################################################
  # 3. Compute quantiles of scaled standardized residuals
  #############################################################################
  
  # Standardize and scale residuals
  if(variance.mdl$model == "AR")   u_sc <- eps[-1]/sqrt(fcast_vola)
  if(variance.mdl$model == "ARMA") u_sc <- eps[-(1:2)]/sqrt(fcast_vola)
  
  u_sc             <- scale(u_sc)
  
  # Compute quantiles of scaled standardized residuals
  q_upper          <- QKDE(q, c(0, max(u_sc)*1.5), data = u_sc, kernel = kernel)
  q_lower          <- -QKDE(q, c(0, max(-u_sc)*1.5), data = -u_sc, kernel = kernel)
  
  
  #############################################################################
  # 4. Collect results
  #############################################################################
  
  # Specify data frame for storing results
  results          <- data.frame(matrix(NA, nrow = 1, ncol = 4))
  names(results)   <- c("Mean", "Upper", "Lower", "Vola")
  
  # Save results in data frame
  results$Mean     <- mean_pred
  results$Upper    <- q_upper
  results$Lower    <- q_lower
  results$Vola     <- ifelse(vola_pred <= 0, NA, sqrt(vola_pred))
   
  return(results)
  
}



###############################################################################
# Function for variance estimation

svm_variance <- function(x, y, m, gamma, cost, epsilon){
  
  if(m == "AR"){

    model   <- svm(x = x, y = y, scale = TRUE,
                    type = "eps-regression", kernel = "radial",
                    gamma = gamma, cost = cost, epsilon = epsilon)
    
    result <- model

  }
  
  if(m == "ARMA"){
    
    rnn_v1   <- svm(x = x, y = y, scale = TRUE,
                    type = "eps-regression", kernel = "radial",
                    gamma = gamma, cost = cost, epsilon = epsilon)
    
    w           <- xts(x = rnn_v1$residuals, order.by = index(y))
    garch_input <- na.omit(cbind(y, x, lag(w)))
    
    model  <- svm(x = garch_input[,2:3], y = garch_input[,1], scale = TRUE, 
                   type = "eps-regression", kernel = "radial",
                   gamma = gamma, cost = cost, epsilon = epsilon)
    
    result <- list(model = model, lagRes = w)
    
  }
  
  
  
  return(result)
  
}



###############################################################################
# Function for plotting results and analysis

plotResults <- function(results, allData, test_n, q){
  
  
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
  
  head <- paste0(q*100, "% and ", (1-q)*100, "% VaR-Forecast")
  plot(results$True, type = "p", ylim = c(ylim_min, ylim_max), 
       ylab = "Return in Percent", main = head)
  points(results$True, col = "green")
  lines(xts(x = fcast_upper, order.by = index(results$True)), col = "red", lwd = 2)
  lines(xts(x = fcast_lower, order.by = index(results$True)), col = "red", lwd = 2)
  
  # plot(org, type = "p", col = "green", lwd = 1, ylim = c(ylim_min, ylim_max), ylab = "Value")
  # lines(fcast_upper, col = "red", lwd = 2)
  # lines(fcast_lower, col = "red", lwd = 2)
  errors <- data.frame(Index = 1:length(org), Real = org)
  out    <- errors$Real > fcast_upper | errors$Real < fcast_lower
  points(errors$Index[out], errors$Real[out], col = "blue", pch = 16)

  #print(prop_upper)
  #print(prop_lower)
  
  prop <- data.frame(prop_upper, prop_lower)
  
  results <- list(Data = results, 
                  Empirical_Coverage = prop)
  
  return(results)
  
}

###############################################################################
# VaR using kernel density estimation

KDE_VaR <- function(x, q){
  
  #############################################################################
  # 1. Estimate quantiles of data
  #############################################################################
  
  q_upper          <- QKDE(q, c(0, max(x)*1.5), data = x)
  q_lower          <- -QKDE(q, c(0, max(-x)*1.5), data = -x)
  
  
  #############################################################################
  # 2. Collect results
  #############################################################################
  
  # Specify data frame for storing results
  results          <- data.frame(matrix(NA, nrow = 1, ncol = 4))
  names(results)   <- c("Mean", "Upper", "Lower", "Vola")
  
  # Save results in data frame
  results$Mean     <- 0
  results$Upper    <- q_upper
  results$Lower    <- q_lower
  results$Vola     <- 1
  
  return(results)
  
}
