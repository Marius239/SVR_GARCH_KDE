###############################################################################
# Description: This script contains the main functions for modeling VaR via a 
# SVR-GARCH-KDE hybrid.
#
###############################################################################
# Author: Marius Lux 2017-03-26
#
###############################################################################

###############################################################################
# Parallized forecasting for a block of data points with the SVR-GARCH-KDE
# hybrid
###############################################################################

svm_kde_dopar <- function(data, q, block, test, mean_mdl, variance_mdl){
  
  if(nrow(data) < block + test) stop("Number of rows in 'data' is less than block + test")
  
  #Source functinos and load packages
  source(paste0(Sys.getenv("USERPROFILE"), "/Desktop/Master/Master_Thesis/Code_Abgabe/SVRGARCHKDE_Functions/Source_File/Source_SVRGARCHKDE.R"))
  library(doParallel)
  library(quantmod)
  
  # Construct data set
  allData    <- data
  test_n     <- test 
  block_size <- block
  loop_start <- nrow(allData) - test_n
  loop_end   <- nrow(allData) - 1
  
  # Set number of clusters
  cl <- makeCluster(30)
  registerDoParallel(cl)
  
  # Run parallelized procedure
  results <- foreach(i = loop_start:loop_end, .combine = "rbind") %dopar% {
    
    # Load functions and packages
    source(paste0(Sys.getenv("USERPROFILE"), "/Desktop/Master/Master_Thesis/Code_Abgabe/SVRGARCHKDE_Functions/Source_File/Source_SVRGARCHKDE.R"))
    library(quantmod)
    library(e1071)
    library(zoo)
    
    # Get variables for one data block
    x            <- allData[(i-block_size+1):i,-1]
    y            <- allData[(i-block_size+1):i,1]
    
    # Check whether only unconditional VaR shall be forecasted via KDE
    mean_mdl_test <- ifelse(is.null(mean_mdl), "No Value", mean_mdl)
    if(mean_mdl_test == "KDE" & variance_mdl == "KDE"){
      
      KDE_VaR(y, q, kernel = "gaussian")
      
    }else{
      
      # Train model and predict
      svm_kde(x = x, y = y, q = q, mean_mdl = mean_mdl, variance.mdl = variance_mdl, kernel = "gaussian")

    }


  }
  stopCluster(cl)
  
  #results$Vola_FCast        <- na.locf(results$Vola_FCast)
  results$Observation       <- data[(nrow(allData) - test_size + 1):nrow(allData),1]
  
  return(results)
}

###############################################################################
# Function to train and predict VaR 
###############################################################################

svm_kde <- function(x, y, q, mean_mdl, variance.mdl, kernel){
  
  #############################################################################
  # 1. Fit mean model
  #############################################################################
  
  # Check if mean model is NULL
  if(is.null(mean_mdl)){
    
    mean             <- xts(x = rep(0, nrow(y)), order.by = index(y))
    eps              <- y 
    mean_pred        <- 0
    
  }else{
    
    # Set parameters
    svm_mean_eps     <- mean_mdl$eps
    svm_mean_cost    <- mean_mdl$cost
    sigma            <- mean_mdl$sigma
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
    
  }
  
  
  
 
  #############################################################################
  # 2. Fit variance model
  #############################################################################
  
  # Set paramters
  svm_var_eps       <- quantile(scale(eps)^2, variance.mdl$eps_quantile)
  svm_var_cost      <- variance.mdl$cost
  sigma             <- variance.mdl$sigma
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
  fcast_vola[1]             <- ifelse(is.na(fcast_vola[1]), u[1,2], fcast_vola[1])
  
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
  foo_quant_pred   <- function(p, res_data){
    
    #lower_bound <- min(min(res_data)*1.5, -10)
    #upper_bound <- max(max(res_data)*1.5, -10)
    bound <- 8
    if(p >= 0.5) result <- QKDE(p, c(-bound, max(res_data)*5), data = res_data, kernel = kernel)
    if(p <  0.5) result <- QKDE(p, c(bound, min(res_data)*5), data = res_data, kernel = kernel)
    
    return(result)
    
  } 
  
  # Predict quantile of standardized residuals
  quant_pred       <- sapply(q, function(x) foo_quant_pred(x, res_data = u_sc))
  
  
  #############################################################################
  # 4. Collect results
  #############################################################################
  
  # Specify data frame for storing results
  results            <- as.data.frame(matrix(NA, nrow = 1, ncol = 3 + length(q)))
  names(results)     <- c("Observation", "Mean_FCast", "Vola_FCast", paste0("Quantile_", q*100, "%"))
  
  # Save results in data frame
  results$Mean_FCast          <- mean_pred
  results$Vola_FCast          <- ifelse(vola_pred <= 0, u[nrow(u),2], sqrt(vola_pred))
  results[1, 4:ncol(results)] <- mean_pred + results$Vola_FCast[1]*quant_pred

  return(results)
  
}



###############################################################################
# Function for variance estimation
###############################################################################

svm_variance <- function(x, y, m, gamma, cost, epsilon){
  
  # Check if ARCH(1) or GARCH(1,1) shall be estimated
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
# Function to fit mean model via SVR
###############################################################################

svm_kde_mean <- function(x, y, mean_mdl){
  
  #############################################################################
  # 1. Fit mean model
  #############################################################################
  
  # Set parameters
  svm_mean_eps     <- quantile(abs(y), mean_mdl$eps)
  svm_mean_cost    <- mean_mdl$cost
  sigma            <- mean_mdl$sigma
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
  
  return(mean_pred)
  
}

###############################################################################
# Mean model parallelized
###############################################################################

svm_kde_mean_dopar <- function(data, block, test, mean_mdl){
  

if(nrow(data) < block + test) stop("Number of rows in 'data' is less than block + test")

source(paste0(Sys.getenv("USERPROFILE"), "/Desktop/Master/Master_Thesis/Code_Abgabe/SVRGARCHKDE_Functions/Source_File/Source_SVRGARCHKDE.R"))
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

cl <- makeCluster(6)
registerDoParallel(cl)

results <- foreach(i = loop_start:loop_end, .combine = "rbind") %dopar% {
  
  source(paste0(Sys.getenv("USERPROFILE"), "/Desktop/Master/Master_Thesis/Code_Abgabe/SVRGARCHKDE_Functions/Source_File/Source_SVRGARCHKDE.R"))
  library(quantmod)
  library(e1071)
  library(zoo)
  
  x            <- allData[(i-block_size+1):i,-1]
  y            <- allData[(i-block_size+1):i,1]
  mean_mdl     <- mean_mdl
  
  svm_kde_mean(x = x, y = y, mean_mdl = mean_mdl)
  
  
}
stopCluster(cl)

real  <- data[(nrow(data) - test + 1):nrow(data),1]
pred  <- xts(results, order.by = index(real))

Predictions       <- cbind(real, pred) 
names(Predictions) <- c("Observation", "Prediction")
      
mse   <- sum((coredata(real) - coredata(pred))^2)/nrow(real)

results <- list(MSE = mse, Predictions = Predictions)

return(results)

}

###############################################################################
# Function for plotting results and analysis
###############################################################################

plotResults <- function(results, allData, test_n, q){
  
  
  results$True <- allData[(nrow(allData) - test_n + 1):nrow(allData),1]
  
  #Analyze Results
  
  results$FCast_Upper <- results$Mean + results$Upper*results$Vola
  results$FCast_Lower <- results$Mean + results$Lower*results$Vola
  
  prop_upper <- sum(results$FCast_Upper < results$True)/nrow(results) 
  prop_lower <- sum(results$FCast_Lower > results$True)/nrow(results) 
  
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
  
  errors <- data.frame(Index = 1:length(org), Real = org)
  out    <- errors$Real > fcast_upper | errors$Real < fcast_lower
  points(errors$Index[out], errors$Real[out], col = "blue", pch = 16)

  prop <- data.frame(prop_upper, prop_lower)
  
  results <- list(Data = results, 
                  Empirical_Coverage = prop)
  
  return(results)
  
}

###############################################################################
# VaR using kernel density estimation #########################################
###############################################################################

KDE_VaR <- function(x, q, kernel){
  
  #############################################################################
  # 1. Estimate quantiles of data
  #############################################################################
  
  foo_quant_pred   <- function(p, res_data){
    
    if(p >= 0.5) result <- QKDE(p, c(0, max(res_data)*1.5), data = res_data, kernel = kernel)
    if(p <  0.5) result <- QKDE(p, c(0, min(res_data)*1.5), data = res_data, kernel = kernel)
    
    return(result)
    
  } 
  
  quant_pred       <- sapply(q, function(y) foo_quant_pred(y, x))

  
  #############################################################################
  # 2. Collect results
  #############################################################################
  
  # Specify data frame for storing results
  results          <- as.data.frame(matrix(NA, nrow = 1, ncol = 3 + length(q)))
  names(results)   <- c("Observation", "Mean_FCast", "Vola_FCast", paste0("Quantile_", q*100, "%"))
  
  # Save results in data frame
  results[1, 4:ncol(results)] <- quant_pred
  
  return(results)
  
}
