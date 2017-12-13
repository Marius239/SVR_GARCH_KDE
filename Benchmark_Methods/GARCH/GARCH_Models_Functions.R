################################################################################
# Description: This file contains the functions to estimate the VaR models
# which are specified in 'Apply_GARCH_Models_V2.R. Two functions are in this
# file. One estimates the models in a for-loop. The other one is parallelized.
#
################################################################################
# Author: Marius Lux 2017-03-26
#
################################################################################

################################################################################
# Estimate GARCH model with loop ###############################################
################################################################################

fit_garch_loop <- function(data, prob, block_size, var_model, dist_model){
  
  # Initilize data frame
  results            <- as.data.frame(matrix(NA, nrow = test_size, ncol = 3 + length(prob)))
  names(results)     <- c("Observation", "Mean_FCast", "Vola_FCast", paste0("Quantile_", prob*100, "%"))
  
  ###############################################################################
  # Run analysis
  ###############################################################################
  
  start <- Sys.time()
  
  j <- 1
  loop_start       <- block_size
  loop_end         <- nrow(data) - 1
  
  for(i in loop_start:loop_end){
    
    # print(paste0(j, "/", loop_end - block_size + 1))  # Print to know iteration step
    
    # Specification 
    if(var_model == "TGARCH"){
      
      spec <- ugarchspec(variance.model     = list(model = "fGARCH", garchOrder = c(1, 1), submodel = var_model),               
                         #mean.model         = list(armaOrder = c(1, 0)),
                         mean.model         = list(armaOrder = c(0, 0), include.mean = FALSE),
                         distribution.model = dist_model)
      
    }else{
      
      spec <- ugarchspec(variance.model     = list(model = var_model, garchOrder = c(1, 1)),               
                         #mean.model         = list(armaOrder = c(1, 0)),
                         mean.model         = list(armaOrder = c(0, 0), include.mean = FALSE),
                         distribution.model = dist_model)
      
    }
    
      
    # Subset data for analysis
    series      <- data[(i-block_size+1):i, 1]
    
    # Estimation and 1-day-ahead forecast
    garch       <- ugarchfit(spec = spec, data = series)
    garch_fcast <- ugarchforecast(garch, n.ahead = 1)
    
    mean_pred   <- as.numeric(fitted(garch_fcast))
    vola_pred   <- as.numeric(sigma(garch_fcast))
    quant_pred  <- sapply(prob, function(x) quantile(garch_fcast,x))
    
    
    # Gather results
    results$Mean_FCast[j]       <- mean_pred
    results$Vola_FCast[j]       <- ifelse(vola_pred <= 0, results$Vola[(j-1)], vola_pred)
    results[j, 4:ncol(results)] <- quant_pred 
    
    j <- j + 1 
  }
  
  results$Observation <- data[(nrow(data) - test_size + 1):nrow(data),1]
  
  end <- Sys.time()
  print(end - start)
  
  return(results)
  
}


################################################################################
# Estimate GARCH model parallelized ############################################
################################################################################

fit_garch_dopar <- function(data, prob, block_size, var_model, dist_model){
  
  library(doParallel)
  
  # Helper function
  helper_foo <- function(data, prob, block_size, var_model, dist_model){
    
    results            <- as.data.frame(matrix(NA, nrow = 1, ncol = 3 + length(prob)))
    names(results)     <- c("Observation", "Mean_FCast", "Vola_FCast", paste0("Quantile_", prob*100, "%"))
    
    # Specification 
    if(var_model == "TGARCH"){
      
      spec <- ugarchspec(variance.model     = list(model = "fGARCH", garchOrder = c(1, 1), submodel = var_model),               
                         #mean.model         = list(armaOrder = c(1, 0)),
                         mean.model         = list(armaOrder = c(0, 0), include.mean = FALSE),
                         distribution.model = dist_model)
      
    }else{
      
      spec <- ugarchspec(variance.model     = list(model = var_model, garchOrder = c(1, 1)),               
                         #mean.model         = list(armaOrder = c(1, 0)),
                         mean.model         = list(armaOrder = c(0, 0), include.mean = FALSE),
                         distribution.model = dist_model)
      
    }
    
    # Subset data for analysis
    series      <- data[(i-block_size+1):i, 1]
    
    # Estimation and 1-day-ahead forecast
    garch       <- NULL
    garch       <- evalWithTimeout(ugarchfit(spec = spec, data = series, solver = "hybrid")
                                   , timeout = 1200
                                   , onTimeout="silent")
    
    if(is.null(garch)){
      
      warning("GARCH solver 'hybrid' did not converge after 20 minuntes. Changes solver to 'nlminb'")
      garch <- ugarchfit(spec = spec, data = series, solver = "nlminb")
      
    }
    
  
    garch_fcast <- ugarchforecast(garch, n.ahead = 1)
    
    mean_pred   <- as.numeric(fitted(garch_fcast))
    vola_pred   <- as.numeric(sigma(garch_fcast))
    quant_pred  <- sapply(prob, function(x) quantile(garch_fcast,x))
    
    
    # Gather results
    results$Mean_FCast          <- mean_pred
    results$Vola_FCast          <- vola_pred #ifelse(vola_pred <= 0, results$Vola[(j-1)], vola_pred)
    results[1, 4:ncol(results)] <- quant_pred 
    
    return(results)
    
  }
  
  ###############################################################################
  # Run analysis
  ###############################################################################
  
  start <- Sys.time()
  
  cl <- makeCluster(35)
  registerDoParallel(cl)
  
  loop_start       <- block_size
  loop_end         <- nrow(data) - 1
  
  results_dopar <- foreach(i = loop_start:loop_end, .combine = "rbind") %dopar% {
    
    library(R.utils)
    library(rugarch)
    
    helper_foo(data, prob, block_size, var_model, dist_model)
    
    # Uncomment and set path to find converging models and thus conclude non-converging
    # write.csv2("Test for no convergence",
    # file = paste0("C:/Users/luxmariu.hub/Desktop/Master/Master_Thesis/Simulation/Track_Parallel/GARCH_NoConverge/Track_", i, ".csv"))
    
  }
  
  stopCluster(cl)
  
  results_dopar$Observation <- data[(nrow(data) - test_size + 1):nrow(data),1]
  
  end <- Sys.time()
  print(end - start)
  
  return(results_dopar)
  
}