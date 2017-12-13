###############################################################################
# Description: Function to compute VaR via Peaks-over-Threshold.
#
###############################################################################
# Author: Marius Lux 2017-03-26
#
###############################################################################

pot_VaR <- function(x, quantile_threshold, VaR){
  
  n       <- length(x)
  quant   <- quantile_threshold
  
  # Check wheter model can be estimated for threshold
  # If not, reduce threshold by 0.01
  pos_prob <- TRUE
  err      <- TRUE
  while(err){
  
    u   <-  quantile(x, quant)
    N_u <- sum(x > u) 
    
    err <-  inherits(try(gpdFit(x, u = u, type = "mle", information = "observed"), silent = TRUE), "try-error")
    
    if(err & pos_prob){
      quant <- quant - 0.01
      if(quant <= 0){
      pos_prob <- FALSE
      }
    }
    
    if(err & !pos_prob) quant <- quantile_threshold + 0.01
    
    
    if(!err) gpd <-  gpdFit(x, u = u, type = "mle", information = "observed") 
  }  
  
  # Extracht parameters
  gamma   <- attr(gpd,"fit")$par.ests[1]    #location parameter
  beta    <- attr(gpd,"fit")$par.ests[2] 
  
  # Compute VaR
  q       <- u + (beta/gamma)*({(n*(1-VaR))/N_u}^(-gamma) - 1)
  
  return(q)
}

