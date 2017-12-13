###############################################################################
# Description: A GARCH(1,1) model with a standard normal distribution for the
# innovations is applied as benchmark method.
#
###############################################################################

rm(list = ls())
graphics.off()

setwd("C:/Users/Marius/Desktop/Simulation")

source("SVRGARCHKDE_Functions/Source_File/Source_SVRGARCHKDE.R")
source("Benchmark_Methods/GARCH_Models_Functions.R")

library(beepr)
library(quantmod)
library(rugarch)


###############################################################################
# Set data
###############################################################################

# Set parameterss
quant              <- 0.95  # Upper quantile, i.e. q > 0.5
block_size         <- 500  # Size of blocks used for estimation
test_size          <- 250*10  # Size of test set Time 2008-2010 when times 4

# Get data
all_data           <- createData()$data
start              <- nrow(all_data) - test_size - block_size + 1
sub_data           <- all_data[start:nrow(all_data)]



###############################################################################
# Run analysis
###############################################################################


results <- fit_garch(sub_data, quant, block_size, "sGARCH", "norm")






start <- Sys.time()

j <- 1
i <- loop_start

for(i in loop_start:loop_end){
  
  print(paste0(j, "/", length(loop_start:loop_end)))  # Print to know iteration step
  
  # Specification 
  spec <- ugarchspec(variance.model     = list(model = "sGARCH", garchOrder = c(1, 1)),               
                     mean.model         = list(armaOrder = c(1, 0)),
                     distribution.model = "norm")
  
  # Subset data for analysis
  series      <- data[(i-block_size+1):i, 1]
  
  # Estimation and 1-day-ahead forecast
  garch       <- ugarchfit(spec = spec, data = series)
  garch_fcast <- ugarchforecast(garch, n.ahead = 1)
  
  mean_pred <- as.numeric(fitted(garch_fcast))
  vola_pred <- as.numeric(sigma(garch_fcast))
  q_upper   <- quantile(garch_fcast, q)
  q_lower   <- quantile(garch_fcast, 1-q)
  
  
  # Gather results
  results$Mean[j]  <- mean_pred
  results$Upper[j] <- q_upper
  results$Lower[j] <- q_lower
  results$Vola[j]  <- ifelse(vola_pred <= 0, results$Vola[(j-1)], vola_pred)
  
  j <- j + 1 
}


results$True <- data[(nrow(data) - test_size + 1):nrow(data),1]

#plotResults(results, data, test_n, q)

end <- Sys.time()
end - start