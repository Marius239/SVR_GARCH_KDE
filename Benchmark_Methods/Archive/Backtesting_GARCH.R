#Backtesting
rm(list = ls())
#graphics.off()

source("Simulation/Backtesting_Functions.R")
source("Simulation/Estimation_Functions.R")

library(quantmod)
library(rugarch)


data        <- createData()
allData     <- data$original

test_n     <- 750
block_size <- 250/2
loop_start <- nrow(allData) - test_n
loop_end   <- nrow(allData) - 1

results <- data.frame(matrix(NA, nrow = test_n, ncol = 4))
names(results) <- c("Mean", "Upper", "Lower", "Vola")

start <- Sys.time()

j <- 1
i <- loop_start
for(i in loop_start:loop_end){
  
  print(j)
  
  spec <- ugarchspec(variance.model = list(model = "sGARCH", 
                                          garchOrder = c(1, 1)),               
                    mean.model     = list(armaOrder = c(1, 0)),
                    distribution.model = "norm")
  
  
  garch       <- ugarchfit(spec = spec, data = allData[(i-block_size+1):i])
  garch_fcast <- ugarchforecast(garch, n.ahead = 1)
  
  u_sc      <- scale(residuals(garch))
  mean_pred <- as.numeric(fitted(garch_fcast))
  vola_pred <- as.numeric(sigma(garch_fcast))
  
  # Fit quantile with peaks over threshold
  q_upper <- pot_VaR(u_sc, 0.7, 0.99)
  q_lower <- -pot_VaR(-u_sc, 0.7, 0.99)
  
  # q_upper   <- quantile(u_sc, 0.99)
  # q_lower   <- quantile(u_sc, 0.01)
  
  #Gather results
  results$Mean[j]  <- mean_pred
  results$Upper[j] <- q_upper
  results$Lower[j] <- q_lower
  results$Vola[j]  <- ifelse(vola_pred <= 0, results$Vola[(j-1)], vola_pred)
  
  j <- j + 1 
}


results$True <- allData[(nrow(allData) - test_n + 1):nrow(allData),1]

#Analyze Results

results$FCast_Upper <- results$Mean + results$Upper*results$Vola
results$FCast_Lower <- results$Mean + results$Lower*results$Vola

prop_upper <- sum(results$FCast_Upper < results$True)/nrow(results)  #
prop_lower <- sum(results$FCast_Lower > results$True)/nrow(results)  #Downside risk: if greater 5% -> BAD

prop_upper
prop_lower

#Plot in-sample results
org         <- as.numeric(results$True)
fcast_upper <- results$FCast_Upper
fcast_lower <- results$FCast_Lower

ylim_max <- max(abs(c(org, fcast_upper, fcast_lower)))
ylim_min <- -ylim_max

plot(org, type = "p", col = "green", lwd = 1, ylim = c(ylim_min, ylim_max))

lines(fcast_upper, col = "red", lwd = 2)
lines(fcast_lower, col = "red", lwd = 2)
errors <- data.frame(Index = 1:length(org), Real = org)
out    <- errors$Real > fcast_upper | errors$Real < fcast_lower
points(errors$Index[out], errors$Real[out], col = "black", pch = 16)

end <- Sys.time()
end - start