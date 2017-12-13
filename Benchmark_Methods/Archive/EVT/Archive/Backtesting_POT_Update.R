#Backtesting
rm(list = ls())
#graphics.off()

source("Simulation/Backtesting_Functions.R")

library(quantmod)
library(fExtremes)

data        <- createData()

allData    <- data$data
test_n     <- 250*3  #Time 2008-2010
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
  
  features_tr <- allData[(i-block_size+1):i,-1]
  target_tr   <- allData[(i-block_size+1):i,1]
  x <- target_tr
  #Delta-Normal assumes 0 means
  mean <- 0
  eps  <- target_tr
  u_sc <- scale(eps)
  
  mean_pred <- 0
  
  #Fit Variance
  quant   <- 0.7
  n       <- length(x)
  u_u     <- quantile(x, quant)
  N_u     <- sum(x > u_u) 
  gpd_u   <- gpdFit(x, u = u_u, type = "mle", information = "observed")
  gamma_u <- attr(gpd_u,"fit")$par.ests[1]    #location parameter
  beta_u  <- attr(gpd_u,"fit")$par.ests[2] 
  q_upper <- u_u + (beta_u/gamma_u)*({(n*(1-0.99))/N_u}^(-gamma_u) - 1)
  
  u_l     <- quantile(-x, quant)
  N_l     <- sum(-x > u_u) 
  gpd_l   <- gpdFit(-x, u = u_l, type = "mle", information = "observed")
  gamma_l      <- attr(gpd_l,"fit")$par.ests[1]    #location parameter
  beta_l    <- attr(gpd_l,"fit")$par.ests[2] 
  q_lower <- -(u_l + (beta_l/gamma_l)*({(n*(1-0.99))/N_l}^(-gamma_l) - 1))
  
  
  #Gather results
  results$Mean[j]  <- mean_pred
  results$Upper[j] <- q_upper
  results$Lower[j] <- q_lower
  results$Vola[j]  <- 1
  
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

lines(fcast_upper, col = "brown", lwd = 2)
lines(fcast_lower, col = "brown", lwd = 2)
errors <- data.frame(Index = 1:length(org), Real = org)
out    <- errors$Real > fcast_upper | errors$Real < fcast_lower
points(errors$Index[out], errors$Real[out], col = "blue", pch = 16)

prop_upper
prop_lower

end <- Sys.time()
end - start

saveRDS(results, file = "Results_POT.rds")