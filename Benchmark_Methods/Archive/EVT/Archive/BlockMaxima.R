###############################################################################
# Description: A GARCH(1,1) model with a standard normal distribution for the
# innovations is applied as benchmark method.
#
###############################################################################
# Usage: Set 'block_size','test_size' and 'probs' in the first part. In third 
# part, set the variance and distribution models.
#
###############################################################################
# Problems: The 'rugarch' optimization procedure may not converge. 
#
###############################################################################

rm(list = ls())
graphics.off()

setwd("C:/Users/Marius/Desktop/Simulation")

source("SVRGARCHKDE_Functions/Backtesting_Functions.R")

library(fExtremes)

###############################################################################
# Set parameters 
###############################################################################

probs              <- c(0.005, 0.01, 0.025, 0.05, 0.1, 0.9, 0.95, 0.975, 0.99, 0.995)
block_size         <- 500  # Size of blocks used for estimation
test_size          <- 250*1  # Size of test set Time 2008-2010 when times 4


###############################################################################
# Get data
###############################################################################

all_data           <- createData()$data
start              <- nrow(all_data) - test_size - block_size + 1
sub_data           <- all_data[start:nrow(all_data)]


###############################################################################
# Run analysis
###############################################################################

results            <- as.data.frame(matrix(NA, nrow = test_size, ncol = 3 + length(probs)))
names(results)     <- c("Observation", "Mean_FCast", "Vola_FCast", paste0("Quantile_", probs*100, "%"))

loop_start     <- block_size
loop_end       <- nrow(sub_data) - 1
j              <- 1

for(i in loop_start:loop_end){
  
  print(j)
 
  x           <- sub_data[(i-block_size+1):i,1]
  
  # Determine the Block Maxima data
  t    = length(x)
  n    = 5
  k    = t/n
  z    = matrix()
  
  for(count in 1:k){
    r    = x[((count-1)*n+1):(count*n)]
    z[count] = max(r)
  }
  w     <- sort(z)
  gev   <- gevFit(w, type="mle")   
  xi    <- attr(gev, "fit")$par.ests[1] # shape parameter
  mu    <- attr(gev, "fit")$par.ests[2] # location parameter
  sigma <- attr(gev, "fit")$par.ests[3] # scale parameter
  
  quant <- 0.99
  
  q_upper <-  mu - (sigma/xi)*{ 1-(-log(quant^n))^(-xi) }
  
  
  t    = length(x)
  
  k    = t/n
  z    = matrix()
  
  for(count in 1:k){
    r    = -x[((count-1)*n+1):(count*n)]
    z[count] = max(r)
  }
  w     <- sort(z)
  gev   <- gevFit(w,type="mle")   
  xi    <- attr(gev,"fit")$par.ests[1]     #shape parameter
  mu    <- attr(gev,"fit")$par.ests[2]    #location parameter
  sigma <- attr(gev,"fit")$par.ests[3] #scale parameter
  
  q_lower <- -{mu - (sigma/xi)*{ 1-(-log(quant^n))^(-xi) }}
  
  # Gather results
  results$Upper[j] <- q_upper
  results$Lower[j] <- q_lower
  
  j <- j + 1 
}

results_old <- results[,14:15]
View(results_old)
