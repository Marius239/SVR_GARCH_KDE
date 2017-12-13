###############################################################################
# Description: 
###############################################################################
# Usage: Set 'block_size','test_size' and 'probs' in the first part. In third 
#
###############################################################################
# Problems: 
#
###############################################################################

rm(list = ls())
graphics.off()

setwd("C:/Users/Marius/Desktop/Simulation")

source("SVRGARCHKDE_Functions/Backtesting_Functions.R")
source("Benchmark_Methods/EVT/BlockMaxima_Functions.R")
source("Simulation/Backtesting_Functions.R")

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

loop_start         <- block_size
loop_end           <- nrow(sub_data) - 1
j                  <- 1

for(i in loop_start:loop_end){
  
  print(j)
  
  x                           <- sub_data[(i-block_size+1):i,1]
  
  results[j, 4:ncol(results)] <- sapply(probs, function(z) pot_VaR(x, z))
  
  j <- j + 1 
}

results$Observation <- sub_data[(nrow(sub_data) - test_size + 1):nrow(sub_data),1]

###############################################################################
# Save results as rds file

file_names <- paste0("Results/Results_", "POT", "_VaR", ".rds")

saveRDS(results, file_names)
