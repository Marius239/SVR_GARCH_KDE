###############################################################################
# Description: 
###############################################################################
# Usage: Set 'block_size' and 'probs' in the first part. In third 
#
###############################################################################
# Problems: 
#
###############################################################################

rm(list = ls())
graphics.off()

setwd("C:/Users/Marius/Desktop/Simulation")

source("Benchmark_Methods/EVT/EVT_Functions.R")

library(fExtremes)

###############################################################################
# Set parameters 
###############################################################################

probs              <- c(0.005, 0.01, 0.025, 0.05, 0.1, 0.9, 0.95, 0.975, 0.99, 0.995)
block_size         <- 500  # Size of blocks used for estimation
time_int           <- "2015-07-01/2016-06-30"  # Time interval for prediction


###############################################################################
# Get data
###############################################################################

all_data           <- readRDS("Data/Datasets.rds")
ts_names           <- names(all_data)


###############################################################################
# Run analysis
###############################################################################

for(index_id in 1:length(ts_names)){
  
  data_set           <- all_data[[index_id]]
  test_size          <- nrow(data_set[time_int])
  start              <- nrow(data_set) - test_size - block_size + 1
  sub_data           <- data_set[start:nrow(data_set)]
  
  results            <- as.data.frame(matrix(NA, nrow = test_size, ncol = 3 + length(probs)))
  names(results)     <- c("Observation", "Mean_FCast", "Vola_FCast", paste0("Quantile_", probs*100, "%"))
  
  loop_start         <- block_size
  loop_end           <- nrow(sub_data) - 1
  j                  <- 1
  
  for(i in loop_start:loop_end){
    
    print(paste0("Time Series: ", index_id, "/", length(ts_names),  
                " Iteration: ", j, "/", length(loop_start:loop_end)))
    
    x                           <- sub_data[(i-block_size+1):i,1]
    results[j, 4:ncol(results)] <- sapply(probs, function(z) block_maxima_VaR(x, z))
    
    j <- j + 1 
  }
  
  results$Observation <- sub_data[(nrow(sub_data) - test_size + 1):nrow(sub_data),1]

  ###############################################################################
  # Save results as rds file
  
  file_names <- paste0("Results/Results_", "BlockMaxima_", ts_names[index_id], "_VaR", ".rds")

  saveRDS(results, file_names)
  
}