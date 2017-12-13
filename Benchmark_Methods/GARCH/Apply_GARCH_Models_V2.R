###############################################################################
# Description: The standard GARCH(1,1) and corresponding EGARCH and TGARCH
# models are coupled with a normal, t and skewed t distribution to 
# forecast Value-at-Risk. It is assumed that the return series has zero mean.
#
###############################################################################
# Usage: Set 'block_size', 'time_int' and 'probs' in the first part. In the 
# second part, the time series can be changed.
#
###############################################################################
# Output: The results are written as RDS-files to the 'Results'-folder.
#
###############################################################################
# Dependencies: 'GARCH_Models_Functions.R'
#
###############################################################################
# Author: Marius Lux 2017-03-26
#
###############################################################################

rm(list = ls())
graphics.off()

setwd(paste0(Sys.getenv("USERPROFILE"), "/Desktop/Master/Master_Thesis/Code_Abgabe"))

source("Benchmark_Methods/GARCH/GARCH_Models_Functions.R")

library(beepr)
library(quantmod)
library(rugarch)
library(R.utils)

# Subfolder of the 'Results' folder where results are saved
result_folder      <- "Tuning_2006to2011_FCast_2011to2016_MeanZero"

################################################################################
# Set parameters 
###############################################################################

probs              <- c(0.005, 0.01, 0.025, 0.05) # Quantiles
block_size         <- 251  # Size of blocks used for estimation
time_int           <- "2011-07-01/2016-06-30"  # Time interval for prediction


###############################################################################
# Get data
###############################################################################

# Load data and set time series to be modeled
all_data           <- readRDS("Data/Datasets.rds")
ts_names           <- c("EuroStoxx50", "S&P500", "Nikkei225")

###############################################################################
# Run analysis
###############################################################################

# Specify models for variance and distribution 
var_models              <- c("sGARCH", "eGARCH", "TGARCH")
dist_models             <- c("norm", "std", "sstd")

# Combine all possible models in a data frame
models_df               <- as.data.frame(expand.grid(var_models, dist_models, stringsAsFactors = FALSE))
colnames(models_df)     <- c("var_models", "dist_models")
rownames(models_df)     <- paste0("Model_", 1:{length(dist_models)*length(var_models)})

# Run analysis for the selected indices
for(index_id in 1:length(ts_names)){
  
  # Create data set
  data_set           <- all_data[[which(names(all_data) == ts_names[index_id])]]
  data_set           <- data_set[substr(time_int, 11, nchar(time_int))]  
  test_size          <- nrow(data_set[time_int])
  start              <- nrow(data_set) - test_size - block_size + 1
  sub_data           <- data_set[start:nrow(data_set), 1]
  
  loop_start         <- block_size
  loop_end           <- nrow(sub_data) - 1

  # Run Simulation for all possible models
  r                  <- lapply(1:nrow(models_df),
                               function(x) fit_garch_dopar(sub_data, probs, block_size,
                                                           models_df$var_models[x],
                                                           models_df$dist_models[x]))
                                                                
  # Save results for each model as rds file
  file_names <- paste0("Results_", models_df$var_models, "-", models_df$dist_models, "_", ts_names[index_id], "_VaR", ".rds")
  
  mapply(function(result_object, name) saveRDS(result_object, name),
         r,
         paste0("Results/",result_folder, "/", file_names))
  
}

# Make noise when code has finished
beep(8)