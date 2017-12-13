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

source("SVRGARCHKDE_Functions/Source_File/Source_SVRGARCHKDE.R")
source("Benchmark_Methods/GARCH/GARCH_Models_Functions.R")

library(beepr)
library(quantmod)
library(rugarch)


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

# Specify models for variance and distribution 
var_models              <- c("sGARCH", "eGARCH")
dist_models             <- c("norm", "std", "sstd")

# Combine all possible models in a data frame
models_df               <- as.data.frame(expand.grid(var_models, dist_models, stringsAsFactors = FALSE))
colnames(models_df)     <- c("var_models", "dist_models")
rownames(models_df)     <- paste0("Model_", 1:{length(dist_models)*length(var_models)})

# Run Simulation for all possible models
r                       <- lapply(1:nrow(models_df),
                                  function(x) fit_garch_dopar(sub_data, probs, block_size,
                                                              models_df$var_models[x],
                                                              models_df$dist_models[x]))
                                                              
# Uncomment for checking the functions
# x <- 1
# r1 <- fit_garch_loop(sub_data, probs, block_size, 
#                      models_df$var_models[x], 
#                      models_df$dist_models[x])

# r2 <- fit_garch_dopar(sub_data, probs, block_size, 
#                     models_df$var_models[x], 
#                     models_df$dist_models[x])


###############################################################################
# Save results for each model as rds file
file_names <- paste0("Results_", models_df$var_models, "-", models_df$dist_models, "_VaR", ".rds")

mapply(function(result_object, name) saveRDS(result_object, name),
       r,
       paste0("Results/", file_names))