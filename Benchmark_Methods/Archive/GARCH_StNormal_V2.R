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
probs              <- c(0.005, 0.01, 0.025, 0.05, 0.1, 0.9, 0.95, 0.975, 0.99, 0.995)
block_size         <- 500  # Size of blocks used for estimation
test_size          <- 250*1  # Size of test set Time 2008-2010 when times 4

# Get data
all_data           <- createData()$data
start              <- nrow(all_data) - test_size - block_size + 1
sub_data           <- all_data[start:nrow(all_data)]



###############################################################################
# Run analysis
###############################################################################

dist_models             <- c("norm", "std")#, "sstd")
var_models              <- c("sGARCH", "eGARCH")

models_df               <- as.data.frame(expand.grid(var_models, dist_models, stringsAsFactors = FALSE))
# row.names(models_df)    <- c("dist_models", "var_models")
# colnames(models_df)     <- paste0("Model_", 1:{length(dist_models)*length(var_models)})


#t <- mapply(function(x, y) fit_garch(sub_data, probs, block_size, x, y), models_df$Var1,models_df$Var2)
#library(parallel)

# t <- mcmapply(function(x, y) fit_garch(sub_data, probs, block_size, x, y), models_df$Var1,models_df$Var2,
#               mc.cores = 4)

r <- lapply(1:nrow(models_df),
            function(x) fit_garch(sub_data, probs, block_size, models_df$Var1[x], models_df$Var2[x]))
         



# results <- fit_garch(sub_data, probs, block_size, "sGARCH", "norm")