rm(list = ls())
graphics.off()

setwd("C:/Users/Marius/Desktop/Simulation")

source("SVRGARCHKDE_Functions/Source_File/Source_SVRGARCHKDE.R")


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
  
  start_time         <- Sys.time()
  
  data_set           <- all_data[[index_id]]
  test_size          <- nrow(data_set[time_int])
  start              <- nrow(data_set) - test_size - block_size + 1
  sub_data           <- data_set[start:nrow(data_set)]
  
  results            <- svm_kde_dopar(sub_data, probs, block_size, test_size,
                                      mean_mdl = "KDE", variance_mdl = "KDE")
  
  end_time           <- Sys.time()
  end_time - start_time
  
  file_names         <- paste0("Results/Results_", "KDE_", ts_names[index_id], "_VaR", ".rds")
  saveRDS(results, file_names)
  
}