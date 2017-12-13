###############################################################################
# Description: Forecasts for the test set using the SVR-GARCH-KDE hybrid are
# produced. The parameter choice is based on the tuning results. The optimal
# parameter are loaded via sourcing the file 'Create_Best_List.R'.
#
###############################################################################
# Usage: Set 'block_size', 'time_int', 'probs' and the time series. 
#
###############################################################################
# Output: The results are written as RDS-files to the 'Results' folder of the 
# in the corresponding time series in the 'Tuning' folder.
#
###############################################################################
# Dependencies: Functions that are loaded by 'Source_SVRGARCHKDE.R' and 
# 'Create_Best_List.R'
#
###############################################################################
# Author: Marius Lux 2017-03-26
#
###############################################################################

rm(list = ls())
graphics.off()

setwd(paste0(Sys.getenv("USERPROFILE"), "/Desktop/Master/Master_Thesis/Code_Abgabe"))

# By sourcing the file, the list with the parameters for the best models from
# model tuning will be loaded
source("BestTuningModels_and_TexTables/Create_Best_List.R")

source("SVRGARCHKDE_Functions/Source_File/Source_SVRGARCHKDE.R")

###############################################################################
# Set parameters 
###############################################################################

probs              <- c(0.005, 0.01, 0.025, 0.05)
block_size         <- 251  # Size of blocks used for estimation
time_int           <- "2011-07-01/2016-06-30"  # Time interval for prediction

###############################################################################
# Get data
###############################################################################

all_data           <- readRDS("Data/Datasets.rds")
ts_names           <- c("EuroStoxx50", "S&P500", "Nikkei225")

###############################################################################
# Run analysis
###############################################################################


for(index_id in 1:length(ts_names)){
 
 
 start_time <- Sys.time()
  
  # Remove irrelvant data
  data_set           <- all_data[[which(names(all_data) == ts_names[index_id])]]
  test_size          <- nrow(data_set[time_int])
  start              <- nrow(data_set) - test_size - block_size + 1
  sub_data           <- data_set[start:nrow(data_set)]
  
  dataset_idx        <- which(ts_names[index_id] == names(model_paras))
  
  mean_mdl           <- NULL
  VaR_df             <- model_paras[[dataset_idx]]$VaR  # Define variance model
  
  # Initilize list for saving results
  final_result       <- list()
  
  for(model_alpha in 1:4){
    
    print(model_alpha) 
    
    VaR_mdl_sub <- VaR_df[VaR_df$alpha == probs[model_alpha]*100,]
    
    psi     <- VaR_mdl_sub[, "psi"]
    costVal <- VaR_mdl_sub[, "c"]
    sigmaVal <- VaR_mdl_sub[, "sigma"]
    
    variance_mdl       <- list(model = "ARMA"
                               , eps_quantile = psi
                               , cost = costVal
                               , sigma = sigmaVal)  # Variance model
    
    
    final_result[[model_alpha]]    <- svm_kde_dopar(sub_data, probs[model_alpha], block_size, test_size, 
                                               mean_mdl = mean_mdl, variance_mdl = variance_mdl)
    
  }
  

  final_result_df <- cbind(final_result[[1]]
                           , final_result[[2]][,4, drop = FALSE]
                           , final_result[[3]][,4, drop = FALSE]
                           , final_result[[4]][,4, drop = FALSE])
  end_time   <- Sys.time()
  print(end_time - start_time)
  
  # Save results
  file_names <- paste0("Results/Tuning_2006to2011_FCast_2011to2016_MeanZero/Results_", "SVM-KDE_", ts_names[index_id], "_VaR", ".rds")
  saveRDS(final_result_df, file_names)

}