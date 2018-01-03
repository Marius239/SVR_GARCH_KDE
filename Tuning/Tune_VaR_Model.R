###############################################################################
# Description: SVR-GARCH-KDE hybrid for a specified grid will be trained. 
# The results will be evaluated in the the script 'Create_Best_List.R' in terms
# of the corresponding LR test's p-value for conditional coverage.
#
###############################################################################
# Usage: Set 'block_size', 'time_int', 'probs' and the time series. The grid
# can also be adjusted.
#
###############################################################################
# Output: The results are written as RDS-files to the 'Results' folder of the 
# in the corresponding time series in the 'Tuning' folder.
#
###############################################################################
# Dependencies: Functions that are loaded by 'Source_SVRGARCHKDE.R'
#
###############################################################################
# Author: Marius Lux 2017-03-26
#
###############################################################################

rm(list = ls())

setwd(paste0(Sys.getenv("USERPROFILE"), "/Desktop/Master/Master_Thesis/Code_Abgabe"))

# Load all functions and packages
# Make sure doParallel, e1071, quantmod and zoo are installed and adjust the 
# paths in 'SVM_KDE_Functions_V6.R' in folder 'SVRGARCHKDE_Functions'
source("SVRGARCHKDE_Functions/Source_File/Source_SVRGARCHKDE.R")

# Specify subfolder of 'Tuning' where results will be saved
project_folder     <- "From2006to2011_ZeroMean"

# Specify time series
series_name        <- "Nikkei225"


###############################################################################
# Get data
###############################################################################


all_data           <- readRDS("Data/Datasets.rds")
ts_names           <- names(all_data)
ts                 <- all_data[[which(ts_names == series_name)]]


###############################################################################
# Set parameters 
###############################################################################

time_int           <- "2006-07-01/2011-06-30"  # Time interval for prediction
ts                 <- ts[substr(time_int, 11, nchar(time_int))]  # Drop to0 new observations
test_size          <- nrow(ts[time_int])  # Size of test set
block_size         <- 251  # Size of blocks used for estimation
start              <- nrow(ts) - test_size - block_size + 1  # Find index for start
ts_sub             <- ts[start:nrow(ts)]  # Remove irrelevant data
probs              <- c(0.01, 0.025, 0.05)  # Quantiles
mean_mdl           <- NULL  # Assume zero mean


###############################################################################
# Defining the grid
###############################################################################

cost             <- 10^(-4:4)
psi              <- seq(0, 0.9, by = 0.1)
gamma            <- cost

grid_df          <- expand.grid(cost, psi, gamma)
names(grid_df)   <- c("cost", "psi", "gamma")


#####################################################################################
# Tuning with for loop 
#####################################################################################

# start <- Sys.time()
# mse <- rep(0, nrow(grid_df))
# for(i in 1:nrow(grid_df)){
#   
#   start_iter <- Sys.time()
#   
#   print(paste0(i, "/", nrow(grid_df)))
#   
#   mean_mdl    <- list(eps = grid_df$epsilon[i], cost = grid_df$cost[i], sigma = grid_df$gamma[i])
#   result_list <- svm_kde_mean_dopar(ts_sub, block_size, test_size, mean_mdl)
#   mse[i]      <- result_list$MSE
#   
#   end_iter <- Sys.time()
#   print(end_iter - start_iter)
# }
# end <- Sys.time()
# end - start

#####################################################################################
# Parallelized tuning
####################################################################################

cl            <- makeCluster(8)
registerDoParallel(cl)

start_proc    <- Sys.time()

results <- foreach(i = 1:nrow(grid_df)) %dopar% {
  
  start_task <- Sys.time()
  
  # Define variance model
  variance_mdl <- list(model = "ARMA", eps_quantile = grid_df$psi[i],
                       cost = grid_df$cost[i], sigma = grid_df$gamma[i])  
  
  res          <- svm_kde_dopar(ts_sub, probs, block_size, test_size,
                                variance_mdl = variance_mdl, mean_mdl = NULL)
  
  # Define data frame for tracking
  end_task <- Sys.time()
  duration <- round(as.numeric(difftime(end_task,start_task, units = "secs")), 2)
  total    <- round(as.numeric(difftime(end_task,start_proc, units = "secs")), 2)
  
  df <- data.frame(Task = i,
                   Start = start_proc,
                   Start_Task = start_task,
                   End_Task = end_task,
                   Dur_Task = duration,
                   Dur_Total = total)

  
  write.csv2(df, file = paste0("Track_Parallel/Task_",i, "_of_", nrow(grid_df),".csv"), row.names = FALSE)
  saveRDS(list(res, grid_df[i,]),
          file = paste0("Tuning/Results/RDS_", series_name, "/", project_folder, "/VaR/", series_name, "_VaR_", i, ".rds"))
  
  # Output of function
  res
  
}

stopCluster(cl)
end_proc <- Sys.time()
end_proc - start_proc

# In case some specifications ended with an error print them
files_all       <- 1:nrow(grid_df)

# In case the one of the below specified does not work, uncomment the other
files_completed <- as.numeric(substr(gsub("[^0-9]", "", list.files(paste0("Tuning/Results/RDS_", series_name, "/", project_folder, "/VaR/"))), 3,nchar(gsub("[^0-9]", "", list.files(paste0("Tuning/Results/RDS_", series_name, "/", project_folder, "/VaR/"))))))
#files_completed <- as.numeric(gsub("[^0-9]", "", list.files(paste0("Tuning/Results/RDS_", series_name, "/", project_folder, "/VaR/"))))

files_failed    <- files_all[!(files_all %in% files_completed)]
grid_df[files_failed,]

