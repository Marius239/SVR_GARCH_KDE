rm(list = ls())

setwd("C:/Users/Marius/Desktop/Simulation")

library(beepr)

source("SVRGARCHKDE_Functions/Source_File/Source_SVRGARCHKDE.R")

# Get data
data       <- createData()$data
#load("NYSE.RData")
#data <- data$data

# Set parameterss
q                  <- 0.95  # Quantile
test_size          <- 250*10  # Size of test set Time 2008-2010 when times 4
block_size         <- 500  # Size of blocks used for estimation

start              <- nrow(data) - block_size - test_size + 1
fit_data           <- data[start:nrow(data),]

# end_train   <- start - 1
# start_train <- end_train - (test_size + block_size) + 1
# train_data  <- data[start_train:end_train,]

start_time <- Sys.time()

results    <- svm_kde_dopar(data = fit_data,
                            q = q,
                            block = block_size,
                            test = test_size,
                            mean_mdl = "KDE",
                            variance_mdl = "KDE")



end_time   <- Sys.time()
end_time - start_time

results$Empirical_Coverage

kde_var_data_95 <- results$Data

beep(8)

saveRDS(kde_var_data_95, file = "Results/Results_KDE_VaR_95.RDS")