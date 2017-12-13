###############################################################################
# Description: The data for the analyses is loaded from Yahoo finance via the
# quantmod package. Log returns are computed and the data will be saved in 
# folder 'Data' as RDS-file.
#
###############################################################################
# Usage: Specify the names of the time series and the corresponding Yahoo
# finance symbol. 
#
###############################################################################
# Author: Marius Lux 2016-09-13
#
###############################################################################


rm(list = ls())
graphics.off()

setwd(paste0(Sys.getenv("USERPROFILE"), "/Desktop/Master/Master_Thesis/Code_Abgabe"))

library(quantmod)

# Yahoo finace symbols for NYSE, DAX, Euro Stoxx 50, S&P 500, Nikkei 225
ts_name    <- c("NYSE", "DAX", "EuroStoxx50", "S&P500", "Nikkei225")
yf_symbols <- c("^NYA", "^GDAXI", "^STOXX50E", "^GSPC", "^N225")

# Define function to get data for lapply
get_data <- function(x){
  
  raw_series <-  getSymbols(x, source = "yahoo",
                            auto.assign = FALSE,
                            from = "1900-01-01", 
                            to = "2016-06-30")[,6]
  
  log_ret    <- na.omit(diff(log(raw_series)))*100  #Compute log returns
  result     <- na.omit(cbind(log_ret, lag(log_ret)))
  
  return(result)
  
}

# Combine all data sets in one list
data_list         <- lapply(yf_symbols, get_data)
names(data_list)  <- ts_name

# Save data files seperately and as one list
file_names        <- paste0("Data/", ts_name, ".rds")

lapply(1:length(ts_name), function(x) saveRDS(data_list[[x]], file_names[x]))
saveRDS(data_list, "Data/Datasets.rds")