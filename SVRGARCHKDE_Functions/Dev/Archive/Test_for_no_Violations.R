#Tesf für t11 = 0

rm(list = ls())
graphics.off()

setwd(paste0(Sys.getenv("USERPROFILE"), "/Desktop/Master/Master_Thesis/Simulation"))

library(xtable)

source("SVRGARCHKDE_Functions/Dev/Result2Table_Functions.R")

project_folder     <- "From1996to2006_ZeroMean"
series_name        <- "Nikkei225"  

# Plot a result
# idx      <- 168
# prob   <- 0.01*100
plot_var <- function(idx, prob){
  
  el <- idx
  zzz <- readRDS(paste0("Tuning\\Results\\RDS_", series_name, "\\", project_folder, "\\VaR\\Nikkei_VaR_", el, ".rds"))
  plot(zzz[[1]]$Observation, type = "l")
  quant <- xts(x = zzz[[1]][, which(names(zzz[[1]]) == paste0("Quantile_", prob,"%"))], order.by = index(zzz[[1]]$Observation))
  lines(quant, col = "green")
  ind <- coredata(zzz[[1]]$Observation) < zzz[[1]][, which(names(zzz[[1]]) == paste0("Quantile_", prob,"%"))]
  points(zzz[[1]]$Observation[ind], pch = 16, col = "blue")
  
  return(zzz)
  
}



var_data <- plot_var(431, 0.5)
 
 df <- var_data[[1]]
# t1 <- sum(df$Observation < df$`Quantile_0.5%`)
# t  <- nrow(df)
# 
# 
# 
# cc_crit  <- ind_crit + 2*log(ll_uc / { (0.005^t1)*(1-0.005)^(t-t1)  })
#   
#   
# pi_hat_uc <- a$Exceedances/100
# ll_uc     <- (pi_hat_uc^t1)*(1-pi_hat_uc)^(t-t1)
# pchisq(2*log(ll_uc / { (0.005^t1)*(1-0.005)^(t-t1)  }), 1, lower.tail = FALSE)
# 
# t0  <- sum(df$Observation > df$`Quantile_0.5%`)
# t01 <- t1
# pi_01 <- t01/t0
# 
# ll_ind <- {(1-pi_01)^(t0-t01)}*pi_01^t01
# 
# pchisq(2*log(ll_ind/ll_uc), 1, lower.tail = FALSE)
# 
# 
# cc <- 2*log(ll_ind/ll_uc) + 2*log(ll_uc / { (0.005^t1)*(1-0.005)^(t-t1)  })
# 
# pchisq(cc, 2, lower.tail = FALSE)



###############################################################################
# 431  1000 0.7 1e+00 SVM_KDE S&P500      0.5        0.49 93.14   NaN   NaN 51.04 72.090
 
 
alternative_test <- function(){
  
  ll_fct <- function(pi, total, n){
    
    return(log( (pi^n) * (1-pi)^(total-n)) )
    
  }
  
  
  t1   <- sum(coredata(df$Observation)[,1] < df$`Quantile_0.5%`)
  t    <- nrow(df)
  pi_1 <- t1/t
  t0  <- sum(coredata(df$Observation)[,1] > df$`Quantile_0.5%`)
  t01 <- t1
  pi_01 <- t01/t0
  
  ll_h0    <- ll_fct(0.005, t, t1)
  ll_uc    <- ll_fct(pi_1, t, t1)
  ll_ind   <- ll_fct((1-pi_01), t0, (t0-t01))
  
  ind_crit <- 2*(ll_ind - ll_uc)
  
  uc_crit  <- 2*(ll_uc - ll_h0)
  ind_crit <- 2*(ll_ind - ll_uc)
  cc_crit  <- uc_crit + ind_crit
  
  pchisq(uc_crit, 1, lower.tail = FALSE)
  pchisq(ind_crit, 1, lower.tail = FALSE)
  pchisq(cc_crit, 2, lower.tail = FALSE)
  
  
}
 



t1   <- sum(coredata(df$Observation)[,1] < df$`Quantile_0.5%`)
t    <- nrow(df)
pi_1 <- t1/t
t0  <- sum(coredata(df$Observation)[,1] > df$`Quantile_0.5%`)
t01 <- t1
pi_01 <- t01/t0

ll_h0    <- ll_fct(0.005, t, t1)
ll_uc    <- ll_fct(pi_1, t, t1)
ll_ind   <- ll_fct((1-pi_01), t0, (t0-t01))

ind_crit <- 2*(ll_ind - ll_uc)

uc_crit  <- 2*(ll_uc - ll_h0)
ind_crit <- 2*(ll_ind - ll_uc)
cc_crit  <- uc_crit + ind_crit

pchisq(uc_crit, 1, lower.tail = FALSE)
pchisq(ind_crit, 1, lower.tail = FALSE)
pchisq(cc_crit, 2, lower.tail = FALSE)

