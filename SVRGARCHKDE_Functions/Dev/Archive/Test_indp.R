library(rugarch)

value   <- coredata(res2$True)
q_fcast <- res2$FCast_Upper


VaRTest(alpha = 0.99, 
        value, 
        q_fcast, 
        conf.level = 0.95)

#Implement test for independence


# ll_bin <- function(k, n, p){
# 
#   ll <- log((gamma(n+1)/{gamma(k+1)*gamma(n-k+1)})*{(1-p)^(n-k)}*p^k)
# 
#   return(ll)
# 
# }
# 
# ll_bin(k = 1.7, n = 100, p = 17/1000)


LR_uc <- function(k, n, p){
  
  lr <- k*log(k/(p*n)) + (n - k)*log((n-k)/{(1-p)*n})
  return(lr*2) 
}



LR_ind <- function(value, q_fcast){
  
  idx <- -value > -q_fcast
  
  n_00 <- sum(idx[-1] == idx[-length(idx)] & idx[-1] == 0)
  n_11 <- sum(idx[-1] == idx[-length(idx)]& idx[-1] == 1)
  n_10 <- sum(idx[-1] != idx[-length(idx)] & idx[-1] == 1)
  n_01 <- sum(idx[-1] != idx[-length(idx)]& idx[-1] == 0)
  
  pi_0 <- (n_01 + n_11)/(n_00 + n_11 + n_10 + n_01)
  L_H0 <- {(1-pi_0)^(n_00+n_10)}*(pi_0^(n_01+n_11))
  
  pi_01 <- n_01/(n_00 + n_01)
  pi_11 <- n_11/(n_10 + n_11)
  L_H1 <- {(1-pi_01)^n_00}*(pi_01^n_01)*{(1-pi_11)^n_10}*pi_11^n_11
  
  lr <- log(L_H1) - log(L_H0)
  return(lr*2) 
}

LR_uc(983, 1000, 0.99)

LR_uc(983, 1000, 0.99) + LR_ind(value, q_fcast)

