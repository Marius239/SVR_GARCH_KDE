library(rugarch)
library(quantmod)

###############################################################################
# Arrange results for single quantile of model

arrange_results <- function(actual, quantile, q, model, ser){
  
  if("xts" %in% class(actual)){
    
    actual <- coredata(actual)
    
  }
  
  test     <- VaRTest(alpha = q, actual = actual, VaR = quantile)
  test2    <- VaRDurTest(alpha = q, actual, VaR = quantile)
  durTest  <- round(100*test2$LRp, 2)
  
  ex       <- round(test$actual.exceed/length(actual)*100, 2)
  p_uc     <- round(test$uc.LRp*100, 2)
  p_cc     <- round(test$cc.LRp*100, 2)
  crit_ind <- test$cc.LRstat - test$uc.LRstat
  p_ind    <- round(pchisq(crit_ind, 2, lower.tail = FALSE)*100, 2)
  
  perf <- data.frame(Model = model,
                     Series = ser,
                     Quantile = q*100,
                     Exceedances = ex,
                     UC = p_uc,
                     ID = p_ind,
                     CC = p_cc)#,
  #DUR = durTest,
  #UC_DUR =( p_uc + durTest)/2)
  #                      UC_Crit = test$uc.LRstat,
  #                      ID_Crit = crit_ind,
  #                      CC_Crit = test$cc.LRstat)
  alt_perf <- alternative_test(actual, quantile, q)
  
  perf$ID <- ifelse(is.finite(perf$ID), perf$ID, alt_perf$Alt_IND)
  perf$CC <- ifelse(is.finite(perf$CC), perf$CC, alt_perf$Alt_CC)
  
  perf <- cbind(perf)
  
  return(perf)
  
}


###############################################################################
# Arrange results for data frame with different quantiles of a model

arrange_results_df <- function(ana_df, prob_foo, model, ser){
  
  list_res      <- lapply(prob_foo, 
                          function(x) arrange_results(actual = ana_df$Observation,
                                                      quantile = ana_df[, {as.numeric(gsub("[^0-9.-]+", "", names(ana_df)))/100} %in% x], 
                                                      q = x,
                                                      model = model,
                                                      ser = ser))
  
  df_res <- Reduce("rbind", list_res)
  
  result <- df_res
  
  return(result)
  
}


###############################################################################
# Test in case no successive VaR violations appeared

alternative_test <- function(actual, quantile, q){
  
  ll_fct <- function(pi, total, n){
    
    return(log( (pi^n) * (1-pi)^(total-n)) )
    
  }
  
  
  t1    <- sum(actual < quantile)
  t     <- length(actual)
  pi_1  <- t1/t
  t0    <- sum(actual >= quantile)
  t01   <- t1
  pi_01 <- t01/t0
  
  ll_h0    <- ll_fct(q, t, t1)
  ll_uc    <- ll_fct(pi_1, t, t1)
  ll_ind   <- ll_fct((1-pi_01), t0, (t0-t01))
  
  ind_crit <- 2*(ll_ind - ll_uc)
  
  uc_crit  <- 2*(ll_uc - ll_h0)
  ind_crit <- 2*(ll_ind - ll_uc)
  cc_crit  <- uc_crit + ind_crit
  
  p_uc   <- round(pchisq(uc_crit, 1, lower.tail = FALSE)*100, 2)
  p_ind  <- round(pchisq(ind_crit, 1, lower.tail = FALSE)*100, 2)
  p_cc   <- round(pchisq(cc_crit, 2, lower.tail = FALSE)*100,2)
  
  result <- data.frame(Alt_UC = p_uc, Alt_IND = p_ind, Alt_CC = p_cc)
  
  return(result)
  
}


