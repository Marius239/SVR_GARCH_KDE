###############################################################################
# Value-at-Risk using Block Maxima

block_maxima_VaR <- function(x, p){
  
  y     <- ifelse(rep(p, length(x)) > 0.5, x, -x)
  
  # Determine the Block Maxima data
  t    <- length(x)
  n    <- 5
  k    <- t/n
  z    <- matrix()
  
  for(count in 1:k){
    r        <- y[((count-1)*n+1):(count*n)]
    z[count] <- max(r)
  }
  
  w     <- sort(z)
  gev   <- gevFit(w, type = "mle")   
  xi    <- attr(gev, "fit")$par.ests[1] # shape parameter
  mu    <- attr(gev, "fit")$par.ests[2] # location parameter
  sigma <- attr(gev, "fit")$par.ests[3] # scale parameter
  
  # Compute quantile
  VaR <-  ifelse(p > 0.5, 
                 mu - (sigma/xi)*{ 1-(-log(p^n))^(-xi) },
                 -{mu - (sigma/xi)*{ 1-(-log((1-p)^n))^(-xi) }})
  
  return(VaR)
  
}


###############################################################################
# Value-at-Risk using peaks-over-threshold

pot_VaR <- function(x, p){
  
  # Postivie or negative returns depending on which tail is analyzed
  y       <- ifelse(rep(p, length(x)) > 0.5, x, -x)
  
  # Apply peaks-over-threshold
  quant   <- 0.7
  n       <- length(y)
  u       <- quantile(y, quant)
  N       <- sum(y > u) 
  
  gpd     <- gpdFit(y, u = u, type = "mle", information = "observed")
  gamma   <- attr(gpd,"fit")$par.ests[1]  # Location parameter
  beta    <- attr(gpd,"fit")$par.ests[2] 
  
  p_VaR   <- ifelse(p > 0.5, 1-p, p)
  q       <- u + (beta/gamma)*({(n*p_VaR)/N}^(-gamma) - 1)
  
  # Compute quantile
  VaR <-  ifelse(p > 0.5, q, -q)
  
  return(VaR)
  
}
