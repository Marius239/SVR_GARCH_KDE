###############################################################################
# Description: Functions to estimate Quantiles based on kernel density 
# estimation.
#
###############################################################################
# Author: Marius Lux 2017-03-26
#
###############################################################################

# Kernel density quantile estimate for observation x
kde_uni <- function(x, data, kernel){
  
  hT  <- bw.nrd0(data)  
  
  if(kernel == "gaussian") p <- mean(pnorm((x-data)/hT))
  
  return(p)
  
}

# Vectorized version of kde_uni
kde <- Vectorize(kde_uni, vectorize.args = "x")

# Estimate KDE based quantile
QKDE <- function(p, Interval, data, kernel){

    tempf <- function(t) kde(t, data, kernel)-p
    
    return(uniroot(tempf,Interval)$root)
    
}

# Cumulative distribution function of the Epanechnikov kernel
p.epa <- function(u){
  
  x <- (0.75*u - 0.25*u^3)*(abs(u) <= 1) + 0.5
  return(x)
  
}

# Density function of the Epanechnikov kernel
d.epa <- function(u){
  
  x <- 0.75*(1 - u^2)*(abs(u) <= 1) 
  return(x)
  
}