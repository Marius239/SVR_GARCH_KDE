kde_uni <- function(x, data){
  
  #Argument
  n <- length(data)
  h <- 1.06*sd(data)*n^(-1/5)
  u <- (x - data)/h
  
  #Epanechnikov
  f_hat <- sum(0.75*(1 - u^2)*ifelse(abs(u) <= 1, 1, 0))/(n*h)
  
  return(f_hat)
}

kde <- function(x, data) sapply(x, function(x) kde_uni(x, data))



find_quantile <- function(dist, quantile){
  result = nlminb(start=0, objective=objective,
                  quantile = quantile,
                  dist = dist)$par
  return(result)
}

objective <- function(x, quantile, dist){
  (CDF(x, dist) - quantile)^2
}



CDF <- function(x, dist, ...){
  integrate(f=dist,
            lower=-Inf,
            upper=x, subdivisions = 4000,
            rel.tol = 1.5e-3)$value
}

