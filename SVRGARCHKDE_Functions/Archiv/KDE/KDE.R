cat("\014") 
rm(list = ls())
graphics.off()

suppressMessages(library(quantmod))


#------------------------------------------------------------------------------
#Load data and prepare for analysis

series <- suppressWarnings(getSymbols("^GDAXI", source = "yahoo", auto.assign = FALSE))
#series <- series[.indexmday(series)==1]
#series <- series[200:950,6]
series <- series$GDAXI.Adjusted
data <- coredata(dailyReturn(series, subset=NULL, type='arithmetic'))
#data <- -1*na.omit(diff(log(coredata(series))))*100



kde_uni <- function(x, data){

  #Argument
  n <- length(data)
  h <- 1.06*sd(data)*n^(-1/5)
  u <- (x - data)/h

  #Epanechnikov
  f_hat <- sum(0.75*(1 - u^2)*ifelse(abs(u) <= 1, 1, 0))/(n*h)

  return(f_hat)
}
# 
# trapezoid <- function(fun, a, b, n=100) {
#   # numerical integral of fun from a to b
#   # using the trapezoid rule with n subdivisions
#   # assume a < b and n is a positive integer
#   h <- (b-a)/n
#   x <- seq(a, b, by=h)
#   y <- fun(x)
#   s <- h * (y[1]/2 + sum(y[2:n]) + y[n+1]/2)
#   return(s)
# }
# 
# 
kde <- function(x) sapply(x, function(x) kde_uni(x, data))

# kcdf <- function(x){
# 
#   #trapezoid(kde, a = -10000, b = x)
#   area <- integrate(kde, lower = -Inf, upper = x, subdivisions = 5000)
#   area$value
# }





#################################################################
objective <- function(x, quantile, dist){
  (CDF(x, dist) - quantile)^2
}

find_quantile <- function(dist, quantile){
  result = nlminb(start=0, objective=objective,
                  quantile = quantile,
                  dist = dist)$par
  return(result)
}

CDF <- function(x, dist){
  integrate(f=dist,
            lower=-Inf,
            upper=x, subdivisions = 4000,
            rel.tol = 1.5e-3)$value
}

d <- function(x) dnorm(x, mean(data), sd(data))

# v <- 3
# (1 - kcdf(v))*100 #Empirical
# (1 - integrate(d, lower = -Inf, v)$value)*100 #normal
# 
# x   <- seq(2,5,length=10000)
# y   <- dnorm(x,mean=mean(data), sd=sd(data))
# plot(x,y, type="l", lwd=1, ylim = c(0, 0.1))
# 
# lines(density(data), col = "green")
start <- Sys.time()
find_quantile(dist = kde, quantile = 0.99)
end <- Sys.time()
end - start
#round(find_quantile(dist = kde, quantile = 0.99),1)
#round(find_quantile(dist = d, 0.99),1)
