#' Simulate M_t
#' 
#' Function to simulate M_t: the mean component of X_t. 
#' @param N The length of the output series
#' @param numTrend The number of terms to include in the varying trend component of M_t. Value represents the maximum degree; all lower order terms included.
#' @param trendType The type of trends to include. Either "polynomial" or "exponential".
 
simMt <- function(N = 1000, numTrend = 0, trendType = "polynomial"){
  
  if(trendType != "polynomial" && trendType != "exponential"){
    warning("Please specify trendType as either 'polynomial' or 'exponential'.")
    stop()
  }
  
  stopifnot(is.numeric(N), N>=10, is.numeric(numTrend), numTrend>=0, (trendType == "polynomial" | trendType == "exponential"))
  
  
  Mt_list <- list()
  t <- 0:(N-1)
  
  ######### Initializing Mt 
  mu <- runif(1, -N/100, N/100)
  mut <- numeric(numTrend)
  
  if(trendType == "polynomial"){
    
    if(numTrend > 0){
      center <- sample(1:N, 1)
      if(numTrend > 1){
        for(k in 1:(numTrend-1)){
          a <- rnorm(1, mean = 0, sd = N/(20*k))
          mut[k] <- paste("(",a,")*((t-center)/N)^",k,"+",sep="")
        }
      }
      
      a <- rnorm(1, mean = 0, sd = N/(20))
      mut[numTrend] <- paste("(",a,")*((t-center)/N)^",numTrend,sep="")
      Mt_0 = NULL
    }
    
    if(numTrend == 0){
      mut = 0
    }
  }
  
  if(trendType == "exponential"){
    if(numTrend > 0){
      if(numTrend > 1){
        for(k in 1:(numTrend-1)){
          a <- sample(-(N/10):(N/10),1)
          mut[k] <- paste("(",a,")*exp(",k,"*(t/N))+",sep="")
        }
      }
      
      a <- sample(-(N/10):(N/10),1)
      mut[numTrend] <- paste("(",a,")*exp(",numTrend,"*(t/N))",sep="")
      Mt_0 = NULL
    }
    
    if(numTrend == 0){
      mut = 0
    }
  }
  
  mut_fn <- mut
  Mt_fn <- paste(c(mut_fn,"+",mu),collapse="")
  
  mut <- eval(parse(text = paste(mut,collapse="")))
  Mt <- mut+mu
  
  if(numTrend == 0){
    Mt <- rep(mu,N)
  }
  
  Mt_list$fn <- Mt_fn
  Mt_list$value <- Mt
  Mt_list$mu <- mu
  Mt_list$numTrend <- numTrend
  
  return(Mt_list)
}
