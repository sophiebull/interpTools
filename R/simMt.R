#' Simulate M_t
#' 
#' Function to simulate M_t: the mean component of X_t. 
#' @param n The length of the output series
#' @param numTrend The number of terms to include in the varying trend component of M_t. Value represents the maximum degree; all lower order terms included.
#' @param trendType The type of trends to include

simMt <- function(n = 1000, numTrend = 0, trendType = "polynomial"){
  
  Mt_list <- list()
  t <- 0:(n-1)
  
  ######### Initializing Mt 
  mu <- sample(0:(n/10),1)
  mut <- numeric(numTrend)
  
  if(trendType == "polynomial"){
    
    if(numTrend > 0){
      if(numTrend > 1){
        for(k in 1:(numTrend-1)){
          a <- sample(-(n/10):(n/10),1)
          mut[k] <- paste("(",a,")*(t/n)^",k,"+",sep="")
        }
      }
      
      a <- sample(-(n/10):(n/10),1)
      mut[numTrend] <- paste("(",a,")*(t/n)^",numTrend,sep="")
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
          a <- sample(-(n/10):(n/10),1)
          mut[k] <- paste("(",a,")*exp(",k,"*(t/n))+",sep="")
        }
      }
      
      a <- sample(-(n/10):(n/10),1)
      mut[numTrend] <- paste("(",a,")*exp(",numTrend,"*(t/n))",sep="")
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
    Mt <- rep(mu,n)
  }
  
  Mt_list$fn <- Mt_fn
  Mt_list$value <- Mt
  Mt_list$mu <- mu
  Mt_list$numTrend <- numTrend
  
  return(Mt_list)
}
