#### simMt.R ####
# function for simulating M_t; the mean component of X_t 
# class(trendType) == character, the type of trend component to include (polynomial, exponential, piecewise, etc.)  
# class(numTrend) == numeric, the number of trends to include. Value represents the maximum degree to assign to the last trend piece
# Note that all lower order trends are included. 

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
  
  return(Mt_list)
}