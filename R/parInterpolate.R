#' Parallel Interpolation 
#' 
#' Function to perform interpolation on gappy series in parallel using user-specified interpolation methods.
#' INTERPOLATION METHOD IDS
#' 1 - Nearest Neighbor
#' 2 - Linear Interpolation
#' 3 - Natural Cubic Spline
#' 4 - FMM Cubic Spline
#' 5 - Hermite Cubic Spline
#' 6 - Stineman Interpolation
#' 7 - Kalman - ARIMA
#' 8 - Kalman - StructTS
#' 9 - Last Observation Carried Forward
#' 10 - Next Observation Carried Backward
#' 11 - Simple Moving Average
#' 12 - Linear Weighted Moving Average
#' 13 - Exponential Weighted Moving Average
#' 14 - Replace with Mean
#' 15 - Replace with Median
#' 16 - Replace with Mode
#' 17 - Replace with Random
#' 18 - Hybrid Wiener 
#' 
#' @param gappyTS A gappy time series vector
#' @param methods vector of IDs for selected interpolation methods, M
#' 

parInterpolate <- function(gappyTS, methods){ 
  
  #Creating a list object to store interpolated series
  int_series <- lapply(int_series <- vector(mode = 'list',length(methods)),function(x)
    lapply(int_series <- vector(mode = 'list', length(gappyTS)),function(x) 
      lapply(int_series <- vector(mode = 'list',length(gappyTS[[1]])),function(x) 
        x<-vector(mode='list',length(gappyTS[[1]][[1]])))))
  
  ## Would be nice to wrap function in mclapply() instead of for()... 
  # but the irony is that it will take too much time to learn how to do! :) 
  
  method_names <- numeric(length(methods))
  
  for(m in 1:length(methods)){ 
    method_names[m] <- algorithm_names[methods[m]]
    
    if(methods[m] == 18){
      function_call <- paste(algorithm_calls[methods[m]], "x", ")","[[1]]", sep = "")
    }
    else{
      function_call <- paste(algorithm_calls[methods[m]], "x", ")", sep = "")
    }
    
    int_series[[m]] <- mclapply(gappyTS, function(x){
      lapply(x, function(x){
        lapply(x, function(x){
          eval(parse(text = function_call))}
        )}
      )}, 
      mc.cores = detectCores())
  }
  
  names(int_series) <- method_names
  return(int_series)
}
