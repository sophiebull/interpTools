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
  ## DEFINING INTERPOLATION ALGORITHMS
  nearestNeighbor <- function(x) {
    stopifnot(is.ts(x)) 
    
    findNearestNeighbors <- function(x, i) {
      leftValid <- FALSE
      rightValid <- FALSE 
      numItLeft <- 1
      numItRight <- 1
      while (!leftValid) {
        leftNeighbor <- x[i - numItLeft]
        if (!is.na(leftNeighbor)) {
          leftValid <- TRUE
          leftNeighbor <- i - numItLeft
        }
        numItLeft <- numItLeft + 1
      }
      while (!rightValid) {
        rightNeighbor <- x[i + numItRight]
        if (!is.na(rightNeighbor)) {
          rightValid <- TRUE
          rightNeighbor <- i + numItRight
        }
        numItRight <- numItRight + 1
      }
      return(c(leftNeighbor, rightNeighbor))
    }
    
    for (i in 1:length(x)) {
      if (is.na(x[i])) {
        nearestNeighborsIndices <- findNearestNeighbors(x, i)
        a <- nearestNeighborsIndices[1]
        b <- nearestNeighborsIndices[2]
        if (i < ((a + b) / 2)) {
          x[i] <- x[a]
        } else {
          x[i] <- x[b]
        }
      }
    }
    return(x)
  }
  
  algorithm_names <- c("Nearest.Neighbor",
                       "Linear.Interpolation", 
                       "Natural.Cubic.Spline",
                       "FMM Cubic.Spline", 
                       "Hermite.Cubic.Spline",
                       "Stineman.Interpolation",
                       "Kalman.ARIMA",
                       "Kalman.StructTS",
                       "Last.Observation.Carried.Forward",
                       "Next.Observation.Carried.Backward", 
                       "Simple.Moving.Average", 
                       "Linear.Weighted.Moving.Average",
                       "Exponential.Weighted.Moving.Average",
                       "Replace.with.Mean",
                       "Replace.with.Median", 
                       "Replace.with.Mode",
                       "Replace.with.Random",
                       "Hybrid.Wiener.Interpolator")
  algorithm_calls <- c("nearestNeighbor(", 
                       "na.approx(", 
                       "na.spline(method = 'natural', object = ",
                       "na.spline(method = 'fmm', object = ", 
                       "na.spline(method = 'monoH.FC', object = ", 
                       "na_interpolation(option = 'stine', x = ", 
                       "na_kalman(model = 'auto.arima', x = ", 
                       "na_kalman(model = 'StructTS', x = ",
                       "imputeTS::na.locf(option = 'locf', x = ", 
                       "imputeTS::na.locf(option = 'nocb', x = ", 
                       "na_ma(weighting = 'simple', x = ",
                       "na_ma(weighting = 'linear', x = ", 
                       "na_ma(weighting = 'exponential', x = ",
                       "na_mean(option = 'mean', x = ", 
                       "na_mean(option = 'median', x = ",
                       "na_mean(option = 'mode', x = ", 
                       "na_random(",
                       "interpolate(gap = which(is.na(x) == TRUE), progress = FALSE, z = ")
  
  algorithms <- data.frame(algorithm_names, algorithm_calls)
  
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
