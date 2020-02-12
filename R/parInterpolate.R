#' Parallel Interpolation 
#' 
#' Function to perform interpolation on gappy series in parallel using user-specified interpolation methods.\cr\cr
#' INTERPOLATION METHOD IDS\cr
#' 1 - Nearest Neighbor - NN\cr
#' 2 - Linear Interpolation - LI\cr
#' 3 - Natural Cubic Spline - NCS\cr
#' 4 - FMM Cubic Spline - FMM\cr 
#' 5 - Hermite Cubic Spline - HCS\cr
#' 6 - Stineman Interpolation - SI\cr
#' 7 - Kalman ARIMA - KAF\cr
#' 8 - Kalman StructTS - KSF\cr
#' 9 - Last Observation Carried Forward - LOCF\cr
#' 10 - Next Observation Carried Backward - NOCB\cr
#' 11 - Simple Moving Average - SMA\cr
#' 12 - Linear Weighted Moving Average - LWMA\cr
#' 13 - Exponential Weighted Moving Average - EWMA\cr
#' 14 - Replace with Mean - RMEA\cr
#' 15 - Replace with Median - REMD\cr
#' 16 - Replace with Mode - RMOD\cr
#' 17 - Replace with Random -RRND\cr
#' 18 - Hybrid Wiener Interpolator - HWI\cr
#' 
#' @param gappyTS A gappy time series vector
#' @param methods vector of IDs for selected interpolation methods, where m = 1,...,M
#' @param FUN_CALL User specified interpolation function to be applied to gappyTS. Must be a character string.
#' @examples
#'# Built-in interpolators
#'methods <- c(17,5) # Replace with Random, Hermite Cubic Spline
#'
#'# User-defined functions to pass to FUN_CALL
#'## Toy function 1: Convert each value of x to its index position
#'plus <- function(x){
#'vec <- numeric(length(x))
#'for(i in 1:length(vec)){
#'  vec[i] <- i
#'}
#'return(vec)
#'}
#'
#'## Toy function 2: Convert each value of x to its negative index position
#'minus <- function(x){
#'  vec <- numeric(length(x))
#'  for(i in 1:length(vec)){
#'    vec[i] <- -i
#'  }
#'  return(vec)
#'}
#'
#'FUN_CALL <- c("plus(","minus(")
#'
#'IntData <- list()
#'
#'for(d in 1:length(OriginalData)){
#'  IntData[[d]] <- parInterpolate(gappyTS = GappyData[[d]], methods = methods, FUN_CALL = FUN_CALL)
#'}
#'names(IntData) <- names(OriginalData)


parInterpolate <- function(gappyTS, methods = NULL, FUN_CALL = NULL, numCores = detectCores()){
  # CALLING REQUIRED LIBRARIES
  require(multitaper)
  require(tsinterp)
  require(imputeTS)
  require(zoo)
  require(forecast)
  require(MASS)
  require(snow)
  require(parallel)
  
  stopifnot(!(is.null(methods) && is.null(FUN_CALL)), !(is.null(gappyTS))#, is.ts(gappyTS)
            )

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
  
  algorithm_names <- c("NN",
                       "LI", 
                       "NCS",
                       "FMM", 
                       "HCS",
                       "SI",
                       "KAF",
                       "KKSF",
                       "LOCF",
                       "NOCB", 
                       "SMA", 
                       "LWMA",
                       "EWMA",
                       "RMEA",
                       "RMED", 
                       "RMOD",
                       "RRND",
                       "HWI")
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
                       "tsinterp::interpolate(gap = which(is.na(x) == TRUE), progress = FALSE, z = ")
  
  algorithms <- data.frame(algorithm_names, algorithm_calls)
  
  #Creating a list object to store interpolated series
  int_series <- lapply(int_series <- vector(mode = 'list',(length(methods)+length(FUN_CALL))),function(x)
    lapply(int_series <- vector(mode = 'list', length(gappyTS)),function(x) 
      lapply(int_series <- vector(mode = 'list',length(gappyTS[[1]])),function(x) 
        x<-vector(mode='list',length(gappyTS[[1]][[1]])))))
  
  method_names <- numeric(length(methods))
  fun_names <- numeric(length(FUN_CALL))
  
  if(!(missing(methods))){
  
      for(m in 1:length(methods)){ 
        method_names[m] <- algorithm_names[methods[m]]
        
        if(methods[m] == 18){
          function_call <- paste(algorithm_calls[methods[m]], "x", ")","[[1]]", sep = "")
        }
        else{
          function_call <- paste(algorithm_calls[methods[m]], "x", ")", sep = "")
        }
        
        int_series[[m]] <- lapply(gappyTS, function(x){
          lapply(x, function(x){
            mclapply(x, function(x){
              eval(parse(text = function_call))}
              , mc.cores = numCores)}
          )})
      }
  }
  
  if(!(missing(FUN_CALL))){
    for(l in 1:length(FUN_CALL)){
  
      FUN_call <- paste(FUN_CALL[l],"x",")",sep="")
      fun_names[l] <- sub("\\(.*", "", FUN_call)
      
      int_series[[length(methods)+l]] <- mclapply(gappyTS, function(x){
        lapply(x, function(x){
          lapply(x, function(x){
            eval(parse(text = FUN_call))}
          )}
        )},
        #mc.cores = detectCores())
        mc.cores = numCores)
    }
  }
  
  names(int_series) <- c(method_names,fun_names)
  return(int_series)
}





