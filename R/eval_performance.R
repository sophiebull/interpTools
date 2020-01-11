#' Evaluate Interpolation Performance of a Single Time Series
#'  
#' Function to define and store performance criteria for the comparison of a single interpolated series with its original.
#' Criteria are "optimized" when they are either maximized (1) or minimized (0) as per the following:\cr
#' pearson_r = 1\cr
#' r_squared = 1\cr
#' abs_differences = 0\cr
#' MBE = 0\cr
#' ME = 0\cr
#' MAE = 0\cr
#' MRE = 0\cr
#' MARE = 0\cr
#' MAPE = 0\cr
#' SSE = 0 \cr
#' MSE = 0\cr
#' RMS = 0 \cr
#' NMSE = 0\cr
#' RE = 1\cr
#' RMSE = 0\cr
#' NRMSD = 0\cr
#' RMSS = 0\cr
#' MdAPE = 0\cr
#' @param x The original time series vector
#' @param X The interpolated time series vector
#' @param gappyx The gappy original time series vector

#best <- data.frame(criterion = criteria, maximize = c(1,1,rep(0,11),1,rep(0,3))) # 1 = yes, 0 = no

eval_performance <- function(x, X, gappyx) {
  # x = original , X = interpolated 
  stopifnot(is.numeric(x), is.numeric(X), length(x) == length(X), is.numeric(gappyx), length(gappyx) == length(x), length(gappyx) == length(X))
  
  n <- length(x)
  
  # identify which values were interpolated
  index <- which(is.na(gappyx))
  
  # only consider values which have been replaced
  X <- X[index]
  x <- x[index]
  
  return <- list()
  
  # Coefficent of Correlation, r
  numerator <- sum((X - mean(X))*(x - mean(x)))
  denominator <- sqrt(sum((X - mean(X))^2)) * sqrt(sum((x - mean(x))^2))
  return$pearson_r <- numerator / denominator
  
  # r^2
  return$r_squared <- return$pearson_r^2  
  
  # Absolute Differences
  return$abs_differences <- sum(abs(X - x))
  
  # Mean Bias Error 
  return$MBE <- sum(X - x) / n
  
  # Mean Error 
  return$ME <- sum(x - X) / n
  
  # Mean Absolute Error 
  return$MAE <- abs(sum(x - X)) / length(x)
  
  # Mean Relative Error 
  if (length(which(x == 0)) == 0) {
    return$MRE <- sum((x - X) / x)  
  } else {
    return$MRE <- NA
  }
  
  # Mean Absolute Relative Error ##### Lepot
  if (length(which(x == 0)) == 0) {
    return$MARE <- 1/length(x)*sum(abs((x - X) / x))
  } else {
    return$MARE <- NA 
  }
  
  # Mean Absolute Percentage Error 
  return$MAPE <- 100 * return$MARE
  
  # Sum of Squared Errors
  return$SSE <- sum((X - x)^2)
  
  # Mean Square Error 
  return$MSE <- 1 / n * return$SSE
  
  # Root Mean Squares, or Root Mean Square Errors of Prediction 
  if (length(which(x == 0)) == 0) {
    return$RMS <- sqrt(1 / n * sum(((X - x)/x)^2))
  } else {
    return$RMS <- NA 
  }
  
  # Mean Squares Error (different from MSE, referred to as NMSE)
  return$NMSE <- sum((x - X)^2) / sum((x - mean(x))^2)
  
  # Reduction of Error, also known as Nash-Sutcliffe coefficient 
  return$RE <- 1 - return$NMSE
  
  # Root Mean Square Error, also known as Root Mean Square Deviations
  return$RMSE <- sqrt(return$MSE)
  
  # Normalized Root Mean Square Deviations 
  return$NRMSD <- 100 * (return$RMSE / (max(x) - min(x)))
  
  # Root Mean Square Standardized Error 
  if (sd(x) != 0) {
    return$RMSS <- sqrt(1 / n * sum(( (X-x)/sd(x) )^2))  
  } else {
    return$RMSS <- NA 
  }
  
  # Median Absolute Percentage Error
  if (length(which(x == 0)) == 0) {
    return$MdAPE <- median(abs((x - X) / x))*100  
  } else {
    return$MdAPE <- NA
  }
  
  return(return)
}

