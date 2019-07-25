#' Evaluate Interpolation Performance of a Single Time Series
#' 
#' Function to define and store performance criteria for the comparison of a single interpolated series with its original.
#' Criteria are "optimized" when they are either maximized (1) or minimized (0) as per the following:
#' # R = 1
#' R^2 = 1
#' abs_differences = 0
#' MBE = 0
#' ME = 0
#' MAE = 0
#' MRE = 0
#' MARE = 0
#' MAPE = 0
#' SSE = 0 
#' MSE = 0
#' RMS = 0 
#' NMSE = 0
#' RE = 1
#' RMSE = 0
#' NRMSD = 0
#' RMSS = 0
#' @param x The original time series vector
#' @param X The interpolated time series vector

#best <- data.frame(criterion = criteria, maximize = c(1,1,rep(0,11),1,rep(0,3))) # 1 = yes, 0 = no

eval_performance <- function(x, X) {
  # x = original , X = interpolated 
  stopifnot(is.numeric(x), is.numeric(X), length(x) == length(X))
  
  n <- length(x)
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
  
  return(return)
}