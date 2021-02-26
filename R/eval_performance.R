#' Evaluate Interpolation Performance of a Single Time Series
#' 
#' Function to define and store performance criteria for the comparison of a single interpolated series with its original. Vectors must be conforming in value, except at indices where missing observations have been interpolated. Subroutine of \code{performance()}.
#'    
#' @param x \code{numeric}; The \strong{original} time series vector.
#' @param X \code{numeric}; The \strong{interpolated} time series vector.
#' @param gappyx \code{numeric}; The \strong{gappy} time series vector. Gaps must be indicated by \code{NA}.
#' @param custom \code{character}; A vector of names of user-defined functions used to calculate custom performance metrics (see details) 
#' 
#' @details The following is a description of the list of performance metrics that are generated: \cr
#' \tabular{ccc}{
#'      ID \tab Criterion \tab Optimal \cr
#'      ...... \tab ........... \tab ......... \cr
#'      1 \tab  pearson_r \tab max  \cr
#'      2 \tab  r_squared \tab max  \cr
#'      3 \tab  AD \tab min  \cr
#'      4 \tab  MBE \tab min  \cr
#'      5 \tab  ME \tab min  \cr
#'      6 \tab  MAE \tab min  \cr
#'      7 \tab  MRE \tab min  \cr
#'      8 \tab  MARE \tab min  \cr
#'      9 \tab  MAPE \tab min  \cr
#'      10 \tab  SSE \tab min  \cr
#'      11 \tab  MSE \tab min  \cr
#'      12 \tab  RMS \tab min  \cr
#'      13 \tab  NMSE \tab min  \cr
#'      14 \tab  RE \tab max  \cr
#'      15 \tab  RMSE \tab min  \cr
#'      16 \tab  NRMSD \tab min  \cr
#'      17 \tab  RMSS \tab min  \cr
#'      18 \tab  MdAPE \tab min  \cr
#'    }
#'
#' @details
#' Users can define and pass-in their own custom performance metric functions, but must adhere to the following rules:
#' \itemize{
#'   \item Inputs are limited to *ONLY* \code{x} (lowercase; the original time series) and \code{X} (uppercase; the interpolated time series)\cr
#'   \item Output must be a single numeric value\cr
#'   }
#'        
#' @examples 
#'  # User-defined functions to calculate a custom performance metric (see Details for rules)
#'  
#'  my_metric1 <- function(x,X){
#'  
#'   # Sum of original + interpolated values
#'   
#'   val <- x + X
#'   
#'   return(val) # return value must be a single numeric element
#'   
#'   }
#'   
#'  my_metric2 <- function(x,X){
#'  
#'   # Sum of index positions of interpolated values
#'  
#'   val <- sum(which(x != X))
#'  
#'   return(val) # return value must be a single numeric element
#'  
#'  } 
#'  
#'  # Implementing in eval_performance()
#'  
#'  eval_performance(x = x, X = X, gappyx = gappyx, custom = c("my_metric1", "my_metric2"))
#'  
#'
#' 

eval_performance <- function(x, X, gappyx, custom = NULL) {

  # x = original , X = interpolated 
  
  if(sum(is.na(gappyx)) == 0) stop(paste0("Gappy data in 'gappyx' does not contain NAs. Please impose gaps and try again."))
  if(sum(x - gappyx, na.rm = TRUE) != 0) stop(paste0("Gappy data in 'gappyx' is not representative of 'x' (original data). The two vectors are non-conforming."))
  if(sum(X[which(!is.na(gappyx))] - x[which(!is.na(gappyx))]) != 0) stop(paste0("Non-interpolated points in 'X' do not match those of the original data in 'x'.  The two vectors are non-conforming."))
  
  if(!is.null(X)){
  stopifnot((is.numeric(x) | is.null(x)),
            (is.numeric(X) | is.null(X)),
            (is.numeric(gappyx) | is.null(gappyx)),
            length(x) == length(X),
            length(gappyx) == length(x), 
            length(gappyx) == length(X))

  # identify which values were interpolated
  index <- which(is.na(gappyx))
  
  # only consider values which have been replaced
  X <- X[index]
  x <- x[index]
  
  n <- length(x)
  
  return <- list()
  
  # Coefficent of Correlation, r
  numerator <- sum((X - mean(X))*(x - mean(x)))
  denominator <- sqrt(sum((X - mean(X))^2)) * sqrt(sum((x - mean(x))^2))
  return$pearson_r <- numerator / denominator
  
  # r^2
  return$r_squared <- return$pearson_r^2  
  
  # Absolute Differences
  return$AD <- sum(abs(X - x))
  
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
  
  # Trimmed Mean Absolute Percentage Error
  #if (length(which(x == 0)) == 0) {
  #  diffs <- abs((x-X)/x)
  #  qs <- quantile(diffs, probs = c(0.05,0.95))
    
  #  logic <- (diffs < qs["5%"]) | (diffs > qs["95%"])
  #  diffs <- diffs[!logic]
    
  #  TMARE <- 1/length(diffs) * sum(diffs)
    
  #  return$TMAPE <- 100*TMARE
    
  #  } else {
  #    return$TMAPE <- NA 
  #  }
  
  # Custom functions
  
  if(!is.null(custom)){
    
    ####################
    ### LOGICAL CHECKS
    ####################
    
    n_custom <- length(custom)
    
    if(n_custom == 1){
      is_fn <- !inherits(try(match.fun(custom), silent = TRUE), "try-error") # FALSE if not a function
    }
    else if(n_custom > 1){
      is_fn <- logical(length(custom))
      for(k in 1:n_custom){
        is_fn[k] <- !inherits(try(match.fun(custom[k]), silent = TRUE), "try-error") # FALSE if not a function
      }
    }
   
    if(!all(is_fn)){
      not <- which(!is_fn)
      stop(c("Custom function(s): ", paste0(custom[not], sep = " ") ,", are not of class 'function'."))
    }
    
    # Check that the output of the function is a single value
    
    check_single <- function(fn){
      
      x <- rnorm(10)
      X <- rnorm(10)
  
      val <- match.fun(fn)(x = x, X = X)
      
      return(all(length(val) == 1, is.numeric(val)))
    }
    
    logic <- logical(n_custom)
    
    for(k in 1:n_custom){
      logic[k] <- check_single(custom[k])
    }
    
    if(!all(logic)){
      stop(c("Custom function(s): ", paste0(custom[!logic], sep = " "), ", do not return a single numeric value."))
    }
    
    # Computing custom metric values
    
    return_call <- character(n_custom)
    
    for(k in 1:n_custom){
      
      return_call[k] <- paste0("return$",custom[k]," <- match.fun(",custom[k],")(x = x, X = X)")
      
      eval(parse(text = return_call[k]))
      
    }
    
  }
  
  
  return(return)
  }
  
  else if(is.null(X)){
    return(NULL)
  }
}

