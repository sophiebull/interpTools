#' Simulate M_t
#' 
#' Subroutine of \code{simXt()}. Function to simulate \eqn{M_t}: the mean component of \eqn{X_t}. 
#' @param N \code{integer}; The length of the output series
#' @param numTrend \code{integer}; The number of terms to include in the varying trend component of \eqn{M_t}. Value represents the maximum degree; all lower order terms included.
#' @param a \code{numeric}; If specified, a vector of custom coefficients for each term in the polynomial.
#' @param mu \code{numeric}; If specified, a single value representing the grand mean of the polynomial.
#' @param center \code{numeric}; If specified, a vector of custom centering parameters for each term in the polynomial. 
 
simMt <- function(N = 1000, numTrend = 0, a = NULL, mu = NULL, center = NULL){
  
  stopifnot(is.numeric(N), N>=10, is.numeric(numTrend), numTrend>=0)
  
  if(any(is.null(a), is.null(mu), is.null(center))){
    nulls <- c(a = is.null(a), mu = is.null(mu), center = is.null(center))
    warning(c("Value(s): '", paste0(names(nulls)[which(nulls)], sep = ", "), "' unspecified. Defaulting to random generation."))
  }
  
  Mt_list <- list()
  t <- 0:(N-1)
  mut <- numeric(numTrend) # Placeholder vector for each term in the full model 

  ##########################
  # POLYNOMIAL COEFFICIENTS
  ##########################
  
  if(is.null(a)){
    if(numTrend > 0){
      a <- numeric(numTrend)
      for(k in 1:numTrend){
        a[k] <- rnorm(1, mean = 0, sd = N/(20*k)) # randomly generate coefficient for each term 
      }
    }
    if(numTrend == 0){
      a = 0
    }
    warning(c("Polynomial coefficient(s) = ", paste0(round(a,3), sep = " ")))
  }
  
  else if(!is.null(a)){
    if(length(a) != numTrend){
        
      missing <- rep(NA, numTrend)
      missing[1:length(a)] <- a
        
      stop(c("Polynomial coefficient(s): ", paste0("a_", which(is.na(missing)), sep = ", ") , "not specified. 
             Length of 'a' must be equal to the number of terms in the polynomial (numTrend = ", paste0(numTrend, ").")))
    }
  }
  
  ###########################
  # CENTERING PARAMETER
  ###########################
  
  if(is.null(center)){
    center <- sample(1:N, 1) # randomly generate common centering parameter for all terms
    if(numTrend == 0){
      center <- 0
    }
    warning(paste0("Centering parameter = ", center))
  }
     
  else if(!is.null(center)){ 
    if(length(center) != numTrend){
        
      missing <- rep(NA, numTrend)
      missing[1:length(center)] <- center
        
      stop(c("Centering parameter(s): ", paste0("c_", which(is.na(missing)), sep = ", ") , "not specified. 
             Length of 'center' must be equal to the number of terms in the polynomial (numTrend = ", paste0(numTrend, ").")))
    }
  }
  
  ######################
  # GRAND MEAN 
  ######################
  
  if(is.null(mu)){
    mu <- runif(1, -N/100, N/100) # randomly generate grand mean
    warning(paste0("mu = ", round(mu,3)))
  } 
  
  else if(!is.null(mu)){
    if(length(mu) != 1){
      stop("'mu' must be a single value representing the grand mean.")
    }
  }
      
  if(numTrend > 0){
    mut <- paste("(",a,")*((t-", center,")/N)^",1:numTrend," +", sep = "", collapse = "")
    mut <- gsub(".{1}$", "", mut)
  }
 
  else if(numTrend == 0){
    mut = 0
  }
  
  mut_fn <- mut
  Mt_fn <- paste(c(mut_fn,"+",mu),collapse=" ")
  
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
