#' Evaluate Interpolation Performance Across Multiple Time Series
#' 
#' Function to calculate the performance metrics between lists of original and interpolated series. \cr \cr
#' See \code{?eval_performance} and additional documentation provided in this package (\code{"~/metric_definitions.pdf/"}) for a full list and description of the the performance criteria.\cr
#' Resulting object is of class '\code{pf}'.
#' 
#' @param OriginalData \code{list}; A list object of dimension D x N of original (complete) time series 
#' @param IntData \code{list}; A list object of dimension D x M x P x G x K x N of interpolated time series (output of parInterpolate.R)
#' @param GappyData \code{list}; A list object of dimension D x P x G x K x N of the gappy original time series (output of simulateGaps.R)
#' @param custom \code{character}; A vector of names of user-defined functions used to calculate custom performance metrics (see details) 
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
#'  performance(OriginalData = OriginalData, IntData = IntData, GappyData = GappyData, custom = c("my_metric1", "my_metric2"))
#' 

performance <- function(OriginalData, IntData, GappyData, custom = NULL){
  
  D <- length(IntData)
  M <- length(IntData[[1]])
  P <- length(IntData[[1]][[1]])
  G <- length(IntData[[1]][[1]][[1]])
  K <- length(IntData[[1]][[1]][[1]][[1]])
  
  # Initializing nested list object
  Performance <- lapply(Performance <- vector(mode = 'list', D),function(x)
                  lapply(Performance <- vector(mode = 'list', M),function(x) 
                    lapply(Performance <- vector(mode = 'list', P),function(x) 
                      lapply(Performance <- vector(mode = 'list', G),function(x)
                        x<-vector(mode='list', K)))))
  
  prop_vec_names <- numeric(P)
  gap_vec_names <- numeric(G)
  method_names <- numeric(M)
  data_names <- numeric(D)
  
  prop_vec <- as.numeric(gsub("p","",names(IntData[[1]][[1]])))
  gap_vec <- as.numeric(gsub("g","",names(IntData[[1]][[1]][[1]])))
  method_names <- names(IntData[[1]])
  
  if(is.null(names(IntData))){
    data_names <- paste0("D", 1:D)
  }
  else{
    data_names <- names(IntData)
  }
  
  # Evaluate the performance criteria for each sample in each (d,m,p,g) specification
  for(d in 1:D){
    for(m in 1:M){
      for(p in 1:P){
        prop_vec_names[p] <- c(paste("p", prop_vec[p],sep="")) # vector of names
        for(g in 1:G){
          gap_vec_names[g] <- c(paste("g", gap_vec[g],sep="")) # vector of names
          for(k in 1:K) { 
            Performance[[d]][[m]][[p]][[g]][[k]] <- unlist(eval_performance(x = OriginalData[[d]], X = IntData[[d]][[m]][[p]][[g]][[k]], gappyx = GappyData[[d]][[p]][[g]][[k]], custom = custom))
          }
          names(Performance[[d]][[m]][[p]]) <- gap_vec_names
        }
        names(Performance[[d]][[m]]) <- prop_vec_names
      }
      names(Performance[[d]]) <- method_names
    }
    names(Performance) <- data_names
  }
  
  Performance <- lapply(Performance, function(x) 
                    lapply(x, function(x) 
                      lapply(x, function(x)
                        lapply(x, function(x){
                            logic <- unlist(lapply(x,FUN = function(x) !is.null(x)))
                            x <-x[logic]
                      }))))
  
  class(Performance) <- "pf"
  return(Performance) 
}
