#' Evaluate Interpolation Performance Across Multiple Time Series
#' 
#' Function to calculate the performance metrics between lists of original and interpolated series. \cr \cr
#' See \code{?eval_performance} and additional documentation provided in this package (\code{"~/metric_definitions.pdf/"}) for a full list and description of the the performance criteria.
#' 
#' @param OriginalData \code{list}; A list object of dimension D x N of original (complete) time series 
#' @param IntData \code{list}; A list object of dimension D x M x P x G x K x N of interpolated time series (output of parInterpolate.R)
#' @param GappyData \code{list}; A list object of dimension D x P x G x K x N of the gappy original time series (output of simulateGaps.R)
#' 
#' 

performance <- function(OriginalData,IntData,GappyData){
  
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
  
  prop_vec <- as.numeric(gsub("p","",names(IntData[[1]][[1]])))
  gap_vec <- as.numeric(gsub("g","",names(IntData[[1]][[1]][[1]])))
  method_names <- names(IntData[[1]])
    
  # Evaluate the performance criteria for each sample in each (d,m,p,g) specification
  for(d in 1:D){
    for(m in 1:M){
      for(p in 1:P){
        prop_vec_names[p] <- c(paste("p", prop_vec[p],sep="")) # vector of names
        for(g in 1:G){
          gap_vec_names[g] <- c(paste("g", gap_vec[g],sep="")) # vector of names
          for(k in 1:K) { 
            Performance[[d]][[m]][[p]][[g]][[k]] <- unlist(eval_performance(x = OriginalData[[d]], X = IntData[[d]][[m]][[p]][[g]][[k]], gappyx = GappyData[[d]][[p]][[g]][[k]]))
          }
          names(Performance[[d]][[m]][[p]]) <- gap_vec_names
        }
        names(Performance[[d]][[m]]) <- prop_vec_names
      }
      names(Performance[[d]]) <- method_names
    }
    names(Performance) <- names(IntData)
  }
  
  Performance <- lapply(Performance, function(x) 
                    lapply(x, function(x) 
                      lapply(x, function(x)
                        lapply(x, function(x){
                            logic <- unlist(lapply(x,FUN = function(x) !is.null(x)))
                            x <-x[logic]
                      }))))
  
  class(Performance) <- "pmat"
  return(Performance) 
}
