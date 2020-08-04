#' Evaluate Interpolation Performance Across Multiple Time Series
#' 
#' Function to calculate the performance metrics between lists of original and interpolated series. \cr \cr
#' See \code{?eval_performance} and additional documentation provided in this package (\code{"~/metric_definitions.pdf/"}) for a full list and description of the the performance criteria.\cr
#' Resulting object is of class '\code{pf}'.
#' 
#' @param OriginalList \code{list}; A list object of dimension D x N of original (complete) time series 
#' @param IntList \code{list}; A list object of dimension D x M x P x G x K x N of interpolated time series (output of parInterpolate.R)
#' @param GappyList \code{list}; A list object of dimension D x P x G x K x N of the gappy original time series (output of simulateGaps.R)
#' 
#' 

performance <- function(OriginalList,IntList,GappyList){
  
  D <- length(IntList)
  M <- length(IntList[[1]])
  P <- length(IntList[[1]][[1]])
  G <- length(IntList[[1]][[1]][[1]])
  K <- length(IntList[[1]][[1]][[1]][[1]])
  
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
  
  prop_vec <- as.numeric(gsub("p","",names(IntList[[1]][[1]])))
  gap_vec <- as.numeric(gsub("g","",names(IntList[[1]][[1]][[1]])))
  method_names <- names(IntList[[1]])
  
  if(is.null(names(IntList))){
    data_names <- paste0("D", 1:D)
  }
  else{
    data_names <- names(IntList)
  }
  
  # Evaluate the performance criteria for each sample in each (d,m,p,g) specification
  for(d in 1:D){
    for(m in 1:M){
      for(p in 1:P){
        prop_vec_names[p] <- c(paste("p", prop_vec[p],sep="")) # vector of names
        for(g in 1:G){
          gap_vec_names[g] <- c(paste("g", gap_vec[g],sep="")) # vector of names
          for(k in 1:K) { 
            Performance[[d]][[m]][[p]][[g]][[k]] <- unlist(eval_performance(x = OriginalList[[d]], X = IntList[[d]][[m]][[p]][[g]][[k]], gappyx = GappyList[[d]][[p]][[g]][[k]]))
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
