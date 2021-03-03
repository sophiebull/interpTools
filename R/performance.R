#' Evaluate Interpolation Performance Across Multiple Time Series
#' 
#' Function to calculate the performance metrics between lists of original and interpolated series. \cr \cr
#' See additional documentation provided in this package (\code{"~/metric_definitions.pdf/"}) for a full descriptions of the the performance criteria.\cr
#' Resulting object is of class '\code{pf}'.
#' 
#' @param OriginalData \code{list}; A list object of dimension D x N of original (complete) time series 
#' @param IntData \code{list}; A list object of dimension D x M x P x G x K x N of interpolated time series (output of \code{parInterpolate()})
#' @param GappyData \code{list}; A list object of dimension D x P x G x K x N of the gappy original time series (output of \code{simulateGaps()})
#' @param custom \code{character}; A vector of names of user-defined functions used to calculate custom performance metrics (see details) 
#' 
#' 
#' @details The following is a description of the list of base performance metrics included in the returned object: \cr
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
  pf <- lapply(pf <- vector(mode = 'list', D),function(x)
                  lapply(pf <- vector(mode = 'list', M),function(x) 
                    lapply(pf <- vector(mode = 'list', P),function(x) 
                      lapply(pf <- vector(mode = 'list', G),function(x)
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
            pf[[d]][[m]][[p]][[g]][[k]] <- unlist(eval_performance(x = OriginalData[[d]], X = IntData[[d]][[m]][[p]][[g]][[k]], gappyx = GappyData[[d]][[p]][[g]][[k]], custom = custom))
          }
          names(pf[[d]][[m]][[p]]) <- gap_vec_names
        }
        names(pf[[d]][[m]]) <- prop_vec_names
      }
      names(pf[[d]]) <- method_names
    }
    names(pf) <- data_names
  }
  
  pf <- lapply(pf, function(x) 
                    lapply(x, function(x) 
                      lapply(x, function(x)
                        lapply(x, function(x){
                            logic <- unlist(lapply(x,FUN = function(x) !is.null(x)))
                            x <-x[logic]
                      }))))
  
  class(pf) <- "pf"
  return(pf) 
}
