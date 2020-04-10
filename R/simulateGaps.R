#' Simulate Gappy Data
#' 
#' Function to simulate the gappy data for a single dataset and store in a list. Function requires gaps.R
#' @param data The original time series vector
#' @param prop_vec Vector of missingness proportions
#' @param gap_vec Vector of gap lengths
#' @param K Number of gappy series to simulate for each gap width and proportion missing specification
#' @examples  
#' prop_vec = c(0.05,0.10,0.15,0.20)
#' gap_vec = c(1,5,10)
#' K = 10 # number of gappy series to simulate under each p,g specification
#'
#' GappyData <- list()
#'
#' for(d in 1:length(OriginalData)){
#'   GappyData[[d]] <- simulateGaps(data = as.numeric(OriginalData[[d]]), prop_vec = prop_vec, gap_vec = gap_vec, K = K)
#' }
#' names(GappyData) <- names(OriginalData)

#'# dimension (d, p, g, k) 

simulateGaps <- function(data, prop_vec, gap_vec, K){
  
  if( !(all(prop_vec > 0) && all(prop_vec < 1))) stop("Values in prop_vec must be between 0 and 1.")
  if( !(all(gap_vec %% 1 == 0) | all(gap_vec >=1 ) )) stop("Values in gap_vec must be positive integers.")
  
  stopifnot(is.vector(data),
            is.numeric(data),
            is.numeric(prop_vec),
            is.numeric(gap_vec),
            !is.null(prop_vec),
            !is.null(gap_vec),
            !is.null(data),
            K %% 1 == 0,
            K > 0)
  
  gapList <- list()
  propList <- list()
  samples <- list()
  
  prop_vec_names <- numeric(length(prop_vec))
  gap_vec_names <- numeric(length(gap_vec))
  
  for(p in 1:length(prop_vec)){  
    prop_vec_names[p] <- c(paste("p", prop_vec[p],sep="")) # vector of names
    for (g in 1:length(gap_vec)){
      gap_vec_names[g] <- c(paste("g", gap_vec[g],sep="")) # vector of names
      for(k in 1:K){   
        samples[[k]] <- as.ts(gaps(data, prop_missing = prop_vec[p], gap_width = gap_vec[g]))
      }
      gapList[[g]] <- samples
      
    }
    names(gapList) <- gap_vec_names
    propList[[p]] <- gapList
    
  }
  names(propList) <- prop_vec_names
  return(propList)
}
