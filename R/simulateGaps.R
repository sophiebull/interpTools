#' Simulate Gappy Data
#' 
#' Function to simulate the MCAR gappy data for a list of complete original time series according to various combinations of gap structure parameters *p* and *g*. and store in a list.  
#' @param OriginalData \code{list}; List object containing the complete original time series vectors (as separate elements)
#' @param prop_vec \code{numeric}; Vector of unique proportions representing the different 'proportion-of-data missing' (*p*) scenarios to simulate
#' @param gap_vec \code{integer}; Vector of unique integer values representing the different 'gap length' scenarios (*g*) to simulate
#' @param K \code{integer}; Number of gappy series to simulate for each (*p,g*) gap specification
#' @examples  
#' 
#' prop_vec = c(0.05,0.10,0.15,0.20)
#' gap_vec = c(1,5,10)
#' K = 10 # number of gappy series to simulate under each p,g specification
#'
#' GappyData <- simulateGaps(OriginalData = OriginalData, prop_vec = prop_vec, gap_vec = gap_vec, K = K)
#' 
#' # dimension (d, p, g, k) 

simulateGaps <- function(OriginalData, prop_vec, gap_vec, K){
  
  if( !(all(prop_vec > 0) && all(prop_vec < 1))) stop("Values in prop_vec must be between 0 and 1.")
  if( !(all(gap_vec %% 1 == 0) | all(gap_vec >=1 ) )) stop("Values in gap_vec must be positive integers.")
  
  stopifnot(is.list(OriginalData),
            is.numeric(prop_vec),
            is.numeric(gap_vec),
            !is.null(prop_vec),
            !is.null(gap_vec),
            !is.null(OriginalData),
            K %% 1 == 0,
            K > 0)
  
  gapList <- list()
  propList <- list()
  dataList <- list()
  samples <- list()
  
  data_vec_names <- paste0("D", 1:length(OriginalData))
  prop_vec_names <- paste0("p", prop_vec)
  gap_vec_names <- paste0("g", gap_vec)
  
  for(d in 1:length(OriginalData)){
    for(p in 1:length(prop_vec)){  
      for (g in 1:length(gap_vec)){
        for(k in 1:K){   
          samples[[k]] <- as.numeric(gaps(OriginalData[[d]], prop_missing = prop_vec[p], gap_width = gap_vec[g]))
        }
        gapList[[g]] <- samples
      }
      names(gapList) <- gap_vec_names
      propList[[p]] <- gapList
    }
    names(propList) <- prop_vec_names
    dataList[[d]] <- propList
  }
  names(dataList) <- data_vec_names
  
  return(dataList)
}
