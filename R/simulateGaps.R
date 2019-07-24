#### simulateGaps.R ####
### Function to simulate the gappy data for a single dataset and store in a list
### class(data) == numeric; Original time series 
### class(prop_vec) == numeric; Vector of proportion missings
### class(gap_vec) == numeric; Vector of gap widths
### K = number of gappy series to simulate for each gap width and proportion missing specification

simulateGaps <- function(data, prop_vec, gap_vec, K){
  
  stopifnot(is.vector(data),
            is.numeric(data),
            is.vector(prop_vec), 
            is.numeric(prop_vec),
            is.vector(gap_vec),
            is.numeric(gap_vec),
            is.null(prop_vec) == FALSE,
            is.null(gap_vec) == FALSE,
            is.null(data) == FALSE,
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