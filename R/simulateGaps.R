#' Simulate Gappy Data
#' 
#' Function to simulate the MCAR gappy data for a list of complete original time series according to various combinations of gap structure parameters *p* and *g*. and store in a list.  
#' @param OriginalData \code{list}; List object containing the complete original time series vectors (as separate elements)
#' @param p \code{numeric}; Vector of unique proportions representing the different 'proportion-of-data missing' (*p*) scenarios to simulate
#' @param g \code{integer}; Vector of unique integer values representing the different 'gap length' scenarios (*g*) to simulate
#' @param K \code{integer}; Number of gappy series to simulate for each (*p,g*) gap specification
#' @examples  
#' 
#' # Subroutine removes p*n points where the number of consecutive missing
#' # observations (gaps) is a multiple of gap_width. For example, in a time
#' # series of length n = 100:
#' 
#'  p = 0.30, g = 2
#' 
#' # will generate 30 missing points, with <15 holes.
#' 
#' # Usage in simulateGaps():
#' 
#' p = c(0.05,0.10,0.15,0.20)
#' g = c(1,5,10)
#' K = 10 # number of gappy series to simulate under each p,g specification
#'
#' GappyData <- simulateGaps(OriginalData = OriginalData, p = p, g = g, K = K)
#' 
#' # dimension (d, p, g, k) 
#' 



simulateGaps <- function(OriginalData, p, g, K){
  
  if( !(all(p > 0) && all(p < 1))) stop("Values in p must be between 0 and 1.")
  if( !(all(g %% 1 == 0) | all(g >=1 ) )) stop("Values in g must be positive integers.")
  
  stopifnot(is.list(OriginalData),
            is.numeric(p),
            is.numeric(g),
            !is.null(p),
            !is.null(g),
            !is.null(OriginalData),
            K %% 1 == 0,
            K > 0)
  
  
  ##########################
  # SUBROUTINE: CREATE GAPS
  ##########################
  
  .gaps <- function(x, prop_missing, gap_width){
    
    n <- length(x)
    
    stopifnot(sum(is.na(x)) == 0,
              is.numeric(x), 
              is.numeric(prop_missing), 
              is.numeric(gap_width),
              gap_width %% 1 == 0,
              length(x) > 2, 
              prop_missing >= 0 & prop_missing <= (n-2)/n,
              gap_width >=0,
              prop_missing*gap_width < length(x)-2) 
    
    poss_values <- 2:(n-1)
    
    if ((prop_missing * n / gap_width) %% 1 != 0) {
      warning(paste("Rounded to the nearest integer multiple; removed ", round(prop_missing*n/gap_width,0)*gap_width, " observations", sep =""))
    }
    
    if((prop_missing * n / gap_width) %% 1 <= 0.5 & (prop_missing * n / gap_width) %% 1 != 0) {
      end_while <- floor(prop_missing * n) - gap_width
    } else {
      end_while <- floor(prop_missing * n)
    }
    num_missing <- 0
    while(num_missing < end_while) {
      hi <- sample(1:(length(poss_values)-gap_width + 1), 1)
      poss_values <- poss_values[-(hi:(hi + gap_width -1))]
      num_missing <- num_missing + gap_width
    }
    
    x.gaps <- x
    if (length(poss_values) == 0) {
      x.gaps[2:(n-1)] <- NA
    } else {
      x.gaps[-poss_values] <- NA
    }
    x.gaps[1] <- x[1]
    x.gaps[n] <- x[n]
    
    return(x.gaps)
  }
  

  ############################
  # Initialization
  ############################
  
  gapList <- list()
  propList <- list()
  GappyData <- list()
  samples <- list()
  
  data_vec_names <- names(OriginalData)
  prop_vec_names <- paste0("p", p)
  gap_vec_names <- paste0("g", g)
  
  
  
  ####################################
  # Generate gappy time series in list
  ###################################
  
  for(d in 1:length(OriginalData)){
    for(vp in 1:length(p)){  
      for (vg in 1:length(g)){
        for(k in 1:K){   
          samples[[k]] <- as.numeric(.gaps(x = as.numeric(OriginalData[[d]]), prop_missing = p[vp], gap_width = g[vg]))
        }
        gapList[[vg]] <- samples
      }
      names(gapList) <- gap_vec_names
      propList[[vp]] <- gapList
    }
    names(propList) <- prop_vec_names
    GappyData[[d]] <- propList
  }
  names(GappyData) <- data_vec_names
  
  return(GappyData)
}
