#' Generate Gaps in a Vector
#' 
#' Subroutine of \code{simulateGaps()}. Function to generate gaps of a specified length constrained by proportion of missingness.
#' @param x \code{numeric}; The complete time series vector from which to remove values
#' @param prop_missing \code{numeric}; Proportion of observations to remove, P
#' @param gap_width \code{integer}; Length of gap (how many successive observations to remove), G
#' @examples  
#' n = 100
#' 
#' # Remove prop_missing*n points where the number of consecutive missing
#' # observations (gaps) is a multiple of gap_width
#' 
#' gappyx <- gaps(rnorm(n), prop_missing = 0.30, gap_width = 2)
#' 
#' # will generate 30 missing points, with <15 holes
#' 

gaps <- function(x, prop_missing, gap_width){
  
  n <- length(x)
  
  stopifnot(sum(is.na(x) == 0),
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
