#' Simulate T_t
#' 
#' Function to simulate T_t: the trend component of X_t.
#' @param N \code{integer}; The length of the output series
#' @param numFreq \code{integer}; The number of sinusoids to generate
#' @param bandwidth \code{integer}; Value is plugged into a negative exponent (base 10) and defines some interval in which to sample frequencies. If specified, 'bandwidth' must be at least -log10(1/numFreq). If unspecified, freq =  runif(numFreq,0,1)
#' @param b \code{numeric}; If specified, a vector of custom coefficients on each sinusoid
#' @param w \code{numeric}; If specified, a vector of custom periods for each sinusoid


simTt <- function(N=1000, numFreq = 20, bandwidth = NULL, b = NULL, w = NULL){  
  
  if( !is.numeric(numFreq)) stop('numFreq must be numeric.')
  if((!is.numeric(bandwidth) && !is.null(bandwidth))) stop('bandwidth must be NULL or numeric.')
  if( any(numFreq < 0 | numFreq%%2 != 0 | numFreq%%1 != 0) ) stop('Please specify an even integer for numFreq')
  if( !(is.null(bandwidth)) &&  bandwidth>=1 && numFreq > 1/(10^-bandwidth) ) stop('Bandwidth must be at least -log10(1/numFreq)')
  if( !(is.numeric(N)) || N%%1 != 0) stop ('N must be a positive integer')
  
  
  if(any(is.null(b), is.null(w))){
    nulls <- c(b = is.null(b), w = is.null(w))
    warning(c("Value(s): '", paste0(names(nulls)[which(nulls)], sep = ", "), "' unspecified. Defaulting to random generation."))
  }
  
  stopifnot(N%%1 == 0, N >= 10, numFreq%%2 == 0, numFreq > 0, (is.null(bandwidth) || (bandwidth >= 1 && (numFreq <= 1/(10^-bandwidth)))),
            (is.null(bandwidth) | is.numeric(bandwidth)))
  
  Tt_list <- list()
  t <- 0:(N-1)
  fourierFreq <- 2*pi/N
  
  ########################
  # SINUSOID COEFFICIENTS
  ########################
  
  if(is.null(b)){
    b <- rnorm(numFreq, mean = 0, sd = N/200) #95% runs from -N/100 to N/100
    warning(c("Sinusoid coefficient(s) = ", paste0(round(b,3), sep = " ")))
  }
  
  else if(!is.null(b)){
    if(length(b) != numFreq){
      
      missing <- rep(NA, numFreq)
      missing[1:length(b)] <- b
      
      stop(c("Sinusoid coefficient(s): ", paste0("b_", which(is.na(missing)), sep = ", ") , "not specified. 
             Length of 'b' must be equal to the number of sinusoids (numFreq = ", paste0(numFreq, ").")))
    }
  }
  
  #######################
  # SINUSOID PERIODS
  #######################
  
  if(!is.null(w)){
    
    if(!is.null(bandwidth)){
      warning("Ignoring 'bandwidth' parameter since 'w' is already specified.")
    }
    
    if(length(w) != numFreq){
      
      missing <- rep(NA, numFreq)
      missing[1:length(w)] <- w
      
      stop(c("Sinusoid period(s): ", paste0("w_", which(is.na(missing)), sep = ", ") , "not specified. 
             Length of 'w' must be equal to the number of sinusoids (numFreq = ", paste0(numFreq, ").")))
    }
  }
  
  if(is.null(w)){
    if(!is.null(bandwidth)){ # specified
      # choose random midpoints
      repeat{
        m <- runif(numFreq/2, fourierFreq+((10^-bandwidth)/2), pi-((10^-bandwidth)/2))
        m <- m[order(m, decreasing = FALSE)]
        check <- abs(diff(m)) >= 10^-bandwidth
        if(!(FALSE %in% check)){
          break
        }
      } ### BE CAREFUL WITH REPEAT, THIS WILL ENDLESSLY RUN IF BANDWIDTH AND NUMFREQ ARE NOT COMPATIBLE
      
      # create non-overlapping frequency bands around m with specified bandwidth
      bands <- c(m-((10^-bandwidth)/2),m+((10^-bandwidth)/2))
      bands <- bands[order(bands, decreasing = FALSE)]
      
      w <- numeric(numFreq)
      
      for(i in 1:(numFreq/2)){
        w[((2*i)-1):(2*i)] <- sample(seq(bands[(2*i)-1],bands[2*i],length.out=1/(10^-bandwidth)), size = 2, replace = FALSE)
      } 
    }
    
    else if(is.null(bandwidth)){ # unspecified
      w <- runif(numFreq, fourierFreq, pi) 
    }
    warning(c("Sinusoid period(s) = ", paste0(round(w,3), sep = " ")))
  }
  
  Tt <- paste("(",b,")*sin(", w,"*t)+", sep = "", collapse = "")
  Tt <- gsub(".{1}$", "", Tt)
  
  Tt_fn <- paste(Tt,collapse="")  
  Tt <- eval(parse(text = paste(Tt,collapse = "")))
  
  Tt_list$fn <- Tt_fn
  Tt_list$value <- Tt
  Tt_list$freq <- w/(2*pi)
  Tt_list$bandwidth <- bandwidth
  return(Tt_list)
}
