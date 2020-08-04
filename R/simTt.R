#' Simulate T_t
#' 
#' Function to simulate T_t: the trend component of X_t.
#' @param N The length of the output series
#' @param bandwidth Value is plugged into a negative exponent (base 10) and defines some interval in which to sample frequencies. If specified, 'bandwidth' must be at least -log10(1/numFreq). If unspecified, freq =  runif(numFreq,0,1)
#' @param numFreq The number of sinusoids to generate

simTt <- function(N=1000, numFreq = 20, bandwidth = NULL){  
  
  if( !is.numeric(numFreq)) stop('numFreq must be numeric.')
  if((!is.numeric(bandwidth) && !is.null(bandwidth))) stop('bandwidth must be NULL or numeric.')
  if( any(numFreq < 0 | numFreq%%2 != 0 | numFreq%%1 != 0) ) stop('Please specify an even integer for numFreq')
  if( !(is.null(bandwidth)) &&  bandwidth>=1 && numFreq > 1/(10^-bandwidth) ) stop('Bandwidth must be at least -log10(1/numFreq)')
  if( !(is.numeric(N)) || N%%1 != 0) stop ('N must be a positive integer')
  
  stopifnot(N%%1 == 0, N >= 10, numFreq%%2 == 0, numFreq > 0, (is.null(bandwidth) || (bandwidth >= 1 && (numFreq <= 1/(10^-bandwidth)))),
            (is.null(bandwidth) | is.numeric(bandwidth)))
  
  Tt_list <- list()
  t <- 0:(N-1)
  fourierFreq <- 2*pi/N
  
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
  
  Tt <- numeric(length(w))
  
  #if(!ampMod){
    for(f in 1:(length(w)-1)){
      a <- rnorm(1, mean = 0, sd = N/200) #95% runs from -N/100 to N/100
      Tt[f] <- paste("(",a,")*sin(",w[f],"*t)+",sep="")
    }
    Tt[length(w)] <- paste("(",a,")*sin(",w[length(w)],"*t)",sep="")
  #}
  
  Tt_fn <- paste(Tt,collapse="")  
  Tt <- eval(parse(text = paste(Tt,collapse = "")))
  
  Tt_list$fn <- Tt_fn
  Tt_list$value <- Tt
  Tt_list$freq <- w/(2*pi)
  Tt_list$bandwidth <- bandwidth
  return(Tt_list)
}
