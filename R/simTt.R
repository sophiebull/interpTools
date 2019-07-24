#### simTt.R ####
# Function to simulate T_t: the trend component of X_t
# class(numFreq) == numeric; How many different frequencies to include (sinusoids)
# class(freqRes) == numeric; How closely to separate the frequencies. Value is assigned as a negative
#                             exponent in some region (+/- freqRes) around a randomly selected number 0<m<1
# class(ampMod) == logical; Whether to include amplitude modulation or not.  Amplitude is randomly sampled -n/10 < a < n/10

simTt <- function(n=1000, numFreq = 20, freqRes = NULL#, ampMod = FALSE
                  ){
  stopifnot(n%%1 == 0, n > 0, numFreq%%1 == 0, numFreq > 0, freqRes > 0#,#is.logical(ampMod)
            )
  
  Tt_list <- list()
  t <- 0:(n-1)
  
  m <- runif(numFreq/2,0+1^(-freqRes),1-1^(-freqRes))
  
  if(!is.null(freqRes)){ # specified
    m <- runif(1,0+2^(-freqRes),1-2^(-freqRes))
    freq <- runif(numFreq, m-2^(-freqRes), m+2^(-freqRes))
  } 
  
  else if(is.null(freqRes)){ # unspecified
    freq <- runif(numFreq,0,1) 
  }
  
  Tt <- numeric(length(freq))
  
  #if(!ampMod){
    for(f in 1:(length(freq)-1)){
      a <- sample(-(n/10):(n/10),1)
      Tt[f] <- paste("(",a,")*sin(",freq[f],"*t)+",sep="")
    }
    Tt[length(freq)] <- paste("(",a,")*sin(",freq[length(freq)],"*t)",sep="")
  #}
  
  Tt_fn <- paste(Tt,collapse="")  
  Tt <- eval(parse(text = paste(Tt,collapse = "")))
  
  Tt_list$fn <- Tt_fn
  Tt_list$value <- Tt
  return(Tt_list)
}
