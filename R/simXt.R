#' Simulate X_t
#' 
#' Function to simulate X_t. The component that is selected to vary will hold all required variables constant: \cr\cr
#' For vary = "Mt", only 'numTrend' is subject to vary. \cr
#' For vary = "Tt", only 'numFreq' is subject to vary. \cr
#' For vary = "Wt", order variables 'p' or 'q' are subject to vary depending on the value of 'fix', which must also be specified.
#' 
#' @param D The number of datasets to generate.
#' @param n The desired length of each complete time series.
#' @param vary "Mt", "Tt", "Wt". A character string specifying which component of Xt to vary. Other components are held constant.
#' @param numTrend If 'vary != Mt', this is the number of terms to include. Value represents the maximum degree of the mean component; all lower order terms included. 
#' @param trendType If 'vary != Mt', this is the type of trends to include in the mean component.
#' @param numFreq If 'vary != Tt', this is the number of sinusoids to generate in the trend component.
#' @param bandwidth If 'vary != Tt', this value is plugged into a negative exponent (base 10) and defines some interval in which to sample frequencies for the trend component. If specified, 'bandwidth' must be at least -log10(1/numFreq). If unspecified, frequencies are distributed ~Uniform(a=0,b=1).
#' @param p If 'fix != p', this is the AR order of the noise component.
#' @param q If 'fix != q', this is the MA order of the noise component.
#' @param fix Which order variable in ARMA(p,q) to fix if 'vary = Wt' (must be either "p" or "q").
#' @param snr Specify the signal-to-noise ratio.
#' 
#' @examples 
#' # Initializing basic parameters
#' D = 5
#' n=1000
#' t=0:(n-1)
#' 
#' # Vary Mt
#' simData <- simXt(D=D, n=n, vary = "Mt", bandwidth = 3, numFreq = 30, 
#'                  trendType = "polynomial", snr = 1.5)
#' 
#' # Vary Tt
#' simData <- simXt(D=D, n=n, vary = "Tt", bandwidth = 3, numTrend = 0, 
#'                  trendType = "polynomial", snr = 1.5)
#' 
#' # Vary Wt, p fixed
#' simData <- simXt(D=D, n=n, vary = "Wt", bandwidth = 3, numFreq = 20, 
#'                  numTrend = 0, trendType = "polynomial", p = 0, fix = "p", snr = 1.5)
#' 
#' # Vary Wt, q fixed
#' simData <- simXt(D=D, n=n, vary = "Wt", bandwidth = 3, numFreq = 20, 
#'                  numTrend = 10, trendType = "polynomial", q = 0, fix = "q", snr = 1.5)
#' 
#' # Creating list object for one of the above variations
#'sets <- numeric(D)
#'for(d in 1:(D-1)){
#'  sets[d] <- paste("D",d,"=simData$Xt[[",d,"]],",sep="")
#'}
#'sets[D] <- paste("D",D,"=simData$Xt[[",D,"]]",sep="")
#'list_call <- paste("list(",paste(sets,collapse=""),")")
#'
#'OriginalData = eval(parse(text=list_call))
#'

simXt <- function(D, n=1000, vary = "all", numTrend = 0, trendType = "polynomial", numFreq = 20, bandwidth = NULL, p=0, q=0, fix, snr = 1.5){
  
  # STOPS 
  
  if(vary == "Wt" && missing(fix)){
    stop("Variable 'fix' must be specified if Wt is to vary.")
  }
  
  stopifnot(is.numeric(D), D>0,
            (vary== "all" || vary == "Mt" || vary == "Tt"|| (vary == "Wt" && !(missing(fix)))),
            is.numeric(snr), snr > 0)
  
  # WARNINGS
  
  if(missing(n)){
    warning("n not specified- defaulting to 1000;")
  }
  
  if(missing(bandwidth)){
    warning("bandwidth not specified- frequencies will be selected randomly from Uniform(a=0,b=1);")
  }
  
  if(missing(trendType)){
    warning("trendType not specified- defaulting to 'polynomial';")
  }
  
  t <- 0:(n-1)
  simList <- list()
  
  if(vary == "Mt"){
  
    if(!(missing(numTrend))){
      warning("numTrend cannot be assigned a constant value if Mt is to vary- value will be replaced with d in 0:(D-1);")
    }

    if(missing(numFreq)){
      warning("numFreq not specified- defaulting to 20;")
    }

    Tt <- simTt(n=n, numFreq=numFreq, bandwidth=bandwidth)
    Wt <- simWt(n=n, p=p, q=q, var = var(Tt$value)/snr)
    
    for(d in 0:(D-1)){
      Mt <- simMt(n=n, numTrend = d, trendType = trendType)
      simList$Xt[[(d+1)]] <- Mt$value+Tt$value+Wt$value
      simList$Mt[[(d+1)]] <- Mt$value
      simList$Mt_mu[[(d+1)]] <- Mt$mu
      simList$Mt_numTrend[[(d+1)]] <- Mt$numTrend
      simList$Tt[[(d+1)]] <- Tt$value
      simList$Wt[[(d+1)]] <- Wt$value
      simList$Mt_fn[[(d+1)]] <- Mt$fn
      simList$Tt_fn[[(d+1)]] <- Tt$fn
      simList$Tt_freq[[(d+1)]] <- Tt$freq
      simList$Tt_bandwidth[[(d+1)]] <- Tt$bandwidth
      simList$Wt_p[[(d+1)]] <- Wt$p
      simList$Wt_q[[(d+1)]] <- Wt$q
      simList$SNR[[(d+1)]] <- var(simList$Tt[[(d+1)]])/var(simList$Wt[[(d+1)]]) 
    }
  }
  
  else if(vary == "Tt"){
    
    if(!(missing(numFreq))){
      warning("numFreq cannot be assigned a constant value if Tt is to vary- value will be replaced with d*10 in 1:D;")
    }
    if(missing(numTrend)){
      warning("numTrend not specified- defaulting to 0;")
    }
    
    Mt <- simMt(n=n, numTrend = numTrend)
    #Wt <- simWt(n=n)  IF Tt IS TO VARY, THEN Wt VARIES AS WELL, TO MAINTAIN SNR
    
    for(d in 0:(D-1)){
      Tt <- simTt(n=n, numFreq = (d+1)*10, bandwidth = bandwidth)
      Wt <- simWt(n=n, var = var(Tt$value)/snr) #P AND Q PARAMETERS DO NOT CHANGE; JUST THE VARIANCE OF Wt
      
      simList$Xt[[(d+1)]] <- Mt$value+Tt$value+Wt$value
      simList$Mt[[(d+1)]] <- Mt$value
      simList$Mt_mu[[(d+1)]] <- Mt$mu
      simList$Mt_numTrend[[(d+1)]] <- Mt$numTrend
      simList$Wt[[(d+1)]] <- Wt$value
      simList$Tt[[(d+1)]] <- Tt$value
      simList$Mt_fn[[(d+1)]] <- Mt$fn
      simList$Tt_fn[[(d+1)]] <- Tt$fn
      simList$Tt_freq[[(d+1)]] <- Tt$freq
      simList$Tt_bandwidth[[(d+1)]] <- Tt$bandwidth
      simList$Wt_p[[(d+1)]] <- Wt$p
      simList$Wt_q[[(d+1)]] <- Wt$q
      simList$SNR[[(d+1)]] <- var(simList$Tt[[(d+1)]])/var(simList$Wt[[(d+1)]]) 
    }
  }
  
  else if(vary == "Wt"){
    
    if(fix == "q"){
    if(!(missing(p))){
      warning("p cannot be assigned a constant value if Wt is to vary and q is already fixed- value will be replaced with d in 0:(D-1);")
    }
    if(missing(numFreq)){
      warning("numFreq not specified- defaulting to 20;")
    }
    if(missing(numTrend)){
      warning("numTrend not specified- defaulting to 0;")
    }
      
      Mt <- simMt(n=n, numTrend = numTrend)
      Tt <- simTt(n=n, numFreq = numFreq, bandwidth = bandwidth)
      
      for(d in 0:(D-1)){
        
        Wt <- simWt(n=n,q=q,p=d, var = var(Tt$value)/snr)
        simList$Xt[[(d+1)]] <- Mt$value+Tt$value+Wt$value
        simList$Mt[[(d+1)]] <- Mt$value
        simList$Mt_mu[[(d+1)]] <- Mt$mu
        simList$Mt_numTrend[[(d+1)]] <- Mt$numTrend
        simList$Wt[[(d+1)]] <- Wt$value
        simList$Tt[[(d+1)]] <- Tt$value
        simList$Mt_fn[[(d+1)]] <- Mt$fn
        simList$Tt_fn[[(d+1)]] <- Tt$fn
        simList$Tt_freq[[(d+1)]] <- Tt$freq
        simList$Tt_bandwidth[[(d+1)]] <- Tt$bandwidth
        simList$Wt_p[[(d+1)]] <- Wt$p
        simList$Wt_q[[(d+1)]] <- Wt$q
        simList$SNR[[(d+1)]] <- var(simList$Tt[[(d+1)]])/var(simList$Wt[[(d+1)]]) 
      }
    }
    
    if(fix == "p"){
    if(!(missing(q))){
      warning("q cannot be assigned a constant value if Wt is to vary and p is already fixed- value will be replaced with d in 0:(D-1);")
    }
      Mt <- simMt(n=n, numTrend = numTrend)
      Tt <- simTt(n=n, numFreq = numFreq, bandwidth = bandwidth)
      
      for(d in 0:(D-1)){
        
        Wt <- simWt(n=n,q=d,p=p, var = var(Tt$value)/snr)
        simList$Xt[[(d+1)]] <- Mt$value+Tt$value+Wt$value
        simList$Mt[[(d+1)]] <- Mt$value
        simList$Mt_mu[[(d+1)]] <- Mt$mu
        simList$Mt_numTrend[[(d+1)]] <- Mt$numTrend
        simList$Wt[[(d+1)]] <- Wt$value
        simList$Tt[[(d+1)]] <- Tt$value
        simList$Mt_fn[[(d+1)]] <- Mt$fn
        simList$Tt_fn[[(d+1)]] <- Tt$fn
        simList$Tt_freq[[(d+1)]] <- Tt$freq
        simList$Tt_bandwidth[[(d+1)]] <- Tt$bandwidth
        simList$Wt_p[[(d+1)]] <- Wt$p
        simList$Wt_q[[(d+1)]] <- Wt$q
        simList$SNR[[(d+1)]] <- var(simList$Tt[[(d+1)]])/var(simList$Wt[[(d+1)]]) 
        }
      }
    }
  
  else if(vary == "all"){
    
    if(!(missing(numTrend))){
      warning("numTrend cannot be assigned a constant value if Mt is to vary- value will be replaced with d in 0:(D-1);")
    }
    
    if(!(missing(numFreq))){
      warning("numFreq cannot be assigned a constant value if Tt is to vary- value will be replaced with d*10 in 1:D;")
    }
    
    for(d in 0:(D-1)){
      Mt <- simMt(n=n, numTrend = d, trendType = trendType)
      Tt <- simTt(n=n, numFreq = (d+1)*10, bandwidth = bandwidth)
      Wt <- simWt(n=n, p=p, q=q, var = var(Tt$value)/snr)
      
      simList$Xt[[(d+1)]] <- Mt$value+Tt$value+Wt$value
      simList$Mt[[(d+1)]] <- Mt$value
      simList$Mt_mu[[(d+1)]] <- Mt$mu
      simList$Mt_numTrend[[(d+1)]] <- Mt$numTrend
      simList$Wt[[(d+1)]] <- Wt$value
      simList$Tt[[(d+1)]] <- Tt$value
      simList$Mt_fn[[(d+1)]] <- Mt$fn
      simList$Tt_fn[[(d+1)]] <- Tt$fn
      simList$Tt_freq[[(d+1)]] <- Tt$freq
      simList$Tt_bandwidth[[(d+1)]] <- Tt$bandwidth
      simList$Wt_p[[(d+1)]] <- Wt$p
      simList$Wt_q[[(d+1)]] <- Wt$q
      simList$SNR[[(d+1)]] <- var(simList$Tt[[(d+1)]])/var(simList$Wt[[(d+1)]]) 
    }
    
  }
  
  return(simList)
}
