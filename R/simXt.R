#' Simulate X_t
#' 
#' Function to simulate X_t. Output is a list object containing the defining parameters. Access the actual time series using $Xt. 
#' 
#' @param N \code{integer}; The desired length of the time series
#' @param numTrend \code{integer}; The degree of the polynomial in the mean component; all lower order terms are included 
#' @param a \code{numeric}; If specified, a vector of custom coefficients for each term in the polynomial mean component
#' @param mu \code{numeric}; If specified, a single value representing the grand mean of the polynomial mean component
#' @param center \code{numeric}; If specified, a vector of custom centering parameters for each term in the polynomial mean component 
#' @param numFreq \code{integer}; The number of sinusoids to generate in the trend component
#' @param bandwidth \code{integer}; This value is plugged into a negative exponent (base 10) and defines some interval in which to sample frequencies for the trend component. If specified, 'bandwidth' must be at least *-log10(1/*\code{numFreq}*)*. If unspecified, frequencies are distributed *~Uniform(a=0,b=1)*
#' @param b \code{numeric}; If specified, a vector of custom coefficients on each sinusoid in the trend component
#' @param w \code{numeric}; If specified, a vector of custom periods for each sinusoid in the trend component
#' @param p \code{integer}; The \emph{AR} order of the noise component
#' @param q \code{integer}; The \emph{MA order of the noise component
#' @param snr \code{numeric}; The desired signal-to-noise ratio
#' 
#' @examples 
#' # Define four different time series
#' 
#' simData_1 <- simXt(N = 1000, numTrend = 2, numFreq = 10, bandwidth = 2, snr = 1.5)
#' simData_2 <- simXt(N = 1000, numTrend = 4, numFreq = 30, bandwidth = 1, snr = 3)
#' simData_3 <- simXt(N = 1000, numTrend = 3, a = c(1/3, 2/3, 5/3), mu = 5, numFreq = 20, snr = 0.7)
#' simData_4 <- simXt(N = 1000, numTrend = 6, numFreq = 40, p = 1, q = 2)                 
#'                  
#' # Creating list object containing the time series themselves in preparation for simulateGaps()
#'
#'  OriginalData = list(D1 = simData_1$Xt,
#'                      D2 = simData_2$Xt,
#'                      D3 = simData_3$Xt,
#'                      D4 = simData_4$Xt)
#'

simXt <- function(N=1000, 
                  numTrend = 0, a = NULL, mu = NULL, center = NULL, 
                  numFreq = 20, bandwidth = NULL, b = NULL, w = NULL, 
                  p=0, q=0, 
                  snr = 1.5){
   
  
  stopifnot(is.numeric(snr), snr > 0)
  
  # WARNINGS
  
  if(missing(N)){
    warning("N not specified- defaulting to 1000;")
  }
  
  t <- 0:(N-1)
  simList <- list()
  
  Mt <- simMt(N = N, numTrend = numTrend, a = a, mu = mu, center = center)
  Tt <- simTt(N = N, numFreq = numFreq, bandwidth = bandwidth, b = b, w = w)
  Wt <- simWt(N = N, p = p, q = q, var = var(Tt$value)/snr)
  
  simList$Xt <- Mt$value+Tt$value+Wt$value
  simList$Mt <- Mt$value
  simList$Mt_mu <- Mt$mu
  simList$Mt_numTrend <- Mt$numTrend
  simList$Tt <- Tt$value
  simList$Wt <- Wt$value
  simList$Mt_f <- Mt$fn
  simList$Tt_fn <- Tt$fn
  simList$Tt_freq <- Tt$freq
  simList$Tt_bandwidth <- Tt$bandwidth
  simList$Wt_p <- Wt$p
  simList$Wt_q <- Wt$q
  simList$SNR <- var(simList$Tt)/var(simList$Wt) 
  
  class(simList) <- "simList"
  return(simList)
}
