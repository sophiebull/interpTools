#' Evaluate Interpolation Performance Across Multiple Time Series
#' 
#' Function to calculate the performance metrics between lists of original and interpolated series.
#' @param OriginalData A list object of dimension Dxn of original (complete) time series 
#' @param IntData A list object of dimension DxMxPxGxKxN of interpolated time series (output of parInterpolate.R)
#' @param GappyData A list object of dimension DxPxGxKxN of the gappy original time series (output of simulateGaps.R)
#' 
performance <- function(OriginalData,IntData,GappyData){
  algorithm_names <- c("Nearest.Neighbor",
                       "Linear.Interpolation", 
                       "Natural.Cubic.Spline",
                       "FMM Cubic.Spline", 
                       "Hermite.Cubic.Spline",
                       "Stineman.Interpolation",
                       "Kalman.ARIMA",
                       "Kalman.StructTS", 
                       "Last.Observation.Carried.Forward",
                       "Next.Observation.Carried.Backward", 
                       "Simple.Moving.Average", 
                       "Linear.Weighted.Moving.Average",
                       "Exponential.Weighted.Moving.Average",
                       "Replace.with.Mean",
                       "Replace.with.Median", 
                       "Replace.with.Mode",
                       "Replace.with.Random",
                       "Hybrid.Wiener.Interpolator")
  
  D <- length(IntData)
  M <- length(IntData[[1]])
  P <- length(IntData[[1]][[1]])
  G <- length(IntData[[1]][[1]][[1]])
  K <- length(IntData[[1]][[1]][[1]][[1]])
  
  # Initializing nested list object
  Performance <- lapply(Performance <- vector(mode = 'list', D),function(x)
    lapply(Performance <- vector(mode = 'list', M),function(x) 
      lapply(Performance <- vector(mode = 'list', P),function(x) 
        lapply(Performance <- vector(mode = 'list', G),function(x)
          x<-vector(mode='list', K)))))
  
  prop_vec_names <- numeric(P)
  gap_vec_names <- numeric(G)
  method_names <- numeric(M)
  
  # Evaluate the performance criteria for each sample in each (d,m,p,g) specification
  for(d in 1:D){
    for(m in 1:M){
      method_names[m] <- algorithm_names[methods[m]]
      for(p in 1:P){
        prop_vec_names[p] <- c(paste("p", prop_vec[p],sep="")) # vector of names
        for(g in 1:G){
          gap_vec_names[g] <- c(paste("g", gap_vec[g],sep="")) # vector of names
          for(k in 1:K) { 
            Performance[[d]][[m]][[p]][[g]][[k]] <- unlist(eval_performance(x = OriginalData[[d]], X = IntData[[d]][[m]][[p]][[g]][[k]], gappyx = GappyData[[d]][[p]][[g]][[k]]))
          }
          names(Performance[[d]][[m]][[p]]) <- gap_vec_names
        }
        names(Performance[[d]][[m]]) <- prop_vec_names
      }
      names(Performance[[d]]) <- method_names
    }
    names(Performance) <- names(IntData)
  }
  return(Performance) 
}
