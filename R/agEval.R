#' Aggregate the Performance Matrices of Multiple Interpolations
#' 
#' Function to 'condense' the performance matrices of the interpolations from each method (M) across K simulations in each (P,G) missingness specification 
#' @param pmats The performance matrices (result of performance.R)
#' @param task Type of aggregation (mean, quantile distribution, etc.)

agEval <- function(pmats){
  
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
  
  D <- length(pmats)
  M <- length(pmats[[1]])
  P <- length(pmats[[1]][[1]])
  G <- length(pmats[[1]][[1]][[1]])
  
  dataset <- 1:D
  
  # Initializing nested list object
  
  Evaluation <- lapply(Evaluation <- vector(mode = 'list', D),function(x)
    lapply(Evaluation <- vector(mode = 'list', P),function(x) 
      lapply(Evaluation <- vector(mode = 'list', G),function(x) 
        x<-vector(mode='list',M))))
  
  prop_vec_names <- numeric(P)
  gap_vec_names <- numeric(G)
  method_names <- numeric(M)
  
  for(d in 1:D){
    for(p in 1:P){
      prop_vec_names[p] <- c(paste("p", prop_vec[p],sep="")) # vector of names
      for(g in 1:G){
        gap_vec_names[g] <- c(paste("g", gap_vec[g],sep="")) # vector of names
        for(m in 1:M){
          method_names[m] <- algorithm_names[methods[m]]
          
            # compute the mean and distribution of the performance criteria in each (d,m,p,g) specification across all k pairs of (x,X) and 
            # store results in a list of data frames
          
            Evaluation[[d]][[p]][[g]][[m]] <- data.frame(
              
              mean = rowMeans(sapply(pmats[[d]][[m]][[p]][[g]],unlist)),
              sd = apply(sapply(pmats[[d]][[m]][[p]][[g]],unlist),1,sd),
              q0 = apply(sapply(pmats[[d]][[m]][[p]][[g]],unlist),1,quantile)["0%",],
              q25 = apply(sapply(pmats[[d]][[m]][[p]][[g]],unlist),1,quantile)["25%",],
              q50 = apply(sapply(pmats[[d]][[m]][[p]][[g]],unlist),1,quantile)["50%",],
              q75 = apply(sapply(pmats[[d]][[m]][[p]][[g]],unlist),1,quantile)["75%",],
              q100 = apply(sapply(pmats[[d]][[m]][[p]][[g]],unlist),1,quantile)["100%",],
              
              gap_width = c(rep(gap_vec[g], 17)),
              prop_missing = c(rep(prop_vec[p],17)),
              dataset = c(rep(dataset[d],17)), 
              method = rep(algorithm_names[methods[m]],17) 
            )  
      
          
        }
        names(Evaluation[[d]][[p]][[g]]) <- method_names 
      }
      names(Evaluation[[d]][[p]]) <- gap_vec_names 
    }
    names(Evaluation[[d]]) <- prop_vec_names 
  }
  names(Evaluation) <- names(OriginalData)
  
  return(Evaluation)
} 