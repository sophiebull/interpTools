#' Aggregate the Performance Matrices of Multiple Interpolations
#' 
#' Function to 'condense' the performance matrices of the interpolations from each method (M) across K simulations in each (P,G) missingness specification 
#' @param pmats The performance matrices (result of performance.R)
#' @param hist logical. TRUE returns a list of histograms of criteria, FALSE returns matrix of aggregated metrics

agEvaluate <- function(pmats, hist = F){
  skew <- function(x){
    stopifnot(is.numeric(x))
    
    sk <- (sum((x-mean(x))^3)/(length(x)*sd(x)^3))
    return(sk)
  }
  
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
          
          # Generate histograms in each subset
          if(hist){
            sqrtC = sqrt(length(pmats[[1]][[1]][[1]][[1]][[1]]))
            par(mfrow=c(floor(sqrtC),ceiling(sqrtC)))
            Evaluation[[d]][[p]][[g]][[m]] <- apply(sapply(pmats[[d]][[m]][[p]][[g]],unlist),1,hist)
          }
          
          
            # compute the mean and distribution of the performance criteria in each (d,m,p,g) specification across all k pairs of (x,X) and 
            # store results in a list of data frames
          else if(!hist){
            Evaluation[[d]][[p]][[g]][[m]] <- data.frame(
              
              mean = rowMeans(sapply(pmats[[d]][[m]][[p]][[g]],unlist)),
              sd = apply(sapply(pmats[[d]][[m]][[p]][[g]],unlist),1,sd),
              q0 = apply(sapply(pmats[[d]][[m]][[p]][[g]],unlist),1,quantile)["0%",],
              q2.5 = apply(sapply(pmats[[d]][[m]][[p]][[g]],unlist),1, FUN=function(x) quantile(x, probs = c(0.025,0.975)))["2.5%",],
              q25 = apply(sapply(pmats[[d]][[m]][[p]][[g]],unlist),1,quantile)["25%",],
              median = apply(sapply(pmats[[d]][[m]][[p]][[g]],unlist),1,median),
              q75 = apply(sapply(pmats[[d]][[m]][[p]][[g]],unlist),1,quantile)["75%",],
              q97.5 = apply(sapply(pmats[[d]][[m]][[p]][[g]],unlist),1, FUN=function(x) quantile(x, probs = c(0.025,0.975)))["97.5%",],
              q100 = apply(sapply(pmats[[d]][[m]][[p]][[g]],unlist),1,quantile)["100%",],
              skewness = apply(sapply(pmats[[d]][[m]][[p]][[g]],unlist),1,skew), 
              dip = apply(sapply(pmats[[d]][[m]][[p]][[g]],unlist),1,FUN = function(x){
                dip.test(x,simulate.p.value = TRUE)$p.value
              }),
              
              gap_width = c(rep(gap_vec[g], 17)),
              prop_missing = c(rep(prop_vec[p],17)),
              dataset = c(rep(dataset[d],17)), 
              method = rep(algorithm_names[methods[m]],17) 
            )  
          }
            
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
