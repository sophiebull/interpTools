
convertNames <- function(agEval){
  old_algorithm_names <- c("Nearest.Neighbor",
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

algorithm_names <- c("NN",
                     "LI", 
                     "NCS",
                     "FMM", 
                     "HCS",
                     "SI",
                     "KAF",
                     "KKSF",
                     "LOCF",
                     "NOCB", 
                     "SMA", 
                     "LWMA",
                     "EWMA",
                     "RMEA",
                     "RMED", 
                     "RMOD",
                     "RRND",
                     "HWI")

for(d in 1:length(agEval)){
  for (p in 1:length(agEval[[1]])){
    for(g in 1:length(agEval[[1]][[1]])){
      names(agEval[[d]][[p]][[g]]) <- algorithm_names[old_algorithm_names %in% names(agEval[[d]][[p]][[g]])]
      for(m in 1:length(agEval[[1]][[1]][[1]])){
        agEval[[d]][[p]][[g]][[m]]$method <- as.factor(rep(names(agEval[[d]][[p]][[g]])[m], nrow(agEval[[d]][[p]][[g]][[m]])))
      }
    }
  }
}

return(agEval)
}
