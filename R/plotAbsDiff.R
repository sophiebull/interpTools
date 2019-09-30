#' Plot Interpolation Error
#' 
#' Function to generate a nested list of plots to visualize the absolute difference between the original time series and the average (across k simulations) interpolated time series under particular missingness scenarios.
#' 
#' @param OriginalData A list containing the original time series
#' @param IntData A list containing the interpolated time series
#' @param d A vector of datasets of interest
#' @param p A vector of 'proportion missings' of interest
#' @param g A vector of 'gap length' of interest
#' @param m A vector of interpolation methods of interest
#' @param output A character variable; "plot" outputs a list of plots to be called, "numeric" outputs a list of numeric vectors corresponding to each plot

plotAbsDiff <- function(OriginalData,IntData,d,p,g,m, output = "plot"){

  require(ggplot2)
  
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


D <- length(d)
M <- length(m)
P <- length(p)
G <- length(g)

#Initializing nested list object
plotList <- lapply(plotList <- vector(mode = 'list', D),function(x)
  lapply(plotList <- vector(mode = 'list', M),function(x) 
    lapply(plotList <- vector(mode = 'list', P),function(x) 
      x<-vector(mode='list',G))))

avInt <- plotList
sdInt <- plotList

# Initializing names
gap_list_names <- numeric(G)
prop_list_names <- numeric(P)
method_list_names <- numeric(M)
data_list_names <- numeric(D)

for(vd in 1:D){
  for(vm in 1:M){
    for(vp in 1:P){
      for(vg in 1:G){
        
        avInt[[vd]][[vm]][[vp]][[vg]] <- apply(sapply(IntData[[d[vd]]][[m[vm]]][[p[vp]]][[g[vg]]],unlist),1,mean)
        sdInt[[vd]][[vm]][[vp]][[vg]] <- apply(sapply(IntData[[d[vd]]][[m[vm]]][[p[vp]]][[g[vg]]],unlist),1,sd)
        
        plot <-  ggplot() +  
          geom_ribbon(aes(ymin = avInt[[vd]][[vm]][[vp]][[vg]],
                          ymax = OriginalData[[d[vd]]], 
                          x = 0:(length(avInt[[vd]][[vm]][[vp]][[vg]])-1)), 
                      fill = "mistyrose2") +
          
          #geom_ribbon(aes(ymin = as.numeric(OriginalData[[d[vd]]]) - sdInt[[vd]][[vm]][[vp]][[vg]], 
          #                ymax = as.numeric(OriginalData[[d[vd]]]) + sdInt[[vd]][[vm]][[vp]][[vg]], 
          #                x = 0:(length(OriginalData[[d[vd]]])-1)), alpha = 0.3) + 
          
          # Plotting original data
          geom_line(aes(x = 0:(length(OriginalData[[d[vd]]])-1), 
                        y = as.numeric(OriginalData[[d[vd]]])), 
                    color = "lightpink3", size = 0.5) +  
          
          # Plotting interpolated data
          geom_line(aes(x = 0:(length(avInt[[vd]][[vm]][[vp]][[vg]])-1), 
                        y = as.numeric(avInt[[vd]][[vm]][[vp]][[vg]])), 
                    color = "lightblue3", size = 0.5) + 
          
          ggtitle(paste("Dataset ",d[vd],": Original v.s. Interpolated Data"), 
                  subtitle = paste(algorithm_names[methods[m[vm]]],
                                   ", p=",prop_vec[p[vp]],
                                   ", g=",gap_vec[g[vg]],
                                   "\n","averaged across",
                                   length(IntData[[d[vd]]][[m[vm]]][[p[vp]]][[g[vg]]]),
                                   "simulations"))+
          
          labs(x = "time", 
               y = "value",
               label = "Legend Text") + 
          
          theme(plot.title = element_text(hjust = 0.5), 
                plot.subtitle = element_text(hjust=0.5))
        
        plotList[[vd]][[vm]][[vp]][[vg]] <- plot

        gap_list_names[vg] <- names(IntData[[1]][[1]][[1]])[g[vg]]
      }
      names(plotList[[vd]][[vm]][[vp]]) <- gap_list_names
      names(avInt[[vd]][[vm]][[vp]]) <- gap_list_names
      prop_list_names[vp] <- names(IntData[[1]][[1]])[p[vp]]
    }
    names(plotList[[vd]][[vm]]) <- prop_list_names
    names(avInt[[vd]][[vm]]) <- prop_list_names
    method_list_names[vm] <- names(IntData[[1]])[m[vm]]
  }
  names(plotList[[vd]]) <- method_list_names
  names(avInt[[vd]]) <- method_list_names
  data_list_names[vd] <- names(IntData)[d[vd]]
}
names(plotList) <- data_list_names
names(avInt) <- data_list_names

if(output == "plot"){
  return(plotList)
}
else if(output == "numeric"){
  return(avInt)
}

}


