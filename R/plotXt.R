#' Plot Each Component of Xt
#' 
#' A function to generate a triptych to visualize each component of the original time series
#' @param d The index of the original time series of interest
#' 

plotXt <- function(d){
  require(ggplot2)
  require(gridExtra)
  
  t <- 0:(length(simData$Xt[[d]])-1)
    
  mt <- ggplot()+
    
    geom_line(aes(x=t,y=simData$Mt[[d]]), col = "tomato1") +
    geom_line(aes(x=t,y=simData$Mt_mu[[d]]), col = "tomato1", lty = 2)+
    
    ggtitle(expression(M[t] == mu[t] + mu), subtitle = paste("polynomial of order",simData$Mt_numTrend[[d]])) + 
    
    labs(x = "time", y = "value")+
    theme_minimal() +
    theme(axis.title.x = element_blank(), 
          axis.text.x.bottom = element_blank(),
          plot.margin = unit(c(1,1,0,1),"cm"))
  
  if(is.null(simData$Tt_bandwidth[[d]])){
    bw = "unspecified"
  }
  
  else if(!(is.null(simData$Tt_bandwidth[[d]]))){
    bw = eval(substitute(paste(10^-bandwidth), list(bandwidth = simData$Tt_bandwidth[[d]])))
  }
  
  freq <- ggplot() + 
    
    geom_point(aes(x = simData$Tt_freq[[d]], y = rep(0,length(simData$Tt_freq[[d]]))), col = "darkgoldenrod1", size = 0.4) + 
    ggtitle(expression(T[t] == sum(a*sin(omega[i]*t), i == 1,Omega)),
            subtitle = paste("F = ", length(simData$Tt_freq[[d]]),", bin width = ", bw, sep="")) + 
    
    labs(x = "frequency", y = "")+
    xlim(0,0.5) + 
    ylim(0,0) + 
    theme_minimal()+
    
    theme(axis.text.y.left = element_blank(),
          axis.title.x = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          plot.margin = unit(c(-2,1,-2,1),"cm")) +
    
    coord_fixed(ratio = 0.4)
  
  tt <- ggplot()+
    
    geom_line(aes(x=t,y=simData$Tt[[d]]), col = "darkgoldenrod1") +
    
    labs(x = "time", y = "value")+
    theme_minimal() +
    theme(axis.title.x = element_blank(), 
          axis.text.x.bottom = element_blank(),
          plot.margin = unit(c(0,1,1,1),"cm"))
    
  
  wt <- ggplot()+
    
    geom_line(aes(x=t,y=as.numeric(simData$Wt[[d]])), col = "dodgerblue3") +
    
    ggtitle(expression(W[t]), subtitle = paste("ARMA(",simData$Wt_p[[d]],",",simData$Wt_q[[d]],")",sep="")) + 
    
    labs(x = "time", y = "value")+
    theme_minimal()+
    theme(plot.margin = unit(c(0,1,1,1),"cm"))

  
    grid.arrange(mt,freq,tt,wt, nrow = 4)

  }

