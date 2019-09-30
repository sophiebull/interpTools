#' Plot Each Component of Xt
#' 
#' A function to generate a triptych to visualize each component of the original time series
#' @param d The index of the original time series of interest
#' @param cptwise Logical; whether to display Xt componentwise or not

plotXt <- function(d, cptwise = T){
  require(ggplot2) 
  require(gridExtra)
  

  t <- 0:(length(simData$Xt[[d]])-1)
    
  mt <- ggplot()+
    
    ylim(min(simData$Xt[[d]]), max(simData$Xt[[d]])) + 
    
    geom_line(aes(x=t,y=simData$Mt[[d]]), lwd = 0.2) +
    geom_line(aes(x=t,y=simData$Mt_mu[[d]]), lty = 2, lwd = 0.2)+
    
    ggtitle(bquote(m[t] == mu[t] + mu*", polynomial of order"~.(simData$Mt_numTrend[[d]]))) + 
    
    annotate("text",x=(length(simData$Mt[[d]])+5),y=simData$Mt_mu[[d]], label = expression(mu))+
    
    labs(x = "time", y = "")+
    theme_light() +
    theme(axis.title.x = element_blank(), 
          axis.text.x.bottom = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.ticks.x = element_blank(),
          plot.margin = unit(c(1,0,1,1),"cm"),
          axis.title.y = element_blank())
  
  if(is.null(simData$Tt_bandwidth[[d]])){
    bw = "unspecified"
  }
  
  else if(!(is.null(simData$Tt_bandwidth[[d]]))){
    bw = eval(substitute(paste(10^-bandwidth), list(bandwidth = simData$Tt_bandwidth[[d]])))
  }
  
  
  tt <- ggplot()+
    
    geom_line(aes(x=t,y=simData$Tt[[d]]), lwd = 0.2) +
    ggtitle(bquote(t[t] == sum(b[i]*sin*"("~omega[i]*t*")",i==1,psi)))+
    
    labs(x = "time", y = "")+
    theme_light() +
    theme(axis.title.x = element_blank(), 
          axis.text.x.bottom = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.ticks.x = element_blank(),
          plot.margin = unit(c(0,0,1,1),"cm"),
          axis.title.y = element_blank()) 

  
  wt <- ggplot()+
    
    geom_line(aes(x=t,y=as.numeric(simData$Wt[[d]])), lwd = 0.2) +
    
    ggtitle(bquote(xi[t]*"~ ARMA("~.(simData$Wt_p[[d]])*","~.(simData$Wt_q[[d]])*"), SNR ="~.(simData$SNR[[d]]))) + 
    
    labs(x = "time", y = "")+
    theme_light()+
    theme(plot.margin = unit(c(0,0,1,1),"cm"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.title.y = element_blank())

  freq <- ggplot() + 
    
    geom_point(aes(x = simData$Tt_freq[[d]], y = rep(0,length(simData$Tt_freq[[d]]))), size = 0.4) + 
    geom_vline(xintercept = simData$Tt_freq[[d]], lwd = 0.05) + 
    ggtitle(bquote(psi == .(length(simData$Tt_freq[[d]]))*","~'bin width' == .(bw))) +
    
    labs(x = bquote(f == omega/(2*pi)), y = "") +
    xlim(0,0.5) + 
    ylim(0,0) + 
    theme_light()+
    
    theme(axis.text.y = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.ticks.y = element_blank(),
          plot.margin = unit(c(0,0,0,1),"cm")) +
    
    coord_fixed(ratio = 0.4)
  
  
    xt <- ggplot()+
    
    geom_line(aes(x=t,y=simData$Xt[[d]]), lwd = 0.2) +
    geom_line(aes(x=t,y=simData$Mt[[d]]), lwd = 0.2, col = "blue")+
      
    ggtitle(bquote(x[t] == m[t] + t[t] + xi[t]))+
    
    labs(x = "time", y = "value")+
    theme_light() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()) 
  
  if(cptwise){
    grid.arrange(mt,tt,wt,freq, nrow = 4)
  }
    
  else if(!cptwise){
    xt
  }
  }

