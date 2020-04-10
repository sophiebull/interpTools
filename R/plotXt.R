#' Plot Each Component of Xt
#' 
#' A function to generate a series of plots to visualize each component of the original time series
#' @param d \code{numeric}; The index of the original time series to show
#' @param cptwise \code{logical}; Whether to display Xt componentwise, or not
#' @param simData \code{simList}; An object containing the information about the original data
#' @param axisLabels \code{logical}; Set to \code{TRUE} if returning a single plot (i.e. \code{cptwise = TRUE} and \code{return != NULL}), and \code{FALSE} otherwise.
#' @param plot.title \code{logical}; Whether to include individual plot titles
#' @param return \code{character}; The particular plot(s) to display.\cr  \cr
#'                            If \code{cptwise = TRUE} and \code{return = NULL}, all of \code{'Mt'}, \code{'Tt'}, \code{'freq'}, and \code{'Wt'} will be displayed.\cr \cr
#'                            If \code{cptwise = TRUE} and \code{return = 'Mt' | 'Tt' | 'freq' | 'Wt'}, the specified plot will show. \cr \cr
#'                            If \code{cptwise = FALSE}, \code{return} must be the default (\code{NULL}) to display the sum total (this is \code{Xt}).
#'                            
#'                               
plotXt <- function(d, simData, cptwise = T, axisLabels = F, plot.title = T, return = NULL){
  require(ggplot2) 
  require(gridExtra)
  
stopifnot(class(simData) == "simList", (is.null(return) | return == "Mt" | return == "Tt" | return == "freq" | return == "Wt"))

if(cptwise & is.null(return)) stop("Please specify if you wish to return ")
  
  
if(cptwise & axisLabels & is.null(return)){
  warning("axisLabels should be set to FALSE if the function is to return all cptwise plots")
}

  t <- 0:(length(simData$Xt[[d]])-1)
  
  if(axisLabels){
    xlab = "time"
    ylab = "value"
    axis.title.x = element_text()
    axis.text.y = element_text()
    axis.text.x = element_text()
    axis.text.x.bottom = element_text()
    axis.title.y = element_text()
  }  
  else if(!axisLabels){
    xlab = ""
    ylab = ""
    axis.title.x = element_blank()
    axis.text.x.bottom = element_blank()
    axis.title.y = element_blank()
    axis.text.y = element_blank()
    axis.text.x = element_blank()
  }
  
  if(plot.title){
    plot.title = element_text()
  }
  else if(!plot.title){
    plot.title = element_blank()
  }
  
  mt <- ggplot()+
    
    ylim(min(simData$Xt[[d]]), max(simData$Xt[[d]])) + 
    
    geom_line(aes(x=t,y=simData$Mt[[d]]), lwd = 0.2) +
    geom_line(aes(x=t,y=simData$Mt_mu[[d]]), lty = 2, lwd = 0.2)+
    
    ggtitle(bquote(m[t] == mu[t] + mu*", polynomial of order"~.(simData$Mt_numTrend[[d]]))) + 
    
    annotate("text",x=(length(simData$Mt[[d]])+5),y=simData$Mt_mu[[d]], label = "\U003BC")+
    
    labs(x = xlab, y = ylab)+
    theme_light() +
    theme(axis.title.x = axis.title.x, 
          axis.text.x.bottom = axis.text.x.bottom,
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.ticks.x = element_blank(),
          plot.margin = unit(c(1,0.2,1,1),"cm"),
          axis.title.y = axis.title.y,
          plot.title = plot.title)
  
  if(is.null(simData$Tt_bandwidth[[d]])){
    bw = "unspecified"
  }
  
  else if(!(is.null(simData$Tt_bandwidth[[d]]))){
    bw = eval(substitute(paste(10^-bandwidth), list(bandwidth = simData$Tt_bandwidth[[d]])))
  }
  
  
  tt <- ggplot()+
    
    geom_line(aes(x=t,y=simData$Tt[[d]]), lwd = 0.2) +
    ggtitle(bquote(t[t] == sum(b[i]*sin*"("~omega[i]*t*")",i==1,psi)))+
    
    labs(x = xlab, y = ylab)+
    theme_light() +
    theme(axis.title.x = axis.title.x, 
          axis.text.x.bottom = axis.text.x.bottom,
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.ticks.x = element_blank(),
          plot.margin = unit(c(0,0.2,1,1),"cm"),
          axis.title.y = axis.title.y,
          plot.title = plot.title) 

  
  wt <- ggplot()+
    
    geom_line(aes(x=t,y=as.numeric(simData$Wt[[d]])), lwd = 0.2) +
    
    ggtitle(bquote(xi[t]*"~ ARMA("~.(simData$Wt_p[[d]])*","~.(simData$Wt_q[[d]])*"), SNR ="~.(simData$SNR[[d]]))) + 
    
    labs(x = xlab, y = ylab)+
    theme_light()+
    theme(plot.margin = unit(c(0,0.2,1,1),"cm"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.title.y = axis.title.y,
          plot.title = plot.title)

  freq <- ggplot() + 
    
    geom_point(aes(x = simData$Tt_freq[[d]], y = rep(0,length(simData$Tt_freq[[d]]))), size = 0.4) + 
    geom_vline(xintercept = simData$Tt_freq[[d]], lwd = 0.05) + 
    ggtitle(bquote(psi == .(length(simData$Tt_freq[[d]]))*","~'bandwidth' == .(bw))) +
    
    labs(x = bquote(f == omega/(2*pi)), y = "") +
    xlim(0,0.5) + 
    ylim(0,0) + 
    theme_light()+
    
    theme(axis.text.y = element_blank(),
          axis.text.x = axis.text.x,
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.ticks.y = element_blank(),
          axis.title.x = axis.title.x,
          plot.title = plot.title,
          plot.margin = unit(c(0,0.2,0,1),"cm")) +
    
    coord_fixed(ratio = 0.4)
  
    xt <- ggplot()+
    
    geom_line(aes(x=t,y=as.numeric(simData$Xt[[d]])), lwd = 0.2) +
    geom_line(aes(x=t,y=simData$Mt[[d]]), lwd = 0.2, col = "white")+
      
    ggtitle(bquote(x["t,"~.(d)] == m["t,"~.(d)] + t["t,"~.(d)] + xi["t,"~.(d)]))+
      
    labs(x = xlab, y = ylab)+
    theme_light() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()) 
  
  if(cptwise){
    if(is.null(return) && axisLabels == F){
    g1 <- grid.arrange(arrangeGrob(mt,tt,wt, nrow = 3), bottom = textGrob("time", vjust = -3.5, hjust = -0.5), left = textGrob("value", rot = 90, vjust = 2))
    grid.arrange(g1,freq, ncol = 1, heights = c(3,0.6), bottom = textGrob(bquote(f == omega/(2*pi))))
      }
    else if(return == "Mt"){
      mt
    }
    else if(return == "Tt"){
      tt
    }
    else if(return == "freq"){
      freq
    }
    else if(return == "Wt"){
      wt
    }
  }
    
  else if(!cptwise){
    xt
  }
  }

