#'  Generate Heatmaps of Performance
#' 
#'  A function to generate a set of heatmaps depicting the performances of particular sets of interpolations. This graphic can be considered as a series of 'compressed' surface plots, arranged by dataset.\cr
#'  
#' @param agEval \code{agEvaluate}; An object containing the aggregated performance metrics (result of \code{agEvaluate()})
#' @param f \code{character}; The statistic of interest that will be depicted by the heatmap. Possible choices are listed in \code{?agEvaluate}.
#' @param crit \code{character}; A single element describing the performance metric of interest
#' @param m \code{character}; A single element describing the interpolation methods of interest
#' @param d \code{numeric}; A vector to indicate datasets of interest
#' @param colors \code{character}; A vector of the desired color palette, with entries in HTML format (\code{"#xxxxxx"}) 

heatmapGrid <- function(agEval, 
                         f="median", 
                         crit, 
                         m, 
                         d = 1:length(agEval), 
                         colors = c("#F9E0AA","#F7C65B","#FAAF08","#FA812F","#FA4032","#F92111")){

  ## LOGICAL CHECKS ############
  
  if(sum(duplicated(d) != 0)) stop(paste0("'d' contains redundant elements at position(s): ", paste0(c(1:length(d))[duplicated(d)], collapse = ", ") ))
  if(sum(duplicated(m) != 0)) stop(paste0("'m' contains redundant elements at position(s): ", paste0(c(1:length(m))[duplicated(m)], collapse = ", ") ))
  if(sum(duplicated(crit) != 0)) stop(paste0("'crit' contains redundant elements at position(s): ", paste0(c(1:length(crit))[duplicated(crit)], collapse = ", ") ))
  
  #if(by != "crit" & by != "method") stop("'by' must be either 'crit' or 'method'.")
  
  if(class(agEval) != "agEvaluate") stop("'agEval' object must be of class 'agEvaluate'. Please use agEvaluate().")
  
  if(length(crit) != 1) stop("'crit' must contain only a single character element.")
  if(length(f) != 1) stop("'f' must contain only a single character element.")
  if(length(m) != 1) stop("'m' must contain only a single character element.")
  #if(length(by) != 1) stop("'by' must contain only a single character element.")
  
  if(!(m %in%  names(agEval[[1]][[1]][[1]]))) stop(paste0("Method '", m,"' not found. Possible choices are: '", paste0(names(agEval[[1]][[1]][[1]]), collapse = "', '"),"'."))
  if(!all(paste0("D",d) %in% names(agEval))) stop("Dataset(s) ", paste0(d[!paste0("D",d) %in% names(agEval)], collapse = ", ")," not found. Possible choices are: ", paste0(gsub("D", "",names(agEval)), collapse = ", "))
  if(!all(f %in% names(agEval[[1]][[1]][[1]][[1]]))) stop(paste0(c("f must be one of: '",paste0(names(agEval[[1]][[1]][[1]][[1]]), collapse = "', '"),"'."), collapse = ""))
  if(!crit %in% rownames(agEval[[1]][[1]][[1]][[1]])) stop(paste0("Criterion '",crit,"' must be one of ", paste(rownames(agEval[[1]][[1]][[1]][[1]]),collapse = ", "),"."))
  
  if(length(colors) <2) stop("'colors' must contain at least two colors (each in HTML format: '#xxxxxx')")
  
 ##################
  
  # get legend
  g_legend<-function(a.gplot){
    tmp <- ggplot_gtable(ggplot_build(a.gplot))
    leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
    legend <- tmp$grobs[[leg]]
    return(legend)}
  
  
  z_list <- compileMatrix(agEval)[[f]]
  D <- length(d)
  
  plott <- list()
  titles <- character()
  for(vd in 1:D){
    rownames(z_list[[crit]][[m]][[d[vd]]]) <- gsub("p","",rownames(z_list[[crit]][[m]][[d[vd]]]), fixed = TRUE)
    colnames(z_list[[crit]][[m]][[d[vd]]]) <- gsub("g","",colnames(z_list[[crit]][[m]][[d[vd]]]), fixed = TRUE)
    
    plott[[vd]] <- melt(z_list[[crit]][[m]][[d[vd]]])
    colnames(plott[[vd]]) <- c("p","g", "value")
    
    titles[vd] <- paste0("Dataset ", vd," , '", m ,"'")
    
  }
  
  if(length(d)>1){
  d_strand <- paste0("plott[[",1:(D-1),"]]$value,")
  d_strand <- paste0(c("range(c(",d_strand,"plott[[",D,"]]$value))"), collapse = "")
  }
  else if(length(d) == 1){
  d_strand <- paste0("range(c(plott[[1]]$value))")  
  }
  
  rng = eval(parse(text = d_strand))
  
  col = colors
  
  mapList <- list()
  
  if(length(d)>1){
    for(vd in 1:D){
      if(vd == 1){
        mapList[[vd]] <- ggplot(plott[[vd]], aes(as.numeric(p), as.factor(g), fill = value)) + geom_tile() + 
          scale_fill_gradientn(colors = col, values = c(0,1),
                               limits = rng) +
          
          labs(x = "proportion missing", y = "gap width", fill = paste0(crit," (",f,")"), title = titles[vd]) + 
          theme_minimal() + 
          
          theme(legend.position = "none",
                axis.title.x = element_blank(),
                axis.title.y = element_blank(),
                plot.title = element_text(size = 12, hjust = 0.5))
      }
      else if(vd != 1 && vd !=D){
        mapList[[vd]] <- ggplot(plott[[vd]], aes(as.numeric(p), as.factor(g), fill = value)) + geom_tile() + 
          scale_fill_gradientn(colors = col, values = c(0,1),
                               limits = rng) +
          
          labs(x = "proportion missing", y = "gap width", fill = paste0(crit," (",f,")"), title = titles[vd] ) + 
          theme_minimal() + 
          
          theme(legend.position = "none",
                axis.title.x = element_blank(),
                axis.title.y = element_blank(),
                axis.text.y = element_blank(),
                plot.title = element_text(size = 12, hjust = 0.5))
      }
      else if(vd == D){
        mapList[[vd]] <- ggplot(plott[[vd]], aes(as.numeric(p), as.factor(g), fill = value)) + geom_tile() + 
          scale_fill_gradientn(colors = col, values = c(0,1),
                               limits = rng) +
          
          labs(x = "proportion missing", y = "gap width", fill = paste0(crit," (",f,")"), title = titles[vd]) + 
          theme_minimal() + 
          
          theme(legend.position = "none",
                axis.title.x = element_blank(),
                axis.title.y = element_blank(),
                axis.text.y = element_blank(),
                plot.title = element_text(size = 12, hjust = 0.5))
      }
    }
  
    
    # make dummy plot to retrieve legend
    dumPlot <- ggplot(plott[[D]], aes(as.numeric(p), as.factor(g), fill = value)) + geom_tile() + 
      scale_fill_gradientn(colors = col, values = c(0,1),
                           limits = rng) +
      theme(legend.position = "bottom") + 
      labs(fill = paste0(crit," (",f,")"))
    
    myLegend<-g_legend(dumPlot)
    
    h_strand <- paste0("mapList[[",1:(D-1),"]],")
    h_strand <- paste0(c("grid.arrange(",h_strand,"mapList[[",D,"]], ncol = ",D,", bottom = 'proportion missing', left = 'gap width')"), collapse = "")
  }
  
  else if(length(d) == 1){
    mapList <- ggplot(plott[[1]], aes(as.numeric(p), as.factor(g), fill = value)) + geom_tile() + 
      scale_fill_gradientn(colors = col, values = c(0,1),
                           limits = rng) +
      
      labs(x = "proportion missing", y = "gap width", fill = paste0(crit," (",f,")"), title = titles[1]) + 
      theme_minimal() + 
      
      theme(legend.position = "none",
            axis.title.x = element_text(),
            axis.title.y = element_text(),
            plot.title = element_text(size = 12, hjust = 0.5)) 
    
    # make dummy plot to retrieve legend
    dumPlot <- ggplot(plott[[1]], aes(as.numeric(p), as.factor(g), fill = value)) + geom_tile() + 
      scale_fill_gradientn(colors = col, values = c(0,1),
                           limits = rng) +
      theme(legend.position = "bottom") + 
      labs(fill = paste0(crit," (",f,")"))
    
    myLegend<-g_legend(dumPlot)
    
    h_strand <- paste0("mapList")
  }
  
  plotWindow <- eval(parse(text = h_strand))
  plotWindow <- grid.arrange(plotWindow, myLegend, heights = c(10,2))
 
  return(plotWindow)
}
