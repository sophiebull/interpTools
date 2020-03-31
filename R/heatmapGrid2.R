heatmapGrid2 <- function(agEval, f="median", crit, m, colors = c("#F9E0AA","#F7C65B","#FAAF08","#FA812F","#FA4032","#F92111"), d = 1:5){

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
    
    titles[vd] <- paste0("bquote(psi ==",vd*10,")")
    
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
          
          labs(x = "proportion missing", y = "gap width", fill = paste0(crit," (",f,")"), title = eval(parse(text = titles[vd]))) + 
          theme_minimal() + 
          
          theme(legend.position = "none",
                axis.title.x = element_blank(),
                axis.title.y = element_blank(),
                plot.title = element_text(size = 12))
      }
      else if(vd != 1 && vd !=D){
        mapList[[vd]] <- ggplot(plott[[vd]], aes(as.numeric(p), as.factor(g), fill = value)) + geom_tile() + 
          scale_fill_gradientn(colors = col, values = c(0,1),
                               limits = rng) +
          
          labs(x = "proportion missing", y = "gap width", fill = paste0(crit," (",f,")"), title = eval(parse(text = titles[vd])) ) + 
          theme_minimal() + 
          
          theme(legend.position = "none",
                axis.title.x = element_blank(),
                axis.title.y = element_blank(),
                axis.text.y = element_blank(),
                plot.title = element_text(size = 12))
      }
      else if(vd == D){
        mapList[[vd]] <- ggplot(plott[[vd]], aes(as.numeric(p), as.factor(g), fill = value)) + geom_tile() + 
          scale_fill_gradientn(colors = col, values = c(0,1),
                               limits = rng) +
          
          labs(x = "proportion missing", y = "gap width", fill = paste0(crit," (",f,")"), title = eval(parse(text = titles[vd]))) + 
          theme_minimal() + 
          
          theme(legend.position = "none",
                axis.title.x = element_blank(),
                axis.title.y = element_blank(),
                axis.text.y = element_blank(),
                plot.title = element_text(size = 12))
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
      
      labs(x = "proportion missing", y = "gap width", fill = paste0(crit," (",f,")"), title = eval(parse(text = titles[1]))) + 
      theme_minimal() + 
      
      theme(legend.position = "none",
            axis.title.x = element_text(),
            axis.title.y = element_text(),
            plot.title = element_text(size = 12)) 
    
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
