multiHeatmap <- function(crit, agEval, m, by = "crit", f = "median"){
  
  M <- length(m)
  C <- length(crit)
  z_list <- compileMatrix(agEval)[[f]]
  
  stopifnot((by == "crit" && length(m) == 1 && length(crit) > 1) | (by == "method" && length(crit) == 1 && length(m)>1))
  
  # get legend
  g_legend<-function(a.gplot){
    tmp <- ggplot_gtable(ggplot_build(a.gplot))
    leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
    legend <- tmp$grobs[[leg]]
    return(legend)}
  
  
  heatmapList <- list()
  
  attr <- list()
  
  if(by == "crit"){
    bound = C
    by_vec = crit
    
    for(cr in 1:bound){
      
      if(cr == 1){
        titles <- paste0("bquote(psi ==",(1:5)*10,")")
        titles_theme <- element_text()
        axis.labels.x <- element_blank()
        axis.labels.y <- element_blank()
        axis.ticks <- element_blank()
        
      }
      else if(cr == bound){
        titles <- rep("",5)
        axis.labels.x <- element_text()
        axis.labels.y <- element_text()
        axis.ticks <- element_blank()
        titles_theme <- element_blank()
      }
      else{
        titles <- rep("",5)
        titles_theme <- element_blank()
        axis.labels.x <- element_blank()
        axis.labels.y <- element_blank()
        axis.ticks <- element_blank()
      }
      
      
      plott <- list()
      
      for(d in 1:5){
        rownames(z_list[[crit[cr]]][[m]][[d]]) <- gsub("p","",rownames(z_list[[crit[cr]]][[m]][[d]]), fixed = TRUE)
        colnames(z_list[[crit[cr]]][[m]][[d]]) <- gsub("g","",colnames(z_list[[crit[cr]]][[m]][[d]]), fixed = TRUE)
        
        
        plott[[d]] <- melt(z_list[[crit[cr]]][[m]][[d]])
        colnames(plott[[d]]) <- c("p","g", "value")
      }
      
      rng = range(c(plott[[1]]$value, plott[[2]]$value ,plott[[3]]$value, plott[[4]]$value, plott[[5]]$value))
      col = colorRampPalette(colors = c("#F9E0AA","#F7C65B","#FAAF08","#FA812F","#FA4032","#F92111"))(100)
      
      D1 <- ggplot(plott[[1]], aes(as.factor(p), as.factor(g), fill = value)) + geom_tile() + 
        scale_fill_gradientn(colours = col, values = c(0,1),
                             limits = rng) +
        
        labs(x = "proportion missing", y = "gap width", fill = by_vec[cr], title = eval(parse(text = titles[1]))) + 
        theme_minimal() + 
        
        theme(legend.position = "none",
              
              axis.title.x = element_blank(),
              axis.title.y = element_blank(),
              axis.text.x = axis.labels.x,
              axis.text.y = element_text(),
              plot.title = titles_theme,
              axis.ticks.x = axis.ticks,
              axis.ticks.y = axis.ticks)
      
      D2 <- ggplot(plott[[2]], aes(as.factor(p), as.factor(g), fill = value)) + geom_tile() + 
        scale_fill_gradientn(colours = col, values = c(0,1),
                             limits = rng) +
        
        labs(x = "proportion missing", y = "gap width", fill = by_vec[cr], title = eval(parse(text = titles[2]))) + 
        theme_minimal() + 
        
        theme(legend.position = "none",
              
              axis.title.x = element_blank(),
              axis.title.y = element_blank(),
              axis.text.x = axis.labels.x,
              axis.text.y = element_blank(),
              axis.ticks.x = axis.ticks,
              axis.ticks.y = axis.ticks,
              plot.title = titles_theme)
      
      D3 <- ggplot(plott[[3]], aes(as.factor(p), as.factor(g), fill = value)) + geom_tile() + 
        scale_fill_gradientn(colours = col, values = c(0,1),
                             limits = rng) +
        
        labs(x = "proportion missing", y = "gap width", fill = by_vec[cr], title = eval(parse(text = titles[3]))) + 
        theme_minimal() + 
        
        theme(legend.position = "none",
              
              axis.title.x = element_blank(),
              axis.title.y = element_blank(),
              axis.text.x = axis.labels.x,
              axis.text.y = element_blank(),
              axis.ticks.x = axis.ticks,
              axis.ticks.y = axis.ticks,
              plot.title = titles_theme)
      
      D4 <- ggplot(plott[[4]], aes(as.factor(p), as.factor(g), fill = value)) + geom_tile() + 
        scale_fill_gradientn(colours = col, values = c(0,1),
                             limits = rng) +
        
        labs(x = "proportion missing", y = "gap width", fill = by_vec[cr], title = eval(parse(text = titles[4]))) + 
        theme_minimal() + 
        
        theme(legend.position = "none",
              
              axis.title.x = element_blank(),
              axis.title.y = element_blank(),
              axis.text.x = axis.labels.x,
              axis.text.y = element_blank(),
              axis.ticks.x = axis.ticks,
              axis.ticks.y = axis.ticks,
              plot.title = titles_theme)
      
      D5 <- ggplot(plott[[5]], aes(as.factor(p), as.factor(g), fill = value)) + geom_tile() + 
        scale_fill_gradientn(colours = col, values = c(0,1),
                             limits = rng) +
        
        labs(x = "proportion missing", y = "gap width", fill = by_vec[cr], title = eval(parse(text = titles[5]))) + 
        theme_minimal() + 
        
        theme(legend.position = "none",
              
              axis.title.x = element_blank(),
              axis.title.y = element_blank(),
              axis.text.x = axis.labels.x,
              axis.text.y = element_blank(),
              axis.ticks.x = axis.ticks,
              axis.ticks.y = axis.ticks,
              plot.title = titles_theme)
      
      
      
      # make dummy plot to retrieve legend
      
      dumPlot <- ggplot(plott[[5]], aes(as.factor(p), as.factor(g), fill = value)) + geom_tile() + 
        scale_fill_gradientn(colours = col, values = c(0,1),
                             limits = rng) +
        theme(legend.position = "right") + 
        labs(fill = crit[cr])
      
      myLegend <-g_legend(dumPlot)
      
      myHeatmaps <- grid.arrange(D1,D2,D3,D4,D5, ncol = 5, widths = c(10,10,10,10,10))
      heatmapList[[cr]] <- grid.arrange(myHeatmaps, myLegend, ncol = 2, widths = c(40,5))
    }
    
    names(heatmapList) <- by_vec
    
    call <- paste0("heatmapList[[",1:(bound-1),"]],")
    call <- c("grid.arrange(",call,paste0("heatmapList[[",bound,"]], nrow = ",bound,", bottom = 'proportion missing', left = 'gap width', top = m)"))
    
    plotWindow <- eval(parse(text = call))
  }
  
  else if(by == "method"){
    bound = M
    by_vec = m 
    rng <- range(z_list[[crit]][m])
    
    for(cr in 1:bound){
      
      if(cr == 1){
        titles <- paste0("bquote(psi ==",(1:5)*10,")")
        titles_theme <- element_text()
        axis.labels.x <- element_blank()
        axis.labels.y <- element_blank()
        axis.ticks <- element_blank()
      }
      else if(cr == bound){
        titles <- rep("",5)
        axis.labels.x <- element_text()
        axis.labels.y <- element_text()
        axis.ticks <- element_blank()
        titles_theme <- element_blank()
      }
      else{
        titles <- rep("",5)
        titles_theme <- element_blank()
        axis.labels.x <- element_blank()
        axis.labels.y <- element_blank()
        axis.ticks <- element_blank()
      }
      
      plott <- list()
      
      for(d in 1:5){
        rownames(z_list[[crit]][[m[cr]]][[d]]) <- gsub("p","",rownames(z_list[[crit]][[m[cr]]][[d]]), fixed = TRUE)
        colnames(z_list[[crit]][[m[cr]]][[d]]) <- gsub("g","",colnames(z_list[[crit]][[m[cr]]][[d]]), fixed = TRUE)
        
        plott[[d]] <- melt(z_list[[crit]][[m[cr]]][[d]])
        colnames(plott[[d]]) <- c("p","g", "value")
      }
      
      rng = range(z_list[[crit]][m])
      col = colorRampPalette(colors = c("#F9E0AA","#F7C65B","#FAAF08","#FA812F","#FA4032","#F92111"))(100)
      
      D1 <- ggplot(plott[[1]], aes(as.factor(p), as.factor(g), fill = value)) + geom_tile() + 
        scale_fill_gradientn(colours = col, values = c(0,1),
                             limits = rng) +
        
        labs(x = "proportion missing", y = "gap width", fill = by_vec[cr], title = eval(parse(text = titles[1]))) + 
        theme_minimal() + 
        
        theme(legend.position = "none",
              
              axis.title.x = element_blank(),
              axis.title.y = element_blank(),
              axis.text.x = axis.labels.x,
              axis.text.y = element_text(),
              plot.title = titles_theme,
              axis.ticks.x = axis.ticks,
              axis.ticks.y = axis.ticks)
      
      D2 <- ggplot(plott[[2]], aes(as.factor(p), as.factor(g), fill = value)) + geom_tile() + 
        scale_fill_gradientn(colours = col, values = c(0,1),
                             limits = rng) +
        
        labs(x = "proportion missing", y = "gap width", fill = by_vec[cr], title = eval(parse(text = titles[2]))) + 
        theme_minimal() + 
        
        theme(legend.position = "none",
              
              axis.title.x = element_blank(),
              axis.title.y = element_blank(),
              axis.text.x = axis.labels.x,
              axis.text.y = element_blank(),
              axis.ticks.x = axis.ticks,
              axis.ticks.y = axis.ticks,
              plot.title = titles_theme)
      
      D3 <- ggplot(plott[[3]], aes(as.factor(p), as.factor(g), fill = value)) + geom_tile() + 
        scale_fill_gradientn(colours = col, values = c(0,1),
                             limits = rng) +
        
        labs(x = "proportion missing", y = "gap width", fill = by_vec[cr], title = eval(parse(text = titles[3]))) + 
        theme_minimal() + 
        
        theme(legend.position = "none",
              
              axis.title.x = element_blank(),
              axis.title.y = element_blank(),
              axis.text.x = axis.labels.x,
              axis.text.y = element_blank(),
              axis.ticks.x = axis.ticks,
              axis.ticks.y = axis.ticks,
              plot.title = titles_theme)
      
      D4 <- ggplot(plott[[4]], aes(as.factor(p), as.factor(g), fill = value)) + geom_tile() + 
        scale_fill_gradientn(colours = col, values = c(0,1),
                             limits = rng) +
        
        labs(x = "proportion missing", y = "gap width", fill = by_vec[cr], title = eval(parse(text = titles[4]))) + 
        theme_minimal() + 
        
        theme(legend.position = "none",
              
              axis.title.x = element_blank(),
              axis.title.y = element_blank(),
              axis.text.x = axis.labels.x,
              axis.text.y = element_blank(),
              axis.ticks.x = axis.ticks,
              axis.ticks.y = axis.ticks,
              plot.title = titles_theme)
      
      D5 <- ggplot(plott[[5]], aes(as.factor(p), as.factor(g), fill = value)) + geom_tile() + 
        scale_fill_gradientn(colours = col, values = c(0,1),
                             limits = rng) +
        
        labs(x = "proportion missing", y = "gap width", fill = by_vec[cr], title = eval(parse(text = titles[5]))) + 
        theme_minimal() + 
        
        theme(legend.position = "none",
              
              axis.title.x = element_blank(),
              axis.title.y = element_blank(),
              axis.text.x = axis.labels.x,
              axis.text.y = element_blank(),
              axis.ticks.x = axis.ticks,
              axis.ticks.y = axis.ticks,
              plot.title = titles_theme,
              axis.title.y.right = element_text())
      
      heatmapList[[cr]] <- grid.arrange(D1,D2,D3,D4,D5, ncol = 5, widths = c(10,10,10,10,10), right = m[cr])
    }
    names(heatmapList) <- by_vec
    
    dumPlot <- ggplot(plott[[5]], aes(as.factor(p), as.factor(g), fill = value)) + geom_tile() + 
      scale_fill_gradientn(colours = col, values = c(0,1),
                           limits = rng) +
      theme(legend.position = "bottom") + 
      labs(fill = crit)
    
    myLegend <- g_legend(dumPlot)
    
    call <- paste0("heatmapList[[",1:(bound-1),"]],")
    call <- c("grid.arrange(",call,paste0("heatmapList[[",bound,"]], nrow = ",bound,", bottom = 'proportion missing', left = 'gap width')"))
    
    plotWindow <- eval(parse(text = call))
    plotWindow <- grid.arrange(plotWindow, myLegend, ncol = 1, heights = c(50, 10))
  }
  
  return(plotWindow)
}



