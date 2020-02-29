multiHeatmap <- function(crit, agEval, m, by = "crit", f = "median", d = 1:5, colors){
  
  M <- length(m)
  C <- length(crit)
  D <- length(d)
  
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
        titles <- paste0("bquote(psi ==",d*10,")")
        titles_theme <- element_text()
        axis.labels.x <- element_blank()
        axis.labels.y <- element_blank()
        axis.text.y <- element_text()
        axis.ticks <- element_blank()
        
      }
      else if(cr == bound){
        titles <- rep("",D)
        axis.labels.x <- element_text()
        axis.text.y <- element_blank()
        axis.labels.y <- element_text()
        axis.ticks <- element_blank()
        titles_theme <- element_blank()
      }
      else{
        titles <- rep("",D)
        titles_theme <- element_blank()
        axis.labels.x <- element_blank()
        axis.labels.y <- element_blank()
        axis.text.y <- element_blank()
        axis.ticks <- element_blank()
      }
      
      plott <- list()
      
      for(vd in 1:D){
        rownames(z_list[[crit[cr]]][[m]][[d[vd]]]) <- gsub("p","",rownames(z_list[[crit[cr]]][[m]][[d[vd]]]), fixed = TRUE)
        colnames(z_list[[crit[cr]]][[m]][[d[vd]]]) <- gsub("g","",colnames(z_list[[crit[cr]]][[m]][[d[vd]]]), fixed = TRUE)
        
        
        plott[[vd]] <- melt(z_list[[crit[cr]]][[m]][[d[vd]]])
        colnames(plott[[vd]]) <- c("p","g", "value")
      }
      
      r_string <- paste0("plott[[",2:(D-1),"]]$value, ", collapse = "")
      r_string <- c("range(c(plott[[1]]$value,", r_string, paste0("plott[[",D,"]]$value))", collapse = ""))
      
      
      rng = eval(parse(text = r_string))
      
      col = colorRampPalette(colors = colors)(100)
      
      plotList <- list()
      
      for(vd in 1:D){
        
        if(vd == 1){
          plotList[[vd]] <- ggplot(plott[[vd]], aes(as.factor(p), as.factor(g), fill = value)) + geom_tile() + 
            scale_fill_gradientn(colours = col, values = c(0,1),
                                 limits = rng) +
            
            labs(x = "proportion missing", y = "gap width", fill = by_vec[cr], title = eval(parse(text = titles[vd]))) + 
            theme_minimal() + 
            
            theme(legend.position = "none",
                  
                  axis.title.x = element_blank(),
                  axis.title.y = element_blank(),
                  axis.text.x = axis.labels.x,
                  axis.text.y = element_text(),
                  plot.title = titles_theme,
                  axis.ticks.x = axis.ticks,
                  axis.ticks.y = axis.ticks)
        }
        
        if(vd != 1){
          plotList[[vd]] <- ggplot(plott[[vd]], aes(as.factor(p), as.factor(g), fill = value)) + geom_tile() + 
            scale_fill_gradientn(colours = col, values = c(0,1),
                                 limits = rng) +
            
            labs(x = "proportion missing", y = "gap width", fill = by_vec[cr], title = eval(parse(text = titles[vd]))) + 
            theme_minimal() + 
            
            theme(legend.position = "none",
                  
                  axis.title.x = element_blank(),
                  axis.title.y = element_blank(),
                  axis.text.x = axis.labels.x,
                  axis.text.y = element_blank(),
                  plot.title = titles_theme,
                  axis.ticks.x = axis.ticks,
                  axis.ticks.y = axis.ticks)
        }
        
      }
      
      # make dummy plot to retrieve legend
      
      dumPlot <- ggplot(plott[[1]], aes(as.factor(p), as.factor(g), fill = value)) + geom_tile() + 
        scale_fill_gradientn(colours = col, values = c(0,1),
                             limits = rng) +
        theme(legend.position = "right") + 
        labs(fill = crit[cr])
      
      myLegend <-g_legend(dumPlot)
      
      h_string <- paste0("plotList[[",2:(D-1),"]],", collapse = "")
      rep_string <- c(paste0(rep("10,",D-1), collapse = ""),"10")
      rep_string <- paste0(rep_string, collapse = "")
      
      h_string <- c("grid.arrange(plotList[[1]],",h_string,paste0("plotList[[",D,"]], ncol = ",D,", widths = c(",rep_string,"))", collapse = ""))
      
      myHeatmaps <- eval(parse(text = h_string))
      
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
        titles <- paste0("bquote(psi ==",d*10,")")
        titles_theme <- element_text()
        axis.labels.x <- element_blank()
        axis.labels.y <- element_blank()
        axis.text.y <- element_text()
        axis.ticks <- element_blank()
      }
      else if(cr == bound){
        titles <- rep("",D)
        axis.labels.x <- element_text()
        axis.labels.y <- element_text()
        axis.ticks <- element_blank()
        axis.text.y <- element_blank()
        titles_theme <- element_blank()
      }
      else{
        titles <- rep("",D)
        titles_theme <- element_blank()
        axis.labels.x <- element_blank()
        axis.labels.y <- element_blank()
        axis.text.y <- element_blank()
        axis.ticks <- element_blank()
      }
      
      plott <- list()
      
      for(vd in 1:D){
        rownames(z_list[[crit]][[m[cr]]][[d[vd]]]) <- gsub("p","",rownames(z_list[[crit]][[m[cr]]][[d[vd]]]), fixed = TRUE)
        colnames(z_list[[crit]][[m[cr]]][[d[vd]]]) <- gsub("g","",colnames(z_list[[crit]][[m[cr]]][[d[vd]]]), fixed = TRUE)
        
        plott[[vd]] <- melt(z_list[[crit]][[m[cr]]][[d[vd]]])
        colnames(plott[[vd]]) <- c("p","g", "value")
      }
      
      rng = range(z_list[[crit]][m])
      col = colorRampPalette(colors = colors)(100)
      
      for(vd in 1:D){
        
        if(vd == 1){
          plotList[[vd]] <- ggplot(plott[[vd]], aes(as.factor(p), as.factor(g), fill = value)) + geom_tile() + 
            scale_fill_gradientn(colours = col, values = c(0,1),
                                 limits = rng) +
            
            labs(x = "proportion missing", y = "gap width", fill = by_vec[cr], title = eval(parse(text = titles[vd]))) + 
            theme_minimal() + 
            
            theme(legend.position = "none",
                  
                  axis.title.x = element_blank(),
                  axis.title.y = element_blank(),
                  axis.text.x = axis.labels.x,
                  axis.text.y = element_text(),
                  plot.title = titles_theme,
                  axis.ticks.x = axis.ticks,
                  axis.ticks.y = axis.ticks)
        }
        
        else if(vd != 1){
          plotList[[vd]] <- ggplot(plott[[vd]], aes(as.factor(p), as.factor(g), fill = value)) + geom_tile() + 
            scale_fill_gradientn(colours = col, values = c(0,1),
                                 limits = rng) +
            
            labs(x = "proportion missing", y = "gap width", fill = by_vec[cr], title = eval(parse(text = titles[vd]))) + 
            theme_minimal() + 
            
            theme(legend.position = "none",
                  
                  axis.title.x = element_blank(),
                  axis.title.y = element_blank(),
                  axis.text.x = axis.labels.x,
                  axis.text.y = element_blank(),
                  plot.title = titles_theme,
                  axis.ticks.x = axis.ticks,
                  axis.ticks.y = axis.ticks)
        }
        
      }
      
      # making dummy plot to retrieve legend
      
      dumPlot <- ggplot(plott[[1]], aes(as.factor(p), as.factor(g), fill = value)) + geom_tile() + 
        scale_fill_gradientn(colours = col, values = c(0,1),
                             limits = rng) +
        theme(legend.position = "bottom") + 
        labs(fill = crit)
      
      myLegend <- g_legend(dumPlot)
      
      h_string <- paste0("plotList[[",2:(D-1),"]],", collapse = "")
      rep_string <- c(paste0(rep("10,",D-1), collapse = ""),"10")
      rep_string <- paste0(rep_string, collapse = "")
      
      h_string <- c("grid.arrange(plotList[[1]],",h_string,paste0("plotList[[",D,"]], ncol = ",D,", widths = c(",rep_string,"), right = m[",cr,"])", collapse = ""))
      
      heatmapList[[cr]] <- eval(parse(text = h_string))
    }
    
    names(heatmapList) <- by_vec
    
    call <- paste0("heatmapList[[",1:(bound-1),"]],")
    call <- c("grid.arrange(",call,paste0("heatmapList[[",bound,"]], nrow = ",bound,", bottom = 'proportion missing', left = 'gap width')"))
    
    plotWindow <- eval(parse(text = call))
    plotWindow <- grid.arrange(plotWindow, myLegend, ncol = 1, heights = c(D*10, 10))
  }
}

return(plotWindow) 
}

