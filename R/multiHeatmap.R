#' Arrange Multiple Heatmaps on a Grid
#' 
#' Function to generate a grid of heatmaps to compare the performances of particular sets of interpolations across a set of __criteria__ or __methods__.
#' \itemize{
#' \item If \code{by = "crit"}, rows of heatmaps for a chosen method are arranged by \strong{criterion}. \cr
#' \item If \code{by = "method"}, rows of heatmaps for a chosen criterion are arranged by \strong{method}. \cr
#' }
#'
#' @param crit \code{character}; A vector describing the performance metrics of interest
#' @param agEval \code{agEvaluate}; An object containing the aggregated performance metrics (result of \code{agEvaluate()})
#' @param m \code{character}; A vector describing the interpolation methods of interest
#' @param by \code{character}; Either \code{"crit"} or \code{"method"}
#' @param f \code{character}; The statistic of interest that will be depicted by the heatmap. Possible choices are listed in \code{?agEvaluate}.
#' @param d \code{numeric}; A vector to indicate datasets of interest
#' @param colors \code{character}; A vector of the desired color palette, with entries in HTML format (\code{"#xxxxxx"}) 

multiHeatmap <- function(crit, 
                         agEval, 
                         m, 
                         by = "crit", 
                         f = "median", 
                         d = 1:5, 
                         colors = c("#F9E0AA","#F7C65B","#FAAF08","#FA812F","#FA4032","#F92111")){
  
  ## LOGICAL CHECKS ############
  
  if(sum(duplicated(d) != 0)) stop(paste0("'d' contains redundant elements at position(s): ", paste0(c(1:length(d))[duplicated(d)], collapse = ", ") ))
  if(sum(duplicated(m) != 0)) stop(paste0("'m' contains redundant elements at position(s): ", paste0(c(1:length(m))[duplicated(m)], collapse = ", ") ))
  if(sum(duplicated(crit) != 0)) stop(paste0("'crit' contains redundant elements at position(s): ", paste0(c(1:length(crit))[duplicated(crit)], collapse = ", ") ))

  if(by != "crit" & by != "method") stop("'by' must be either 'crit' or 'method'.")
  if(by == "crit" & length(crit) < 2) stop("Only one criterion was chosen. Please specify at least one more, or use 'heatmapGrid2()' instead.")
  if(by == "method" & length(m) < 2) stop("Only one method was chosen. Please specify at least one more, or use 'heatmapGrid2()' instead.")
  
  if(class(agEval) != "agEvaluate") stop("'agEval' object must be of class 'agEvaluate'. Please use agEvaluate().")
  
  if(by == "method" & length(crit) != 1) stop("'crit' must contain only a single character element if you wish to arrange by method.")
  
  if(length(f) != 1) stop("'f' must contain only a single character element.")
  if(length(by) != 1) stop("'by' must contain only a single character element.")
  
  if(by == "crit" & length(m) != 1) stop("'m' must contain only a single character element if you wish to arrange by criterion.")
  
  if(!all(m %in%  names(agEval[[1]][[1]][[1]]))) stop(paste0("Method(s) '", paste0(m[!m%in% names(agEval[[1]][[1]][[1]])], collapse = ", "), "' not found. Possible choices are: '", paste0(names(agEval[[1]][[1]][[1]]), collapse = "', '"),"'."))
  if(!all(paste0("D",d) %in% names(agEval))) stop("Dataset(s) ", paste0(d[!paste0("D",d) %in% names(agEval)], collapse = ", ")," not found. Possible choices are: ", paste0(gsub("D", "",names(agEval)), collapse = ", "))
  if(!all(f %in% names(agEval[[1]][[1]][[1]][[1]])[1:12])) stop(paste0(c("f must be one of: '",paste0(names(agEval[[1]][[1]][[1]][[1]])[1:12], collapse = "', '"),"'."), collapse = ""))
  if(!all(crit %in% rownames(agEval[[1]][[1]][[1]][[1]]))) stop(paste0("Criterion '",crit,"' must be one of ", paste(rownames(agEval[[1]][[1]][[1]][[1]]),collapse = ", "),"."))
  
  if(length(colors) <2) stop("'colors' must contain at least two colors (each in HTML format: '#xxxxxx')")
  
  ##################
  
  M <- length(m)
  C <- length(crit)
  D <- length(d)
  
  z_list <- compileMatrix(agEval)[[f]]
  
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
        labs(fill = paste0(crit[cr]," (",f,")"))
      
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
        labs(fill = paste0(crit," (",f,")"))
      
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

return(plotWindow) 
}

