#' Plot Uncollapsed Cross-sections by 'Gap Width' or 'Proportion Missing'
#' 
#' Function to generate uncollapsed cross-section plots to visualize changes in the performance metrics of interest
#' as the \strong{proportion of missing data} and \strong{gap width increase}. Instead of three axes, as in the surface plots, 
#' either \code{p} or \code{g} can be broken down such that changes in performance are depicted with respect to one variable, 
#' across each value of the other. The variable to form the x-axis is specified in \code{cross_section}.
#' Plots are arranged vertically, where each represents performance plotted against each fixed value in the variable not specified in \code{cross_section}.
#' \itemize{
#' \item The middle line is the \strong{median} value \cr
#' \item The upper ribbon boundary is the \strong{97.5% quantile} value \cr
#' \item The lower ribbon boundary is the \strong{2.5% quantile} value \cr
#' }
#'
#' @param agEval \code{agEvaluate}; An object containing the aggregated performance metrics (result of \code{agEvaluate()})
#' @param cross_section \code{character}; An element describing the gap structure variable to represent on the x-axis: either \code{"p"} or \code{"g"}
#' @param crit \code{character}; An element describing the performance metric of interest
#' @param d \code{numeric}; A vector to indicate datasets of interest
#' @param m \code{character}; A vector of interpolation methods of interest
#' @param layer_type \code{character}; How to layer the ribbons (by "method" or by "dataset") 
#' @param f \code{character}; The statistic of interest defining the ribbon. Possible choices are listed in \code{?agEvaluate}
#' @param highlight \code{character/numeric}; A single method (if \code{layer_type = "method"}) or dataset (if \code{layer_type = "dataset"}) to highlight.
#' @param highlight_color \code{character}; An HTML color of format \code{"#xxxxxx"} to apply to \code{highlight}
#' @param colors \code{character}; A vector of the desired color palette, with entries in HTML format (\code{"#xxxxxx"}) 

plotCS_un <- function( agEval, 
                       cross_section = "p", 
                       crit, 
                       d = 1:length(agEval), 
                       m = names(agEval[[1]][[1]][[1]]), 
                       f = "median", 
                       layer_type = "method", 
                       highlight = "HWI", highlight_color = "#FA4032",
                       colors = c("#FF8633","#FFAF33","#FFD133","#FFEC33","#D7FF33","#96FF33")){
  
  ## LOGICAL CHECKS ############
  
  if(sum(duplicated(d) != 0)) stop(paste0("'d' contains redundant elements at position(s): ", paste0(c(1:length(d))[duplicated(d)], collapse = ", ") ))
  if(sum(duplicated(m) != 0)) stop(paste0("'m' contains redundant elements at position(s): ", paste0(c(1:length(m))[duplicated(m)], collapse = ", ") ))
  
  if(layer_type != "method" & layer_type != "dataset") stop("'layer_type' must equal either 'method' or 'dataset'.")
  if(cross_section != "p" & cross_section != "g") stop("'cross_section' must equal either 'p' or 'g'.")
  
  if(!all(f %in% names(agEval[[1]][[1]][[1]][[1]])[1:12])) stop(paste0(c("f must be one of: '",paste0(names(agEval[[1]][[1]][[1]][[1]])[1:12], collapse = "', '"),"'."), collapse = ""))
  if(!all(paste0("D",d) %in% names(agEval))) stop("Dataset ", paste0(d[!paste0("D",d) %in% names(agEval)], collapse = ", ")," not found. Possible choices are: ", paste0(gsub("D", "",names(agEval)), collapse = ", "))
  if(!all(m %in%  names(agEval[[1]][[1]][[1]]))) stop("Method(s) '", paste0(m[!m %in% names(agEval[[1]][[1]][[1]])], collapse = ", ' "),"' not found. Possible choices are: '", paste0(names(agEval[[1]][[1]][[1]]), collapse = "', '"),"'.")
  if(!crit %in% rownames(agEval[[1]][[1]][[1]][[1]])) stop(paste0("Criterion '",crit,"' must be one of ", paste(rownames(agEval[[1]][[1]][[1]][[1]]),collapse = ", "),"."))
  
  if(class(agEval) != "agEvaluate") stop("'agEval' object must be of class 'agEvaluate'. Please use agEvaluate().")

  if(!is.null(highlight)){
    if(length(highlight) != 1) stop("'highlight' must contain only a single character element.")
    if(length(highlight_color) != 1) stop("'highlight_color' must contain only a single character element.")
    
    if(layer_type == "method" & !highlight %in% m) stop(paste0(c("'highlight' must be an element of 'm'. Choose one of: '", paste0(m, collapse = "', '"),"'."), collapse = ""))
    if(layer_type == "dataset" & !highlight %in% d) stop(paste0(c("'highlight' must be an element of 'd'. Choose one of: '", paste0(d, collapse = "', '"),"'."), collapse = ""))
    
    if(layer_type == "dataset" & !is.numeric(highlight)) stop("If 'layer_type' = 'dataset', then 'highlight' must be of class 'numeric'.")
    if(layer_type == "method" & !is.character(highlight)) stop("If 'layer_type' = 'method', then 'highlight' must be of class 'character'.")
    
  }
  
  if(layer_type == "method" & length(d) != 1) stop("Sorry, only one dataset can be viewed at a time.")
  if(layer_type == "dataset" & length(m) != 1) stop("Sorry, only one method can be viewed at a time.")
  
  if(layer_type == "method" & length(m) <= 1) stop("Please specify at least two methods.")
  if(layer_type == "dataset" & length(m) <= 1) stop("Please specify at least two methods.")
  
  if(length(crit) != 1) stop("'crit' must contain only a single character element.")
  if(length(f) != 1) stop("'f' must contain only a single character element.")
  if(length(layer_type) != 1) stop("'layer_type' must contain only a single character element.")
  if(length(cross_section) != 1) stop("'cross_section' must contain only a single character element.")
  
  if(layer_type == "method" & length(m) > 1 & length(colors) < length(m)) warning(paste0("'colors' should contain at least ", length(m), " elements (each in HTML format: '#xxxxxx') if layering more than one method."))
  if(layer_type == "dataset" & length(d) > 1 & length(colors) < length(d)) warning(paste0("'colors' should contain at least ", length(d), " elements (each in HTML format: '#xxxxxx') if layering more than one dataset."))
  

  
  ##################

  

  ######
  
  
  P <- length(agEval[[1]])
  G <- length(agEval[[1]][[1]])
  M <- length(m)
  
  prop_vec_names <- gsub("p","",names(agEval[[1]]), fixed = TRUE)
  gap_vec_names <- gsub("g","",names(agEval[[1]][[1]]), fixed = TRUE)
  method_vec_names <- m
  data_vec_names <- paste0("D",d)
  
  if(cross_section == "p"){
    bound = G
    xaxisTitle <- "proportion missing"
    yaxisTitle <- "gap width"
    y2axisTitle <- "f(p)"
    names <- gap_vec_names
    unfixedNames <- prop_vec_names
    #fixedCol <- paste0("sub('\\(', '', substr(names(theTab)[",i,"], 1, regexpr('\\,', names(theTab)[",i,"])-1))")
  }
  
  else if(cross_section == "g"){
    bound = P
    xaxisTitle <- "gap width"
    yaxisTitle <- "proportion missing"
    y2axisTitle <- "f(g)"
    names <- prop_vec_names
    unfixedNames <- gap_vec_names
    #fixedCol <- paste0("sub('\\)','', gsub('.*,','',names(theTab)[",i,"]))")
  }
  
  
  if(layer_type == "method"){
    colorList <- colorRampPalette(colors)(M)
    colorListMatch <- colorList[1:M]
    names(colorListMatch) <- method_vec_names
  }
  else if(layer_type == "dataset"){
    colorList <- colorRampPalette(colors)(D)
    colorListMatch <- colorList[1:D]
    names(colorListMatch) <- data_vec_names
  }
  
  if(!is.null(highlight)){
  colorListMatch[highlight] <- highlight_color
  }
  
  theTabList <- list()
  
  if(layer_type == "method"){
    for(fi in 1:bound){
        theTab <- CStable(agEval = agEval, m = m, d = d, 
                          crit = crit, f = f, 
                          cross_section = cross_section, 
                          layer_type = layer_type, 
                          collapse = F, 
                          fixedIndex = fi)
        
          for(i in 1:length(theTab)){
            theTab[[i]] <- data.frame(lapply(theTab[[i]], as.character), stringsAsFactors=FALSE)
            theTab[[i]] <- apply(theTab[[i]], 1, FUN = function(x) gsub("}","", x, fixed = TRUE))
            theTab[[i]] <- apply(theTab[[i]], 1, FUN = function(x) gsub("\\textbf{","", x, fixed = TRUE))
            methods <- theTab[[i]][,1]
            theTab[[i]] <- data.frame(methods, apply(theTab[[i]][,2:4], 2, as.numeric),
                                      rep(unfixedNames[i], length(methods)),  
                                      stringsAsFactors = FALSE)
            
            names(theTab[[i]]) <- c("method", "Q2.5", "Q50", "Q97.5","unfixed") 
            
          }
      
        
      theTabList[[fi]] <- theTab 
      theTabList[[fi]] <- do.call(rbind, theTabList[[fi]]) # melt the list
    }
    
    names(theTabList) <- names
  }
  
  else if(layer_type == "dataset"){
    for(fi in 1:bound){
      theTab <- CStable(agEval = agEval, m = m, d = d, 
                        crit = crit, f = f, 
                        cross_section = cross_section, 
                        layer_type = layer_type, 
                        collapse = F, 
                        fixedIndex = fi)
      
      for(i in 1:length(theTab)){
        theTab[[i]] <- data.frame(lapply(theTab[[i]], as.character), stringsAsFactors=FALSE)
        theTab[[i]] <- apply(theTab[[i]], 1, FUN = function(x) gsub("}","", x, fixed = TRUE))
        theTab[[i]] <- apply(theTab[[i]], 1, FUN = function(x) gsub("\\textbf{","", x, fixed = TRUE))
        datasets <- theTab[[i]][,1]
        theTab[[i]] <- data.frame(datasets, apply(theTab[[i]][,2:4], 2, as.numeric),
                                  rep(unfixedNames[i], length(datasets)),  
                                  stringsAsFactors = FALSE)
        
        names(theTab[[i]]) <- c("dataset", "Q2.5", "Q50", "Q97.5","unfixed") 
        
      }
      
      theTabList[[fi]] <- theTab 
      theTabList[[fi]] <- do.call(rbind, theTabList[[fi]]) # melt the list
    }
    
    names(theTabList) <- names
  }
  
  plotListCS <- list()
  

  if(layer_type == "method"){
    for(fi in 1:(bound-1)){    
      plotListCS[[fi]] <- ggplot() + geom_errorbar(data = theTabList[[fi]], aes(x = unfixed, ymin = Q2.5, ymax = Q97.5, color = method), width = 0.05) + 
        geom_point(data = theTabList[[fi]], aes(x = unfixed, y = Q50, color = method))+ 
        geom_line(data = theTabList[[fi]], aes(x = unfixed, y = Q50, color = method, group = method), lwd = 0.3)+ 
        
        geom_ribbon(data = theTabList[[fi]], 
                    aes(x = unfixed, 
                        ymin = Q2.5,
                        ymax = Q97.5,
                        fill = method,
                        group = method),
                    alpha = 0.2) +
        
        scale_color_manual(values = colorListMatch) + 
        scale_fill_manual(values = colorListMatch) + 
        
        scale_y_continuous(breaks = round(seq(min(theTabList[[fi]]$Q2.5), max(theTabList[[fi]]$Q97.5), length.out = 5),0), 
                           sec.axis = dup_axis(),
                           position = "right")+
        
        theme_minimal() + 
        theme(plot.margin = unit(c(0,0.5,0,0.5),"cm"),
              panel.background = element_blank(),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              axis.text.x = element_blank(),
              axis.text.y.left = element_blank(),
              axis.title.y.right = element_blank(),
              legend.position = "none",
              axis.title.y.left =element_text(angle = 0, vjust = 0.5, margin = margin(t = 0, r = 20, b = 0, l = 0))) +
        
        labs(title="", x="", y = names[fi])
    }
    
    plotListCS[[bound]] <- ggplot() + geom_errorbar(data = theTabList[[bound]], aes(x = unfixed, ymin = Q2.5, ymax = Q97.5, color = method), width = 0.05) + 
      geom_point(data = theTabList[[bound]], aes(x = unfixed, y = Q50, color = method))+ 
      geom_line(data = theTabList[[bound]], aes(x = unfixed, y = Q50, color = method, group = method), lwd = 0.3)+ 
      geom_ribbon(data = theTabList[[bound]], 
                  aes(x = unfixed, 
                      ymin = Q2.5,
                      ymax = Q97.5,
                      fill = method,
                      group = method),
                  alpha = 0.2) +
      
      scale_color_manual(values = colorListMatch) + 
      scale_fill_manual(values = colorListMatch) +
      
      scale_y_continuous(breaks = round(seq(min(theTabList[[bound]]$Q2.5), max(theTabList[[bound]]$Q97.5), length.out = 5),0), 
                         sec.axis = dup_axis(),
                         position = "right")+ 
      
      theme_minimal() + 
      theme(plot.margin = unit(c(0,0.5,0,0.5),"cm"),
            panel.background = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            legend.position = "none",
            axis.title.x = element_blank(),
            axis.title.y.right = element_blank(),
            axis.text.y.left = element_blank(),
            axis.title.y.left =element_text(angle = 0, vjust = 0.5, margin = margin(t = 0, r = 20, b = 0, l = 0))) +
      
      labs(title="", x="", y = names[bound])
    
  }
    
  else if(layer_type == "dataset"){
    for(fi in 1:(bound-1)){    
      plotListCS[[fi]] <- ggplot() + geom_errorbar(data = theTabList[[fi]], aes(x = unfixed, ymin = Q2.5, ymax = Q97.5, color = dataset), width = 0.05) + 
        geom_point(data = theTabList[[fi]], aes(x = unfixed, y = Q50, color = dataset))+ 
        geom_line(data = theTabList[[fi]], aes(x = unfixed, y = Q50, color = dataset, group = dataset), lwd = 0.3)+ 
        
        geom_ribbon(data = theTabList[[fi]], 
                    aes(x = unfixed, 
                        ymin = Q2.5,
                        ymax = Q97.5,
                        fill = dataset,
                        group = dataset),
                    alpha = 0.2) +
        
        scale_color_manual(values = colorListMatch) + 
        scale_fill_manual(values = colorListMatch) + 
        
        scale_y_continuous(breaks = round(seq(min(theTabList[[fi]]$Q2.5), max(theTabList[[fi]]$Q97.5), length.out = 5),0), 
                           sec.axis = dup_axis(),
                           position = "right")+
        
        theme_minimal() + 
        theme(plot.margin = unit(c(0,0.5,0,0.5),"cm"),
              panel.background = element_blank(),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              axis.text.x = element_blank(),
              axis.text.y.left = element_blank(),
              axis.title.y.right = element_blank(),
              legend.position = "none",
              axis.title.y.left =element_text(angle = 0, vjust = 0.5, margin = margin(t = 0, r = 20, b = 0, l = 0))) +
        
        labs(title="", x="", y = names[fi])
    } 
    
    plotListCS[[bound]] <- ggplot() + geom_errorbar(data = theTabList[[bound]], aes(x = unfixed, ymin = Q2.5, ymax = Q97.5, color = dataset), width = 0.05) + 
      geom_point(data = theTabList[[bound]], aes(x = unfixed, y = Q50, color = dataset))+ 
      geom_line(data = theTabList[[bound]], aes(x = unfixed, y = Q50, color = dataset, group = dataset), lwd = 0.3)+ 
      geom_ribbon(data = theTabList[[bound]], 
                  aes(x = unfixed, 
                      ymin = Q2.5,
                      ymax = Q97.5,
                      fill = dataset,
                      group = dataset),
                  alpha = 0.2) +
      
      scale_color_manual(values = colorListMatch) + 
      scale_fill_manual(values = colorListMatch) +
      
      scale_y_continuous(breaks = round(seq(min(theTabList[[bound]]$Q2.5), max(theTabList[[bound]]$Q97.5), length.out = 5),0), 
                         sec.axis = dup_axis(),
                         position = "right")+ 
      
      theme_minimal() + 
      theme(plot.margin = unit(c(0,0.5,0,0.5),"cm"),
            panel.background = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            legend.position = "none",
            axis.title.x = element_blank(),
            axis.title.y.right = element_blank(),
            axis.text.y.left = element_blank(),
            axis.title.y.left =element_text(angle = 0, vjust = 0.5, margin = margin(t = 0, r = 20, b = 0, l = 0))) +
      
      labs(title="", x="", y = names[bound])  
    
  }
  

    # function to get legend
    g_legend<-function(a.gplot){
      tmp <- ggplot_gtable(ggplot_build(a.gplot))
      leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
      legend <- tmp$grobs[[leg]]
      return(legend)}

    
  plotWindow <- do.call("grid.arrange", c(plotListCS, ncol = 1))
  
  if(layer_type == "method"){
    # make dummy plot to retrieve legend
    dumPlot <- ggplot() + geom_errorbar(data = theTabList[[1]], aes(x = unfixed, ymin = Q2.5, ymax = Q97.5, color = method), width = 0.05)  + 
      scale_color_manual(values = colorListMatch) + 
      theme_minimal() +
      theme(legend.position = "bottom",
            legend.title = element_blank())
    
    plotWindow <- grid.arrange(plotWindow, 
                               left = textGrob(yaxisTitle, rot = 90, vjust = 0.5, gp = gpar(fontsize = 12)),
                               top = textGrob(paste0("f = ",crit," (",f ,") \n Dataset ",d), gp=gpar(fontsize=12, lineheight = 1)),
                               bottom = textGrob(xaxisTitle, gp = gpar(fontsize = 12, lineheight = 3)),
                               right = textGrob(y2axisTitle, rot = 270, vjust = 0.5, gp = gpar(fontsize = 12)))
  }
  
  else if(layer_type == "dataset"){
    # make dummy plot to retrieve legend
    dumPlot <- ggplot() + geom_errorbar(data = theTabList[[1]], aes(x = unfixed, ymin = Q2.5, ymax = Q97.5, color = dataset), width = 0.05)  + 
      scale_color_manual(values = colorListMatch) + 
      theme_minimal() +
      theme(legend.position = "bottom",
            legend.title = element_blank())
    
    plotWindow <- grid.arrange(plotWindow, 
                               left = textGrob(yaxisTitle, rot = 90, vjust = 0.5, gp = gpar(fontsize = 12)),
                               top = textGrob(paste0("f = ", crit," (",f ,") \n ",m), gp=gpar(fontsize=12, lineheight = 1)),
                               bottom = textGrob(xaxisTitle, gp = gpar(fontsize = 12, lineheight = 3)),
                               right = textGrob(y2axisTitle, rot = 270, vjust = 0.5, gp = gpar(fontsize = 12)))
  }
  
  mylegend<-g_legend(dumPlot)

  plotWindow <- grid.arrange(plotWindow, mylegend, heights = c(10,1))
  
  return(plotWindow)
}
