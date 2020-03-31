plotCS_un <- function(agEval, 
                       cross_section = "p", 
                       crit, 
                       d, 
                       m = names(agEval[[1]][[1]][[1]]), 
                       f = "median", 
                       layer_type = "method", 
                       highlight = "HWI", highlight_color = "#FA4032",
                       colors = c("#F9E0AA","#F7C65B","#FAAF08","#FA812F","#FA4032","#F92111")){
  
  stopifnot(length(d)==1, length(crit)==1, (cross_section == "p" | cross_section == "g"), 
            class(agEval) == "agEvaluate", (layer_type == "method" | layer_type == "dataset"),
            length(highlight) == 1,
            highlight %in% names(agEval[[1]][[1]][[1]]),
            is.character(highlight_color))
  
  P <- length(agEval[[1]])
  G <- length(agEval[[1]][[1]])
  M <- length(m)
  
  prop_vec_names <- gsub("p","",names(agEval[[1]]), fixed = TRUE)
  gap_vec_names <- gsub("g","",names(agEval[[1]][[1]]), fixed = TRUE)
  method_vec_names <- m

  if(cross_section == "p"){
    bound = G
    xaxisTitle <- "proportion missing"
    yaxisTitle <- "gap width"
    y2axisTitle <- "value"
    names <- gap_vec_names
    unfixedNames <- prop_vec_names
    #fixedCol <- paste0("sub('\\(', '', substr(names(theTab)[",i,"], 1, regexpr('\\,', names(theTab)[",i,"])-1))")
  }
  
  else if(cross_section == "g"){
    bound = P
    xaxisTitle <- "gap width"
    yaxisTitle <- "proportion missing"
    y2axisTitle <- "value"
    names <- prop_vec_names
    unfixedNames <- gap_vec_names
    #fixedCol <- paste0("sub('\\)','', gsub('.*,','',names(theTab)[",i,"]))")
  }
  
  colorList <- colors
  
  colorListMatch <- colorList[1:M]
  names(colorListMatch) <- method_vec_names
  colorListMatch[highlight] <- highlight_color
  
  theTabList <- list()
  
  for(fi in 1:bound){
    
      theTab <- bestTable(agEval = agEval, m = m, d = d, 
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

  plotListCS <- list()
  
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
  
    # get legend
    g_legend<-function(a.gplot){
      tmp <- ggplot_gtable(ggplot_build(a.gplot))
      leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
      legend <- tmp$grobs[[leg]]
      return(legend)}
    
    # make dummy plot to retrieve legend
    dumPlot <- ggplot() + geom_errorbar(data = theTabList[[1]], aes(x = unfixed, ymin = Q2.5, ymax = Q97.5, color = method), width = 0.05)  + 
      scale_color_manual(values = colorListMatch) + 
      theme_minimal() +
      theme(legend.position = "bottom",
            legend.title = element_blank())
  
    mylegend<-g_legend(dumPlot)
    
  plotWindow <- do.call("grid.arrange", c(plotListCS, ncol = 1))
  
  plotWindow <- grid.arrange(plotWindow, 
                             left = textGrob(yaxisTitle, rot = 90, vjust = 0.5, gp = gpar(fontsize = 12)),
                             top = textGrob(paste(crit,", Dataset ",d,sep=""), gp=gpar(fontsize=15, lineheight = 3)),
                             bottom = textGrob(xaxisTitle, gp = gpar(fontsize = 12, lineheight = 3)),
                             right = textGrob(y2axisTitle, rot = 270, vjust = 0.5, gp = gpar(fontsize = 12)))

  plotWindow <- grid.arrange(plotWindow, mylegend, heights = c(10,1))
  
  return(plotWindow)
}
