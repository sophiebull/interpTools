multiCS <- function(agEval,
                    d=1:length(agEval), 
                    m=names(agEval[[1]][[1]][[1]]), 
                    crit, 
                    layer_type = "method", 
                    f = "median", 
                    cross_section = "p",
                    highlight = "HWI", 
                    highlight_color = "#EE5C42",
                    colors = c("#EAECEE", "#D5D8DC","#ABB2B9","#808B96", "#566573", "#2C3E50")){
  
  stopifnot(length(crit) == 1)
  
  D <- length(d)
  M <- length(m)
  
  # BUILD LEGEND ###########################################################################################
  
  z_list <- compileMatrix(agEval=agEval)[[f]]
  
  data_list_names = names(z_list[[1]][[1]])[d]
  method_list_names <- names(z_list[[1]])[names(z_list[[1]]) %in% m]
  
  if(layer_type == "method"){
    colorList <- colorRampPalette(colors)(M)
    
    colorListMatch <- colorList[1:M]
    names(colorListMatch) <- method_list_names
    colorListMatch[[highlight]] <- highlight_color
    
    nameSurface = method_list_names
    bound = M
    oth_bound = D
  }
  
  else if(layer_type == "dataset"){
    colorList <- colorRampPalette(colors)(D)
    
    colorListMatch <- colorList[1:D]
    names(colorListMatch) <- data_list_names
    colorListMatch[grepl(highlight, data_list_names)] <- highlight_color
    
    nameSurface = data_list_names
    bound = D
    oth_bound = M
  }
  
  manualLegend <- data.frame(color = as.character(colorListMatch), 
                             nameSurface = nameSurface, 
                             x = rep(0,bound), 
                             y = seq(0.20,0.80,length.out = bound), 
                             textcol = rep("black",bound),
                             stringsAsFactors = FALSE)
  
  theLegend <- ggplot(data = manualLegend) +
    
    geom_point(
      x = manualLegend$x, 
      y = manualLegend$y, 
      color = manualLegend$color,
      size = 5) +
    
    geom_text(x = manualLegend$x+0.05, 
              y = manualLegend$y,
              label = manualLegend$nameSurface,
              color = manualLegend$textcol,
              hjust = 0, size = 4) +
    
    xlim(-0.15,0.3) + 
    
    theme_void()
  
  # GENERATE PLOTS #######################################################################################
  
  pcs <- plotCS(agEval = agEval,
                       d = d, 
                       m = m, 
                       crit = crit, 
                       layer_type = layer_type, 
                       f = f, 
                       cross_section = cross_section,
                       highlight = highlight, 
                       highlight_color = highlight_color,
                       colors = colors)
  
  titleList <- list()
  
  if(layer_type == "method"){
    for(i in 1:oth_bound){
      titleList[[i]] <- paste0("Dataset ", gsub("D","",names(pcs[[1]])[i]))
    }
  }
  else if(layer_type == "dataset"){
    for(i in 1:oth_bound){
      titleList[[i]] <- gsub("Average","Avg", gsub("."," ", names(pcs[[1]])[i],fixed = TRUE))
    }
  }
  
  plotList <- list()
  
  for(i in 1:oth_bound){
    
    call <- paste0("pcs[[1]][[",i,"]] +")
    call <- paste0(c(call,paste0("theme(axis.title.x = element_blank(),
                                        axis.title.y = element_blank(),
                                        plot.title = element_text(hjust = 0.5), 
                                        plot.margin = unit(c(0.3,0.3,0.3,0.3), 'cm')) + labs(title = '",titleList[[i]],"')") ))
    
    plotList[[i]] <- eval(parse(text = call))
  }
  
  
  nrows = round_any(oth_bound, 2, f = ceiling)/2
    
  p_string <- paste0("plotList[[",1:(oth_bound-1),"]],")
  p_string <- c("grid.arrange(",p_string,paste0("plotList[[",oth_bound,"]]"),", theLegend, nrow = ",nrows,", 
                 top = textGrob('",paste0(names(pcs)[1]," (",f,")"),"', gp = gpar(fontsize = 14, font = 1)),
                 bottom = textGrob('proportion missing', gp = gpar(fontsize = 12, font = 1)),
                 left = textGrob('value', rot = 90, gp = gpar(fontsize = 12, font = 1)))")
  
  return(eval(parse(text = p_string)))
  
}
