#' Arrange Multiple Collapsed Cross-section Plots on a Grid
#' 
#' Function to generate a grid of collapsed cross-section plots to faciliate comparison of the 'collapsed' performance metrics of interest
#' across \strong{method} or \strong{dataset}. Details of each individual plot structure in \code{?plotCS}.
#' @param agEval \code{agEvaluate}; An object containing the aggregated performance metrics (result of \code{agEvaluate()})
#' @param d \code{numeric}; A vector to indicate datasets of interest
#' @param m \code{character}; A vector of interpolation methods of interest
#' @param crit \code{character}; An element describing the performance metric of interest
#' @param layer_type \code{character}; How to layer the ribbons (by "method" or by "dataset") 
#' @param f \code{character}; The statistic of interest defining the ribbon. Possible choices are listed in \code{?agEvaluate}
#' @param cross_section \code{character}; An element describing the gap structure variable to represent on the x-axis: either \code{"p"} or \code{"g"}
#' @param highlight \code{character/numeric}; A single method (if \code{layer_type = "method"}) or dataset (if \code{layer_type = "dataset"}) to highlight.
#' @param highlight_color \code{character}; An HTML color of format \code{"#xxxxxx"} to apply to \code{highlight}
#' @param colors \code{character}; A vector of the desired color palette, with entries in HTML format (\code{"#xxxxxx"}) 

multiCS <- function(agEval,
                    d=1:length(agEval), 
                    m=names(agEval[[1]][[1]][[1]]), 
                    crit, 
                    layer_type = "method", 
                    f = "median", 
                    cross_section = "p",
                    highlight = "HWI", 
                    highlight_color = "#EE5C42",
                    colors = c("#F9E0AA","#F7C65B","#FAAF08","#FA812F","#FA4032","#F92111")){
  
## LOGICAL CHECKS ############
  
  if(sum(duplicated(d) != 0)) stop(paste0("'d' contains redundant elements at position(s): ", paste0(c(1:length(d))[duplicated(d)], collapse = ", ") ))
  if(sum(duplicated(m) != 0)) stop(paste0("'m' contains redundant elements at position(s): ", paste0(c(1:length(m))[duplicated(m)], collapse = ", ") ))
  
  if(layer_type == "method" & length(d) < 2) stop("Only one dataset was chosen. Please specify at least one more, or use 'plotCS()' instead.")
  if(layer_type == "dataset" & length(m) < 2) stop("Only one method was chosen. Please specify at least one more, or use 'plotCS()' instead.")
  
  if(!all(f %in% names(agEval[[1]][[1]][[1]][[1]])[1:12])) stop(paste0(c("f must be one of: '",paste0(names(agEval[[1]][[1]][[1]][[1]])[1:12], collapse = "', '"),"'."), collapse = ""))
  if(!all(paste0("D",d) %in% names(agEval))) stop("Dataset(s) ", paste0(d[!paste0("D",d) %in% names(agEval)], collapse = ", ")," not found. Possible choices are: ", paste0(gsub("D", "",names(agEval)), collapse = ", "))
  if(!all(m %in%  names(agEval[[1]][[1]][[1]]))) stop("Method(s) '", paste0(m[!m %in% names(agEval[[1]][[1]][[1]])], collapse = ", ' "),"' not found. Possible choices are: '", paste0(names(agEval[[1]][[1]][[1]]), collapse = "', '"),"'.")
  if(!crit %in% rownames(agEval[[1]][[1]][[1]][[1]])) stop(paste0("Criterion '",crit,"' must be one of ", paste(rownames(agEval[[1]][[1]][[1]][[1]]),collapse = ", "),"."))
  
  if(cross_section != "p" & cross_section != "g") stop("'cross_section' must equal either 'p' or 'g'.")
  if(layer_type != "method" & layer_type != "dataset") stop("'layer_type' must equal either 'method' or 'dataset'.")
  
  if(class(agEval) != "agEvaluate") stop("'agEval' object must be of class 'agEvaluate'. Please use agEvaluate().")
  
  if(layer_type == "method" & !highlight %in% m) stop(paste0(c("'highlight' must be an element of 'm'. Choose one of: '", paste0(m, collapse = "', '"),"'."), collapse = ""))
  if(layer_type == "dataset" & !highlight %in% d) stop(paste0(c("'highlight' must be an element of 'd'. Choose one of: '", paste0(d, collapse = "', '"),"'."), collapse = ""))
  
  if(length(crit) != 1) stop("'crit' must contain only a single character element.")
  if(length(f) != 1) stop("'f' must contain only a single character element.")
  if(length(layer_type) != 1) stop("'layer_type' must contain only a single character element.")
  if(length(cross_section) != 1) stop("'cross_section' must contain only a single character element.")
  if(length(highlight) != 1) stop("'highlight' must contain only a single character element.")
  if(length(highlight_color) != 1) stop("'highlight_color' must contain only a single character element.")
  
  if(length(colors) <2) stop("'colors' must contain at least two colors (each in HTML format: '#xxxxxx')")
  
  if(layer_type == "dataset" & !is.numeric(highlight)) stop("If 'layer_type' = 'dataset', then 'highlight' must be of class 'numeric'.")
  if(layer_type == "method" & !is.character(highlight)) stop("If 'layer_type' = 'method', then 'highlight' must be of class 'character'.")
  
  ##################
  
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
    
  
  if(cross_section == "p"){
    axxTitle = "proportion missing"
  }
  else if(cross_section == "g"){
    axxTitle = "gap width"
  }
    
  p_string <- paste0("plotList[[",1:(oth_bound-1),"]],")
  p_string <- c("grid.arrange(",p_string,paste0("plotList[[",oth_bound,"]]"),", theLegend, nrow = ",nrows,", 
                 top = textGrob('",paste0(names(pcs)[1]," (",f,")"),"', gp = gpar(fontsize = 14, font = 1)),
                 bottom = textGrob('",axxTitle,"', gp = gpar(fontsize = 12, font = 1)),
                 left = textGrob('value', rot = 90, gp = gpar(fontsize = 12, font = 1)))")
  
  return(eval(parse(text = p_string)))
  
}
