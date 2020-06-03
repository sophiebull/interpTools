#' Arrange Multiple Surface Plots on a Grid
#' 
#' Function to generate a grid of surface plots (from the \code{plotly} package) to visualize changes in the performance metrics of interest
#' with gap structure, across a number of datasets or methods. The structure of each surface plot is described in \code{?plotSurface}. \cr \cr
#' \itemize{
#' \item If \code{layer_type = "method"}, plots are arranged in a grid, by \strong{dataset}. \cr
#' \item If \code{layer_type = "dataset"}, plots will be arranged in a grid, by \strong{method}. \cr
#' }
#' 
#' @param d \code{numeric}; A vector to indicate datasets of interest
#' @param m \code{character}; A vector of interpolation methods of interest
#' @param crit \code{character}; An element describing the performance metric of interest
#' @param agEval \code{agEvaluate}; An object containing the aggregated performance metrics (result of \code{agEvaluate()})
#' @param layer_type \code{character}; How to layer the surfaces (by "method" or by "dataset") 
#' @param f \code{character}; The statistic of interest defining the surface \code{f(p,g)}. Possible choices are listed in \code{?agEvaluate}.
#' @param highlight \code{character/numeric}; A single method (if \code{layer_type = "method"}) or dataset (if \code{layer_type = "dataset"}) to highlight.
#' @param highlight_color \code{character}; An HTML color of format \code{"#xxxxxx"} to apply to \code{highlight}
#' @param colors \code{character}; A vector of the desired color palette, with entries in HTML format (\code{"#xxxxxx"}) 
#' 

multiSurface <- function(d = 1:length(agEval),
                         m = names(agEval[[1]][[1]][[1]]),
                         crit,
                         agEval, 
                         layer_type = "method",
                         f = "median",
                         highlight = "HWI",
                         highlight_color = "#FA4032",
                         colors = c("#F9E0AA","#F7C65B","#FAAF08","#FA812F","#FA4032","#F92111")
                         ){
 
  ## LOGICAL CHECKS ############
  
  if(sum(duplicated(d) != 0)) stop(paste0("'d' contains redundant elements at position(s): ", paste0(c(1:length(d))[duplicated(d)], collapse = ", ") ))
  if(sum(duplicated(m) != 0)) stop(paste0("'m' contains redundant elements at position(s): ", paste0(c(1:length(m))[duplicated(m)], collapse = ", ") ))
  
  if(!all(m %in%  names(agEval[[1]][[1]][[1]]))) stop("Method(s) '", paste0(m[!m %in% names(agEval[[1]][[1]][[1]])], collapse = ", ' "),"' not found. Possible choices are: '", paste0(names(agEval[[1]][[1]][[1]]), collapse = "', '"),"'.")
  if(!all(paste0("D",d) %in% names(agEval))) stop("Dataset(s) ", paste0(d[!paste0("D",d) %in% names(agEval)], collapse = ", ")," not found. Possible choices are: ", paste0(gsub("D", "",names(agEval)), collapse = ", "))
  if(!all(f %in% names(agEval[[1]][[1]][[1]][[1]]))) stop(paste0(c("f must be one of: '",paste0(names(agEval[[1]][[1]][[1]][[1]]), collapse = "', '"),"'."), collapse = ""))
  if(!crit %in% rownames(agEval[[1]][[1]][[1]][[1]])) stop(paste0("Criterion '",crit,"' must be one of ", paste(rownames(agEval[[1]][[1]][[1]][[1]]),collapse = ", "),"."))
  
  if(length(crit) != 1) stop("'crit' must contain only a single character element.")
  if(length(f) != 1) stop("'f' must contain only a single character element.")
  if(length(layer_type) != 1) stop("'layer_type' must contain only a single character element.")
  if(length(highlight) != 1) stop("'highlight' must contain only a single character element.")
  if(length(highlight_color) != 1) stop("'highlight_color' must contain only a single character element.")
  
  if(layer_type != "method" & layer_type != "dataset") stop("'layer_type' must equal either 'method' or 'dataset'.")
  
  if(class(agEval) != "agEvaluate") stop("'agEval' object must be of class 'agEvaluate'. Please use agEvaluate().")
  
  if(layer_type == "method" & !highlight %in% m) stop(paste0(c("'highlight' must be an element of 'm'. Choose one of: '", paste0(m, collapse = "', '"),"'."), collapse = ""))
  if(layer_type == "dataset" & !highlight %in% d) stop(paste0(c("'highlight' must be an element of 'd'. Choose one of: '", paste0(d, collapse = "', '"),"'."), collapse = ""))
  
  if(length(colors) <2) stop("'colors' must contain at least two colors (each in HTML format: '#xxxxxx')")
  
  if(layer_type == "dataset" & !is.numeric(highlight)) stop("If 'layer_type' = 'dataset', then 'highlight' must be of class 'numeric'.")
  if(layer_type == "method" & !is.character(highlight)) stop("If 'layer_type' = 'method', then 'highlight' must be of class 'character'.")
  
  if(layer_type == "method" & length(d) < 2) stop("Only 1 dataset was chosen. Please specify at least one more, or use plotSurface() instead.") 
  if(layer_type == "dataset" & length(m) < 2) stop("Only 1 method was chosen. Please specify at least one more, or use plotSurface() instead.") 
  
  if(layer_type == "method" & length(d) < 3){ warning("multiSurface() layout works best with at least three plots. Next time, specify at least 3 elements in 'd'.")}
  if(layer_type == "dataset" & length(m) < 3){ warning("multiSurface() layout works best with at least three plots. Next time, specify at least 3 elements in 'm'.")}
  
  D <- length(d)
  M <- length(m)
  
  ##################
  
  # BUILD LEGEND ###########################################################################
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
                             y = seq(0.20,0.80,length.out =bound), 
                             textcol = rep("black",bound),
                             stringsAsFactors = FALSE)
  
  theLegend <- plot_ly(data = manualLegend, showlegend = F) %>%
    
    add_markers(x = manualLegend$x,
                y = manualLegend$y, 
                size = 50, 
                name = manualLegend$nameSurface,
                color = I(manualLegend$color)) %>%
    
    add_text(x = manualLegend$x + 0.03,
             y = manualLegend$y,
             text = manualLegend$nameSurface,
             color = I(manualLegend$textcol),
             textposition = "right") %>% 
    
    layout(xaxis = list(title = "", 
                        showgrid = F, 
                        showticklabels = F,
                        zeroline = F,
                        range = c(-0.15,0.3)), 
           yaxis = list(title = "", 
                        showgrid = F, 
                        showticklabels = F,
                        zeroline = F,
                        range = c(0,1.05)))
  
  
  # GENERATE PLOTS #######################################################################################
  
  ps <- plotSurface(crit=crit, 
                    agEval = agEval, 
                    layer_type = layer_type,
                    f = f,
                    m = m, 
                    d = d,
                    highlight = highlight,
                    highlight_color = highlight_color,
                    colors = colors)
  
  titleList <- list()
  
  if(layer_type == "method"){
    for(i in 1:oth_bound){
      titleList[[i]] <- list(
        text = paste("Dataset ",gsub("D","",names(ps[[1]])[i]),sep=""),
        xref = "paper",
        yref = "paper",
        yanchor = "bottom",
        xanchor = "center",
        align = "center",
        x = 0.5,
        y = 0.75,
        showarrow = FALSE
      )
    }
  }
  
  else if(layer_type == "dataset"){
    
    for(i in 1:oth_bound){
      titleList[[i]] <- list(
        text = gsub("Average","Avg", gsub("."," ", names(ps[[1]])[i],fixed = TRUE)),
        xref = "paper",
        yref = "paper",
        yanchor = "bottom",
        xanchor = "center",
        align = "center",
        x = 0.5,
        y = 0.75,
        showarrow = FALSE
      )
    }
  }
  
  plotList <- list()

  for(i in 1:oth_bound){
    plotList[[i]] <- ps[[1]][[i]] %>% layout(annotations = titleList[[i]])
  }
  
  nrows = round_any(oth_bound, 2, f = ceiling)/2
  
  p_string <- paste0("plotList[[",1:(oth_bound-1),"]],")
  p_string <- paste0(c("subplot(",p_string,"plotList[[",oth_bound,"]], theLegend, nrows = ",nrows,") %>%"), collapse = "")

  x1 <- rep(c(0,1/2), nrows)
  x2 <- rep(c(1/2,1), nrows)
  
  y1 <- rep(((nrows-1):0)/nrows, each = 2)
  y2 <- rep((nrows:1)/nrows, each = 2)
  
  scene_string <- paste0("scene",2:(oth_bound+1))
  scene_string <- c("scene",scene_string)
  
  
  l_string <- paste0("layout(title = crit,")
  
  l_string <- c(l_string, paste0(scene_string[1:oth_bound]," = list(domain = list(x = c(",x1[1:oth_bound],",",x2[1:oth_bound],"), y = c(",y1[1:oth_bound],",",y2[1:oth_bound],")),
                                 aspectmode = 'manual',
                                 aspectratio = list(x = 1, y= 1, z= 0.8),
                                 camera = list(eye = list(x = 1.65, y = -1.15, z = 0.30))),"))
  l_string <- c(l_string, paste0(scene_string[oth_bound+1], " = list(domain = list(x = c(",x1[oth_bound+1],",",x2[oth_bound+1],"), y = c(",y1[oth_bound+1],",",y2[oth_bound+1],"))))"))

  call <- paste0(c(p_string,l_string))
  
  return(eval(parse(text = call)))
}
