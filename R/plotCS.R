#' Plot Collapsed Cross-sections by 'Gap Width' or 'Proportion Missing'
#' 
#' Function to generate collapsed cross-section plots to visualize changes in the performance metrics of interest
#' as the \strong{proportion of missing data} and \strong{gap width increase}. Instead of three axes, as in the surface plots, 
#' either \code{p} or \code{g} can be 'collapsed' such that changes in performance are depicted with respect to one variable, 
#' across all values of the other. This variable is specified in \code{cross_section}, and will form the x-axis of the cross-section plot.
#' \itemize{
#' \item The middle line is the \strong{median} value of the collapsed variable \cr
#' \item The upper ribbon boundary is the \strong{maximum} value \cr
#' \item The lower ribbon boundary is the \strong{minimum} value \cr
#' }
#' 
#' @param d \code{numeric}; A vector to indicate datasets of interest
#' @param m \code{character}; A vector of interpolation methods of interest
#' @param crit \code{character}; An element describing the performance metric of interest
#' @param agEval \code{agEvaluate}; An object containing the aggregated performance metrics (result of \code{agEvaluate()})
#' @param layer_type \code{character}; How to layer the ribbons (by "method" or by "dataset") 
#' @param f \code{character}; The statistic of interest defining the ribbon. Possible choices are listed in \code{?agEvaluate}
#' @param cross_section \code{character}; An element describing the gap structure variable to represent on the x-axis: either \code{"p"} or \code{"g"}
#' @param highlight \code{character/numeric}; A single method (if \code{layer_type = "method"}) or dataset (if \code{layer_type = "dataset"}) to highlight.
#' @param highlight_color \code{character}; An HTML color of format \code{"#xxxxxx"} to apply to \code{highlight}
#' @param colors \code{character}; A vector of the desired color palette, with entries in HTML format (\code{"#xxxxxx"}) 

plotCS <- function(d = 1:length(agEval), 
                   m=names(agEval[[1]][[1]][[1]]), 
                   crit, 
                   agEval, 
                   layer_type = "method", 
                   f = "median", 
                   cross_section = "p",
                   highlight = "HWI", highlight_color = "#EE5C42",
                   colors = c("#F9E0AA","#F7C65B","#FAAF08","#FA812F","#FA4032","#F92111")){
  
  ## LOGICAL CHECKS ############
  
  if(sum(duplicated(d) != 0)) stop(paste0("'d' contains redundant elements at position(s): ", paste0(c(1:length(d))[duplicated(d)], collapse = ", ") ))
  if(sum(duplicated(m) != 0)) stop(paste0("'m' contains redundant elements at position(s): ", paste0(c(1:length(m))[duplicated(m)], collapse = ", ") ))
  
  if(layer_type != "method" & layer_type != "dataset") stop("'layer_type' must equal either 'method' or 'dataset'.")
  if(cross_section != "p" & cross_section != "g") stop("'cross_section' must equal either 'p' or 'g'.")
  
  if(!all(f %in% names(agEval[[1]][[1]][[1]][[1]])[1:12])) stop(paste0(c("f must be one of: '",paste0(names(agEval[[1]][[1]][[1]][[1]])[1:12], collapse = "', '"),"'."), collapse = ""))
  if(!all(paste0("D",d) %in% names(agEval))) stop("Dataset(s) ", paste0(d[!paste0("D",d) %in% names(agEval)], collapse = ", ")," not found. Possible choices are: ", paste0(gsub("D", "",names(agEval)), collapse = ", "))
  if(!all(m %in%  names(agEval[[1]][[1]][[1]]))) stop("Method(s) '", paste0(m[!m %in% names(agEval[[1]][[1]][[1]])], collapse = ", ' "),"' not found. Possible choices are: '", paste0(names(agEval[[1]][[1]][[1]]), collapse = "', '"),"'.")
  if(!crit %in% rownames(agEval[[1]][[1]][[1]][[1]])) stop(paste0("Criterion '",crit,"' must be one of ", paste(rownames(agEval[[1]][[1]][[1]][[1]]),collapse = ", "),"."))
  
  if(class(agEval) != "agEvaluate") stop("'agEval' object must be of class 'agEvaluate'. Please use agEvaluate().")
  
  if(layer_type == "method" & !highlight %in% m) stop(paste0(c("'highlight' must be an element of 'm'. Choose one of: '", paste0(m, collapse = "', '"),"'."), collapse = ""))
  if(layer_type == "dataset" & !highlight %in% d) stop(paste0(c("'highlight' must be an element of 'd'. Choose one of: '", paste0(d, collapse = "', '"),"'."), collapse = ""))
  
  if(length(crit) != 1) stop("'crit' must contain only a single character element.")
  if(length(f) != 1) stop("'f' must contain only a single character element.")
  if(length(layer_type) != 1) stop("'layer_type' must contain only a single character element.")
  if(length(cross_section) != 1) stop("'cross_section' must contain only a single character element.")
  if(length(highlight) != 1) stop("'highlight' must contain only a single character element.")
  if(length(highlight_color) != 1) stop("'highlight_color' must contain only a single character element.")
  
  if(length(colors) < 2) stop("'colors' must contain at least two colors (each in HTML format: '#xxxxxx')")
  
  if(layer_type == "dataset" & !is.numeric(highlight)) stop("If 'layer_type' = 'dataset', then 'highlight' must be of class 'numeric'.")
  if(layer_type == "method" & !is.character(highlight)) stop("If 'layer_type' = 'method', then 'highlight' must be of class 'character'.")
  
  ##################

  P <- length(agEval[[1]])
  G <- length(agEval[[1]][[1]])
  prop_vec_names <- names(agEval[[1]])
  gap_vec_names <- names(agEval[[1]][[1]])
  
  
  D <- length(d)
  M <- length(m)
  C <- length(crit)
  
  mat <- compileMatrix(agEval)
  z_list <- mat[[f]]
  q2.5_list <- mat[["q2.5"]]
  q97.5_list <- mat[["q97.5"]]
  
  method_list_names <- m
  data_list_names <- names(z_list[[1]][[1]])[d]
  
  ## Generating a list of plots
  
  ## z_list[[criterion]][[method]][[dataset]]
  
  if(layer_type == "method"){ 
    colorList <- colors
    
    colorListMatch <- colorList[1:M]
    names(colorListMatch) <- method_list_names
    colorListMatch[highlight] <- highlight_color

    if(cross_section == "p"){
    axx <- as.numeric(gsub("p","", rownames(z_list[[1]][[1]][[1]]), fixed = TRUE)) #prop_vec
    data <- z_list
    axxTitle <- "proportion missing"
    fun <- function(x){return(x)} 
    }
    else if(cross_section == "g"){
    axx <- as.numeric(gsub("g","", colnames(z_list[[1]][[1]][[1]]), fixed = TRUE)) #gap_vec
    axxTitle <- "gap width" 
    data <- z_list
    fun <- function(x){return(t(x))} 


    }
    
    plotList <- lapply(plotList <- vector(mode = 'list', C), function(x)
      x <- vector(mode = 'list', D)) 
    #plotList[[criterion]][[dataset]]
    
    for(s in 1:C){
      for(vd in 1:D){
        z <- numeric(M)
        for(vm in 1:(M-1)){
          z[vm] <- paste0("geom_ribbon(data = data.frame(fun(data[['",crit[s],"']][['",m[vm],"']][[",d[vd],"]])),
                                                   aes(x = axx, ymin = apply(fun(data[['",crit[s],"']][['",m[vm],"']][[",d[vd],"]]),1,min),
                                                   ymax = apply(fun(data[['",crit[s],"']][['",m[vm],"']][[",d[vd],"]]),1,max),
                                                                                group = ",vm,"), 
                                                   fill = colorListMatch['",m[vm],"'], alpha = 0.4) + 

                          geom_line(data = data.frame(fun(data[['",crit[s],"']][['",m[vm],"']][[",d[vd],"]])),
                            color = colorListMatch['",m[vm],"'], alpha = 1,
                            aes(x = axx, y = apply(fun(data[['",crit[s],"']][['",m[vm],"']][[",d[vd],"]]),1,median),
                            col = names(data[['",crit[s],"']])['",m[vm],"'], group = ",vm,")) +")  
            
            
        }
        
        z[M] <- paste0("geom_ribbon(data = data.frame(fun(data[['",crit[s],"']][['",m[M],"']][[",d[vd],"]])),
                                                   aes(x = axx, ymin = apply(fun(data[['",crit[s],"']][['",m[M],"']][[",d[vd],"]]),1,min),
                                                   ymax = apply(fun(data[['",crit[s],"']][['",m[M],"']][[",d[vd],"]]),1,max),
                                                                              group = ",M,"), 
                                                   fill = colorListMatch['",m[M],"'], alpha = 0.4) + 
                      
                      geom_line(data = data.frame(fun(data[['",crit[s],"']][['",m[M],"']][[",d[vd],"]])),
                      color = colorListMatch['",m[M],"'], alpha = 1,
                      aes(x = axx, y = apply(fun(data[['",crit[s],"']][['",m[M],"']][[",d[vd],"]]),1,median), 
                      col = names(data[['",crit[s],"']])['",m[M],"'], group = ",M,"))")
      
        z <- paste(z, collapse = "")
        
        plotList[[s]][[vd]] <-  eval(parse(text = paste0("ggplot() +", 
                                                        "theme_light() + ylim(",
                                                        (1.2^(-1))*min(unlist(
                                                          lapply(z_list[[crit[s]]][m],function(x) lapply(x, min)))[grepl(d[vd],names(unlist(lapply(z_list[[crit[s]]][m],function(x) lapply(x, min)))))]
                                                          ),
                                                        ",",
                                                        1.2*max(unlist(
                                                          lapply(z_list[[crit[s]]][m],function(x) lapply(x, max)))[grepl(d[vd],names(unlist(lapply(z_list[[crit[s]]][m],function(x) lapply(x, max)))))]
                                                        ),") +",
                                                        z)))
        
        plotList[[s]][[vd]] <- plotList[[s]][[vd]] +  
                                ggtitle(paste("\n Criterion = ",names(z_list[crit[s]])," (",f,")","\n Dataset = ",d[vd], sep = "")) + 
                                xlab(axxTitle) + ylab("value") + 
                                scale_color_manual("", breaks = names(data[[crit[s]]]), values = colorListMatch, labels = names(data[[crit[s]]]))  +
                                theme(panel.grid.major = element_blank(),
                                      panel.grid.minor = element_blank())
        
        }
      names(plotList[[s]]) <- data_list_names
      }
    names(plotList) <- crit
  }
  
  
  
  else if(layer_type == "dataset"){ 
    colorList <- colors
    
    colorListMatch <- colorList[1:D]
    names(colorListMatch) <- data_list_names
    colorListMatch[which(grepl(highlight,data_list_names))] <- highlight_color
    
    if(cross_section == "p"){
      axx <- as.numeric(gsub("p","",rownames(z_list[[1]][[1]][[1]]))) #prop_vec
      data <- z_list
      axxTitle <- "proportion missing"
      fun <- function(x){return(x)} 
    }
    else if(cross_section == "g"){
      axx <- as.numeric(gsub("g","",colnames(z_list[[1]][[1]][[1]]))) #gap_vec
      axxTitle <- "gap width" 
      data <- z_list
      fun <- function(x){return(t(x))} 
    }
    
    plotList <- lapply(plotList <- vector(mode = 'list', C), function(x)
      x <- vector(mode = 'list', M)) 
    #plotList[[criterion]][[method]]
    
    for(s in 1:C){
      for(vm in 1:M){
        z <- numeric(D)
        for(vd in 1:(D-1)){
          z[vd] <- paste0("geom_ribbon(data = data.frame(fun(data[['",crit[s],"']][['",m[vm],"']][[",d[vd],"]])), 
                                                   aes(x = axx, ymin = apply(fun(data[['",crit[s],"']][['",m[vm],"']][[",d[vd],"]]),1,min),
                                                   ymax = apply(fun(data[['",crit[s],"']][['",m[vm],"']][[",d[vd],"]]),1,max),
                                                                  group = ",vd,"), 
                                                   fill = colorListMatch[",vd,"], alpha = 0.4) + 

                          geom_line(data = data.frame(fun(data[['",crit[s],"']][['",m[vm],"']][[",d[vd],"]])),
                          color = colorListMatch[",vd,"], alpha = 1,
                          aes(x = axx, y = apply(fun(data[['",crit[s],"']][['",m[vm],"']][[",d[vd],"]]),1,median),
                          col = names(data[['",crit[s],"']][['",m[vm],"']])[",d[vd],"], group = ",vd,")) + ") 
        }
        
        z[D] <- paste0("geom_ribbon(data = data.frame(fun(data[['",crit[s],"']][['",m[vm],"']][[",d[D],"]])), 
                                                     aes(x = axx, ymin = apply(fun(data[['",crit[s],"']][['",m[vm],"']][[",d[D],"]]),1,min),
                                                   ymax = apply(fun(data[['",crit[s],"']][['",m[vm],"']][[",d[D],"]]),1,max),
                                                                  group = ",D,"), 
                                                   fill = colorListMatch[",D,"], alpha = 0.4) + 
                      
                      geom_line(data = data.frame(fun(data[['",crit[s],"']][['",m[vm],"']][[",d[D],"]])),
                      color = colorListMatch[",D,"], alpha = 1,
                      aes(x = axx, y = apply(fun(data[['",crit[s],"']][['",m[vm],"']][[",d[D],"]]),1,median), 
                      col = names(data[['",crit[s],"']][['",m[vm],"']])[",d[D],"], group = ",D,"))")
        
        z <- paste(z, collapse = "")
        
        plotList[[s]][[vm]] <-  eval(parse(text = paste0("ggplot() +", 
                                                        "theme_light() + ylim(",
                                                        (1.2^(-1))*min(unlist(lapply(z_list[[crit[s]]][[m[vm]]], min))),
                                                        ",",
                                                        1.2*max(unlist(lapply(z_list[[crit[s]]][[m[vm]]], max))),") +",
                                                        z)))
        
        plotList[[s]][[vm]] <- plotList[[s]][[vm]] +  
          ggtitle(paste("\n Criterion = ",names(z_list[crit[s]])," (",f,")","\n Method = ",method_list_names[vm], sep = "")) + 
          xlab(axxTitle) + ylab("value") + 
          scale_color_manual("", breaks = names(data[[crit[s]]][[m[vm]]]), 
                                  values = colorListMatch, labels = names(data[[crit[s]]][[m[vm]]])) + 
          theme(panel.grid.major = element_blank(),
                panel.grid.minor = element_blank())
        
      }
      names(plotList[[s]]) <- method_list_names
    }
    names(plotList) <- crit
  }
  
  return(plotList)
}


