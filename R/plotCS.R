#' Plot Cross Section
#' 
#' Function to generate cross-sectional plots to visualize changes in the performance metrics of interest
#' as the proportion of missing data and gap width increase. \cr
#' Metrics are aggregated across K simulations in each p,g,d,m specification, and plotted with respect to 
#' a particular variable (p or g), collapsing across the other.  \cr
#' Middle line is the median with respect to \code{f}, upper
#' and lower ribbon limits are \code{min} and \code{max}, respectively. \cr
#' The x-axis represents \code{p} or \code{g}, the proportion of missing data, or gap width.\cr 
#' The y-axis represents the value of the performance metric of interest, collapsed (median) across the `other variable'.
#' The user decides which variable to show on the x-axis.
#' 
#' @param d A vector of the indexes of the datasets of interest
#' @param m character; A vector of the interpolation methods of interest (maximum of 5)
#' @param crit A character vector describing the performance metrics of interest
#' @param agEval A list object (result of agEval.R) of aggregated performance metrics
#' @param layer_type "method" (default) or "dataset"; how to slice the data
#' @param f "median" (default); which statistic to use for f(p,g)
#' @param cross_section "p" or "g"; which axis to show against criterion value 

plotCS <- function(d=1:length(agEval), 
                        m=names(agEval[[1]][[1]][[1]]), 
                        crit, 
                        agEval, 
                        layer_type = "method", 
                        f = "median", 
                        cross_section = "p",
                        highlight = "HWI", highlight_colour = "#EE5C42"){
  require(plotly)
  require(dplyr)
  require(RColorBrewer)
  
  stopifnot((layer_type == "method" || layer_type == "dataset"),
            f %in% names(agEval[[1]][[1]][[1]][[1]])[1:11],
            (cross_section == "p" || cross_section == "g"),
            class(agEval) == "agEvaluate",
            length(d) <= length(agEval),
            length(m) <= length(agEval[[1]][[1]][[1]]),
            crit %in% rownames(agEval[[1]][[1]][[1]][[1]])
  )
  
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
    colorList <- c("#EAECEE", "#D5D8DC","#ABB2B9","#808B96", "#566573", "#2C3E50")
    
    colorListMatch <- colorList[1:M]
    names(colorListMatch) <- method_list_names
    colorListMatch[highlight] <- highlight_colour

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
          z[vm] <- paste("geom_ribbon(data = data.frame(fun(data[['",crit[s],"']][['",m[vm],"']][[",d[vd],"]])), 
                                                   color = colorListMatch['",m[vm],"'],
                                                   aes(x = as.factor(axx), ymin = apply(fun(data[['",crit[s],"']][['",m[vm],"']][[",d[vd],"]]),1,min),
                                                   ymax = apply(fun(data[['",crit[s],"']][['",m[vm],"']][[",d[vd],"]]),1,max),
                                                                                group = ",vm,"), 
                                                   fill = colorListMatch['",m[vm],"'], alpha = 0.4) + 

                          geom_line(data = data.frame(fun(data[['",crit[s],"']][['",m[vm],"']][[",d[vd],"]])),
                            colour = colorListMatch['",m[vm],"'],
                            aes(x = as.factor(axx), y = apply(fun(data[['",crit[s],"']][['",m[vm],"']][[",d[vd],"]]),1,median),
                            col = names(data[['",crit[s],"']])['",m[vm],"'], group = ",vm,")) +",
                         sep="")  
            
            
        }
        
        z[M] <- paste("geom_ribbon(data = data.frame(fun(data[['",crit[s],"']][['",m[M],"']][[",d[vd],"]])), 
                                                   color = colorListMatch['",m[M],"'],
                                                   aes(x = as.factor(axx), ymin = apply(fun(data[['",crit[s],"']][['",m[M],"']][[",d[vd],"]]),1,min),
                                                   ymax = apply(fun(data[['",crit[s],"']][['",m[M],"']][[",d[vd],"]]),1,max),
                                                                              group = ",M,"), 
                                                   fill = colorListMatch['",m[M],"'], alpha = 0.4) + 
                      
                      geom_line(data = data.frame(fun(data[['",crit[s],"']][['",m[M],"']][[",d[vd],"]])),
                      colour = colorListMatch['",m[M],"'],
                      aes(x = as.factor(axx), y = apply(fun(data[['",crit[s],"']][['",m[M],"']][[",d[vd],"]]),1,median), 
                      col = names(data[['",crit[s],"']])['",m[M],"'], group = ",M,"))",sep="")
      
        z <- paste(z, collapse = "")
        
        plotList[[s]][[vd]] <-  eval(parse(text = paste("ggplot() +", 
                                                        "theme_minimal() +",
                                                        z,sep="")))
        
        plotList[[s]][[vd]] <- plotList[[s]][[vd]] +  
                                ggtitle(paste("\n Criterion = ",names(z_list[crit[s]])," (",f,")","\n Dataset = ",d[vd], sep = "")) + 
                                xlab(axxTitle) + ylab("value") + 
                                scale_colour_manual("", breaks = names(data[[crit[s]]]), values = colorListMatch, labels = names(data[[crit[s]]]))  +
                                theme(panel.grid.major = element_blank(),
                                      panel.grid.minor = element_blank())
        
        }
      names(plotList[[s]]) <- data_list_names
      }
    names(plotList) <- crit
  }
  
  
  
  else if(layer_type == "dataset"){ 
    colorList <- c("#EAECEE", "#D5D8DC","#ABB2B9","#808B96", "#566573")
    
    colorListMatch <- colorList[1:D]
    names(colorListMatch) <- data_list_names
    colorListMatch[which(grepl(highlight,data_list_names))] <- highlight_colour
    
    if(cross_section == "p"){
      axx <- rownames(z_list[[1]][[1]][[1]]) #prop_vec
      data <- z_list
      axxTitle <- "proportion missing"
      fun <- function(x){return(x)} 
    }
    else if(cross_section == "g"){
      axx <- colnames(z_list[[1]][[1]][[1]]) #gap_vec
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
          z[vd] <- paste("geom_ribbon(data = data.frame(fun(data[['",crit[s],"']][['",m[vm],"']][[",d[vd],"]])), 
                                                   colour = colorListMatch[",vd,"],
                                                   aes(x = as.factor(axx), ymin = apply(fun(data[['",crit[s],"']][['",m[vm],"']][[",d[vd],"]]),1,min),
                                                   ymax = apply(fun(data[['",crit[s],"']][['",m[vm],"']][[",d[vd],"]]),1,max),
                                                                  group = ",vd,"), 
                                                   fill = colorListMatch[",vd,"], alpha = 0.4) + 

                          geom_line(data = data.frame(fun(data[['",crit[s],"']][['",m[vm],"']][[",d[vd],"]])),
                          colour = colorListMatch[",vd,"],
                          aes(x = as.factor(axx), y = apply(fun(data[['",crit[s],"']][['",m[vm],"']][[",d[vd],"]]),1,median),
                          col = names(data[['",crit[s],"']][['",m[vm],"']])[",d[vd],"], group = ",vd,")) + ",sep="") 
        }
        
        z[D] <- paste("geom_ribbon(data = data.frame(fun(data[['",crit[s],"']][['",m[vm],"']][[",d[D],"]])), 
                                                     colour = colorListMatch[",D,"],
                                                     aes(x = as.factor(axx), ymin = apply(fun(data[['",crit[s],"']][['",m[vm],"']][[",d[D],"]]),1,min),
                                                   ymax = apply(fun(data[['",crit[s],"']][['",m[vm],"']][[",d[D],"]]),1,max),
                                                                  group = ",D,"), 
                                                   fill = colorListMatch[",D,"], alpha = 0.4) + 
                      
                      geom_line(data = data.frame(fun(data[['",crit[s],"']][['",m[vm],"']][[",d[D],"]])),
                      colour = colorListMatch[",D,"],
                      aes(x = as.factor(axx), y = apply(fun(data[['",crit[s],"']][['",m[vm],"']][[",d[D],"]]),1,median), 
                      col = names(data[['",crit[s],"']][['",m[vm],"']])[",d[D],"], group = ",D,"))",sep="")
        
        z <- paste(z, collapse = "")
        
        plotList[[s]][[vm]] <-  eval(parse(text = paste("ggplot() +", 
                                                        "theme_minimal() +",
                                                        z,sep="")))
        
        plotList[[s]][[vm]] <- plotList[[s]][[vm]] +  
          ggtitle(paste("\n Criterion = ",names(z_list[crit[s]])," (",f,")","\n Method = ",method_list_names[vm], sep = "")) + 
          xlab(axxTitle) + ylab("value") + 
          scale_colour_manual("", breaks = names(data[[crit[s]]][[m[vm]]]), 
                                  values = colorListMatch, labels = names(data[[crit[s]]][[m[vm]]]))
        
      }
      names(plotList[[s]]) <- method_list_names
    }
    names(plotList) <- crit
  }
  
  return(plotList)
}


