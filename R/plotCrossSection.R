#' Plot Cross Section
#' 
#' Function to generate cross-sectional plots to visualize changes in the performance metrics of interest
#' as the proportion of missing data and gap width increase. Metrics are aggregated across K simulations in each p,g,d,m specification. \cr
#' The x-axis represents p, the proportion of missing data.\cr 
#' The y-axis represents g, the gap width.\cr
#' The z-axis represents the value of the performance metric of interest.
#' The user decides which variable to show with respect to the z-axis.
#' 
#' @param d A vector of the indexes of the datasets of interest
#' @param m A vector of the interpolation methods of interest (maximum of 5)
#' @param crit A character vector describing the performance metrics of interest
#' @param agEval A list object (result of agEval.R) of aggregated performance metrics
#' @param layer_type "method" (default) or "dataset"; how to slice the data
#' @param f "mean" or "median" (default); which statistic to use for f(p,g)
#' @param cross_section "p" or "g"; which axis to show against criterion value 

plotCS <- function(d=1:length(agEval), 
                        m=1:length(agEval[[1]][[1]][[1]]), 
                        crit, 
                        agEval, 
                        layer_type = "method", 
                        f = "median", 
                        cross_section = NULL){
  require(plotly)
  require(dplyr)
  require(RColorBrewer)
  
  stopifnot((layer_type == "method" || layer_type == "dataset"),
            (f == "mean" || f == "median"),
            (is.null(cross_section) || cross_section == "p" || cross_section == "g"))
  
  P <- length(agEval[[1]])
  G <- length(agEval[[1]][[1]])
  prop_vec_names <- names(agEval[[1]])
  gap_vec_names <- names(agEval[[1]][[1]])
  
  
  D <- length(d)
  M <- length(m)
  C <- length(crit)
  
  z_list <- lapply(z_list <- vector(mode = 'list', C),function(x)
    lapply(z_list <- vector(mode = 'list', M),function(x) 
      x <- vector(mode = 'list', D)))
  
  critMat <- matrix(nrow=length(agEval[[1]]),ncol=length(agEval[[1]][[1]]))
  rownames(critMat) <- prop_vec_names
  colnames(critMat) <- gap_vec_names
  
  data_list_names <- numeric(D)
  method_list_names <- numeric(M)
  
  for(s in 1:C){
    for(vm in 1:M){
      for(vd in 1:D){
        for(p in 1:P){
          for(g in 1:G){
            critMat[p,g] <- agEval[[d[vd]]][[p]][[g]][[m[vm]]][crit[s],f]
            method_list_names[vm] <- as.character(agEval[[d[vd]]][[p]][[g]][[m[vm]]][crit[s], "method"]) 
          }
        }
        z_list[[s]][[vm]][[vd]] <- critMat
        data_list_names[vd] <- paste("D",d[vd],sep="") 
      }
      names(z_list[[s]][[vm]]) <- data_list_names
    }
    names(z_list[[s]]) <- method_list_names
  }
  names(z_list) <- crit
  
  ## Generating a list of plots
  
  ## z_list[[criterion]][[method]][[dataset]]
  
  if(layer_type == "method"){ 
    colorList <- c("grey90","grey70","grey50","grey30","grey10","grey0")

    if(cross_section == "p"){
    axx <- prop_vec
    data <- z_list
    axxTitle <- "proportion missing"
    fun <- function(x){return(x)} 
    }
    else if(cross_section == "g"){
    axx <- gap_vec
    axxTitle <- "gap width" 
    data <- t(z_list)
    fun <- function(x){return(t(x))} 
    }
    
    plotList <- lapply(plotList <- vector(mode = 'list', C), function(x)
      x <- vector(mode = 'list', D)) 
    #plotList[[criterion]][[dataset]]
    
    for(s in 1:C){
      for(vd in 1:D){
        z <- numeric(M)
        for(vm in 1:(M-1)){
          z[vm] <- paste("geom_ribbon(data = data.frame(fun(data[[",s,"]][[",vm,"]][[",vd,"]])), 
                                                   aes(x = axx, ymin = apply(fun(data[[",s,"]][[",vm,"]][[",vd,"]]),1,min),
                                                   ymax = apply(fun(data[[",s,"]][[",vm,"]][[",vd,"]]),1,max)), 
                                                   fill = colorList[",vm,"], alpha = 0.2) + 

                          geom_line(data = data.frame(fun(data[[",s,"]][[",vm,"]][[",vd,"]])), 
                          aes(x = axx, y = apply(fun(data[[",s,"]][[",vm,"]][[",vd,"]]),1,median),
                              col = names(data[[",s,"]])[",vm,"])) + ",sep="") 
        }
        
        z[M] <- paste("geom_ribbon(data = data.frame(fun(data[[",s,"]][[",M,"]][[",vd,"]])), 
                                                   aes(x = axx, ymin = apply(fun(data[[",s,"]][[",M,"]][[",vd,"]]),1,min),
                                                   ymax = apply(fun(data[[",s,"]][[",M,"]][[",vd,"]]),1,max)),
                                                   fill = colorList[",M,"], alpha = 0.2) + 
                      
                      geom_line(data = data.frame(fun(data[[",s,"]][[",M,"]][[",vd,"]])), 
                      aes(x = axx, y = apply(fun(data[[",s,"]][[",M,"]][[",vd,"]]),1,median), 
                      col = names(data[[",s,"]])[",M,"]))",sep="")
      
        z <- paste(z, collapse = "")
        
        plotList[[s]][[vd]] <-  eval(parse(text = paste("ggplot() +", 
                                                        "theme_minimal() +",
                                                        z,sep="")))
        
        plotList[[s]][[vd]] <- plotList[[s]][[vd]] +  
                                ggtitle(paste("\n Criterion = ",names(z_list)[s]," (",f,")","\n Dataset = ",vd, sep = "")) + 
                                xlab(axxTitle) + ylab("value") + 
                                scale_colour_manual("", breaks = names(data[[s]])[1:M], values = colorList[1:M])
        
        }
      names(plotList[[s]]) <- data_list_names
      }
    names(plotList) <- crit
  }
  
  
  
  else if(layer_type == "dataset"){ 
    colorList <- c("grey90","grey70","grey50","grey30","grey10","grey0")
    
    if(cross_section == "p"){
      axx <- prop_vec
      data <- z_list
      axxTitle <- "proportion missing"
      fun <- function(x){return(x)} 
    }
    else if(cross_section == "g"){
      axx <- gap_vec
      axxTitle <- "gap width" 
      data <- t(z_list)
      fun <- function(x){return(t(x))} 
    }
    
    plotList <- lapply(plotList <- vector(mode = 'list', C), function(x)
      x <- vector(mode = 'list', M)) 
    #plotList[[criterion]][[method]]
    
    for(s in 1:C){
      for(vm in 1:M){
        z <- numeric(D)
        for(vd in 1:(D-1)){
          z[vd] <- paste("geom_ribbon(data = data.frame(fun(data[[",s,"]][[",vm,"]][[",vd,"]])), 
                                                   aes(x = axx, ymin = apply(fun(data[[",s,"]][[",vm,"]][[",vd,"]]),1,min),
                                                   ymax = apply(fun(data[[",s,"]][[",vm,"]][[",vd,"]]),1,max)), 
                                                   fill = colorList[",vd,"], alpha = 0.2) + 

                          geom_line(data = data.frame(fun(data[[",s,"]][[",vm,"]][[",vd,"]])), 
                          aes(x = axx, y = apply(fun(data[[",s,"]][[",vm,"]][[",vd,"]]),1,median),
                              col = names(data[[",s,"]][[",vm,"]])[",vd,"])) + ",sep="") 
        }
        
        z[D] <- paste("geom_ribbon(data = data.frame(fun(data[[",s,"]][[",vm,"]][[",D,"]])), 
                                                   aes(x = axx, ymin = apply(fun(data[[",s,"]][[",vm,"]][[",D,"]]),1,min),
                                                   ymax = apply(fun(data[[",s,"]][[",vm,"]][[",D,"]]),1,max)),
                                                   fill = colorList[",D,"], alpha = 0.2) + 
                      
                      geom_line(data = data.frame(fun(data[[",s,"]][[",vm,"]][[",D,"]])), 
                      aes(x = axx, y = apply(fun(data[[",s,"]][[",vm,"]][[",D,"]]),1,median), 
                      col = names(data[[",s,"]][[",vm,"]])[",D,"]))",sep="")
        
        z <- paste(z, collapse = "")
        
        plotList[[s]][[vm]] <-  eval(parse(text = paste("ggplot() +", 
                                                        "theme_minimal() +",
                                                        z,sep="")))
        
        plotList[[s]][[vm]] <- plotList[[s]][[vm]] +  
          ggtitle(paste("\n Criterion = ",names(z_list)[s]," (",f,")","\n Method = ",method_list_names[vm], sep = "")) + 
          xlab(axxTitle) + ylab("value") + 
          scale_colour_manual("", breaks = names(data[[s]][[vm]])[1:D], values = colorList[1:D])
        
      }
      names(plotList[[s]]) <- method_list_names
    }
    names(plotList) <- crit
  }
  
  return(plotList)
}


