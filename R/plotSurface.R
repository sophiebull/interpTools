#' Plot Surface
#' 
#' Function to generate a surface plot to visualize changes in the performance metrics of interest
#' as the proportion of missing data and gap width increase. Metrics are aggregated across K simulations in each p,g,d,m specification. \cr
#' The x-axis represents p, the proportion of missing data.\cr 
#' The y-axis represents g, the gap width.\cr
#' The z-axis represents the value of the performance metric of interest.
#' 
#' @param d A vector of the indexes of the datasets of interest
#' @param m A vector of the interpolation methods of interest (maximum of 5)
#' @param crit A character vector describing the performance metrics of interest
#' @param agEval A list object (result of agEval.R) of aggregated performance metrics
#' @param layer_type "method" (default) or "dataset"; how to slice the data
#' @param f "mean" or "median" (default); which statistic to use for f(p,g)

plotSurface <- function(d=1:length(agEval), 
                        m=1:length(agEval[[1]][[1]][[1]]), 
                        crit, 
                        agEval, 
                        layer_type = "method", 
                        f = "median", output = "plots"){
  require(plotly)
  require(dplyr)
  require(RColorBrewer)
  
  stopifnot((layer_type == "method" || layer_type == "dataset"),
            (f == "mean" || f == "median"))
  
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
  
  ## Generating a list of surfaces 
  
  prop_vec <- names(agEval[[1]]) # proportions
  gap_vec <- names(agEval[[1]][[1]]) # gaps
  
  if(layer_type == "method"){
  
      plotList <- lapply(plotList <- vector(mode = 'list', C), function(x)
                    x <- vector(mode = 'list', D))
      
      palette <- list()
      colorList <- list(c("grey90","grey90"),
                        c("grey70","grey70"),
                        c("grey50","grey50"),
                        c("grey30","grey30"),
                        c("grey10","grey10"),
                        c("grey0","grey0"))
        
      for(i in 1:length(colorList)){
        palette[[i]] <- colorRampPalette(colorList[[i]])(P*G)
      }
    
      axx <- list(
        nticks = length(gap_vec),
        range = c(min(gap_vec),max(gap_vec)),
        title = "gap width"
      )
      
      axy <- list(
        nticks = length(prop_vec),
        range = c(min(prop_vec),max(prop_vec)),
        title = "proportion missing"
      )
      
      axz <- list(title = "value")
      
    for(s in 1:C){
      for(vd in 1:D){
        z <- numeric(M)
        for(vm in 1:(M-1)){
          z[vm] <- paste("add_surface(x=gap_vec,y=prop_vec,z=z_list[[",s,"]][[",vm,"]][[",vd,"]], 
                         colorscale = list(seq(0,1,length.out=P*G), palette[[",vm,"]]), 
                         name = names(z_list[[1]])[",vm,"], opacity = 1) %>% ",sep="")
        }
        z[M] <- paste("add_surface(x=gap_vec,y=prop_vec,z=z_list[[",s,"]][[",M,"]][[",vd,"]], 
                      colorscale = list(seq(0,1,length.out=P*G), palette[[",M,"]]),
                      name = names(z_list[[1]])[",M,"], opacity = 1)",sep="")
        
        z <- paste(z, collapse = "")
        
        plotList[[s]][[vd]] <-  eval(parse(text = paste('plot_ly(scene="',paste("scene",vd,sep=""),'") %>%',
                                  "layout(xaxis = axx, yaxis = axy) %>%
                                    layout(",paste("scene",vd,sep=""),"= list(
                                      xaxis = list(title = ''),
                                      yaxis = list(title = ''),
                                      zaxis = list(title = '')
                                    )) %>%",z,sep="")))

        
        plotList[[s]][[vd]] <- plotList[[s]][[vd]] %>%  layout(title = paste("\n Criterion = ",names(z_list)[s]," (",f,")","\n Dataset = ",vd, sep = "")) 
        
        plotList[[s]][[vd]] <- hide_colorbar(plotList[[s]][[vd]])
        
        }
        names(plotList[[s]]) <- data_list_names
      }
    names(plotList) <- crit
    }

  else if(layer_type == "dataset"){
      # fix the method.
      # layers are by dataset. 
      
      plotList <- lapply(plotList <- vector(mode = 'list', C), function(x)
        x <- vector(mode = 'list', M))
      
      palette <- list()
      colorList <- list(c("grey90","grey90"),
                            c("grey70","grey70"),
                            c("grey50","grey50"),
                            c("grey30","grey30"),
                            c("grey10","grey10"))
      
      for(i in 1:length(colorList)){
        palette[[i]] <- colorRampPalette(colorList[[i]])(P*G)
      }
      
      axx <- list(
        nticks = length(gap_vec),
        range = c(min(gap_vec),max(gap_vec)),
        title = "gap width"
      )
      
      axy <- list(
        nticks = length(prop_vec),
        range = c(min(prop_vec),max(prop_vec)),
        title = "proportion missing"
      )
      
      axz <- list(title = "value")
      
      
      for(s in 1:C){
        for(vm in 1:M){
          z <- numeric(D)
          for(vd in 1:(D-1)){
            z[vd] <- paste("add_surface(x=gap_vec,y=prop_vec,z=z_list[[",s,"]][[",vm,"]][[",vd,"]], 
                           colorscale = list(seq(0,1,length.out=P*G), palette[[",vd,"]]),
                           name = names(z_list[[1]][[1]])[",vd,"], opacity = 1) %>% ",sep="")
          }
          z[D] <- paste("add_surface(x=gap_vec,y=prop_vec,z=z_list[[",s,"]][[",vm,"]][[",D,"]], 
                        colorscale = list(seq(0,1,length.out=P*G), palette[[",D,"]]),
                        name = names(z_list[[1]][[1]])[",D,"], opacity = 1)",sep="")
          
          z <- paste(z, collapse = "")
          
          plotList[[s]][[vm]] <- eval(parse(text = paste('plot_ly(scene="',paste("scene",vm,sep=""),'") %>%',
                                                         "layout(xaxis = axx, yaxis = axy) %>%
                                                         layout(",paste("scene",vm,sep=""),"= list(
                                                         xaxis = list(title = ''),
                                                         yaxis = list(title = ''),
                                                         zaxis = list(title = '')
                                                         )) %>%",
                                                         z,sep="")))
          
          plotList[[s]][[vm]] <- plotList[[s]][[vm]] %>%  layout(title = paste("\n Criterion = ",names(z_list)[s]," (",f,")","\n Method = ",method_list_names[vm], sep = ""))
          
          plotList[[s]][[vm]] <- hide_colorbar(plotList[[s]][[vm]])
          }
        names(plotList[[s]]) <- method_list_names
        }
      names(plotList) <- crit
    }  
  
  if(output == "plots"){
    return(plotList)
  }
  else if(output == "raw"){
    return(z_list)
 }  
  }


  

