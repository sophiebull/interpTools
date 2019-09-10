#' Plot Surface
#' 
#' Function to generate a surface plot to visualize average changes in the performance metrics of interest
#' as the proportion of missing data and gap width increase. Metrics are averaged across K simulations in each p,g,d,m specification. \cr
#' The x-axis represents p, the proportion of missing data.\cr
#' The y-axis represents g, the gap width.\cr
#' The z-axis represents the value of the performance metric of interest.
#' 
#' @param d A vector of the indexes of the datasets of interest
#' @param m A vector of the interpolation methods of interest (maximum of 5)
#' @param crit A character vector describing the performance metrics of interest
#' @param agEval A list object (result of agEval.R) of aggregated performance metrics

plotSurface <- function(d=1:length(agEval), m=1:length(agEval[[1]][[1]][[1]]), crit, agEval, layer_type = "method"){
  require(plotly)
  require(dplyr)
  require(RColorBrewer)
  
  stopifnot(length(m)<=5, (layer_type == "method" || layer_type == "dataset"))
  
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
            critMat[p,g] <- agEval[[d[vd]]][[p]][[g]][[m[vm]]][crit[s],"mean"]
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
      colorList <- list(c("#DA5526","#FEBC38"),
                        c("#B42F32","#DF6747"),
                        c("#FFECD2","#FCB69F"),
                        c("#FF9A9E","#FECFEF"),
                        c("#FEADA6","#F5EFEF"))
        
      for(i in 1:length(colorList)){
        palette[[i]] <- colorRampPalette(colorList[[i]])(P*G)
      }
    
      axx <- list(
        nticks = length(gap_vec),
        range = c(min(gap_vec),max(gap_vec))
      )
      
      axy <- list(
        nticks = length(prop_vec),
        range = c(min(prop_vec),max(prop_vec))
      )
      
    for(s in 1:C){
      for(vd in 1:D){
        z <- numeric(M)
        for(vm in 1:(M-1)){
          z[vm] <- paste("add_surface(x=gap_vec,y=prop_vec,z=z_list[[",s,"]][[",vm,"]][[",vd,"]], 
                         colorscale = list(seq(0,1,length.out=P*G), palette[[",vm,"]]), 
                         name = names(z_list[[1]])[",vm,"]) %>% ",sep="")
        }
        z[M] <- paste("add_surface(x=gap_vec,y=prop_vec,z=z_list[[",s,"]][[",M,"]][[",vd,"]], 
                      colorscale = list(seq(0,1,length.out=P*G), palette[[",M,"]]),
                      name = names(z_list[[1]])[",M,"])",sep="")
        
        z <- paste(z, collapse = "")
        
        plotList[[s]][[vd]] <- eval(parse(text = paste("plot_ly() %>%",
                                                       "layout(xaxis = axx, yaxis = axy) %>%",
                                                        z,sep="")))
        
        plotList[[s]][[vd]] <- plotList[[s]][[vd]] %>%  layout(title = paste("\n Criterion = ",names(z_list)[s],"\n Dataset = ",vd, sep = ""))
          
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
      colorList <- list(c("#DA5526","#FEBC38"),
                        c("#B42F32","#DF6747"),
                        c("#FFECD2","#FCB69F"),
                        c("#FF9A9E","#FECFEF"),
                        c("#FEADA6","#F5EFEF"))
      
      for(i in 1:length(colorList)){
        palette[[i]] <- colorRampPalette(colorList[[i]])(P*G)
      }
      
      axx <- list(
        nticks = length(gap_vec),
        range = c(min(gap_vec),max(gap_vec))
      )
      
      axy <- list(
        nticks = length(prop_vec),
        range = c(min(prop_vec),max(prop_vec))
      )
      
      for(s in 1:C){
        for(vm in 1:M){
          z <- numeric(D)
          for(vd in 1:(D-1)){
            z[vd] <- paste("add_surface(x=gap_vec,y=prop_vec,z=z_list[[",s,"]][[",vm,"]][[",vd,"]], 
                           colorscale = list(seq(0,1,length.out=P*G), palette[[",vd,"]]), 
                           name = names(z_list[[1]][[1]])[",vd,"]) %>% ",sep="")
          }
          z[D] <- paste("add_surface(x=gap_vec,y=prop_vec,z=z_list[[",s,"]][[",vm,"]][[",D,"]], 
                        colorscale = list(seq(0,1,length.out=P*G), palette[[",D,"]]),
                        name = names(z_list[[1]][[1]])[",D,"])",sep="")
          
          z <- paste(z, collapse = "")
          
          plotList[[s]][[vm]] <- eval(parse(text = paste("plot_ly() %>%",
                                                         "layout(xaxis = axx, yaxis = axy) %>%",
                                                         z,sep="")))
          
          plotList[[s]][[vm]] <- plotList[[s]][[vm]] %>%  layout(title = paste("\n Criterion = ",names(z_list)[s],"\n Method = ",method_list_names[vm], sep = ""))
          
          }
        names(plotList[[s]]) <- method_list_names
        }
      names(plotList) <- crit
    }  
  
  
return(plotList)
}
