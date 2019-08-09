#' Plot Surface
#' 
#' Function to generate a surface plot to visualize average changes in the performance metrics of interest
#' as the proportion of missing data and gap width increase. Metrics are averaged across K simulations in each p,g,d,m specification. \cr
#' The x-axis represents p, the proportion of missing data.\cr
#' The y-axis represents g, the gap width.\cr
#' The z-axis represents the value of the performance metric of interest.
#' 
#' @param d A vector of the indexes of the datasets of interest
#' @param m A vector of the interpolation methods of interest 
#' @param crit A character vector describing the performance metrics of interest
#' @param agEval A list object (result of agEval.R) of aggregated performance metrics

plotSurface <- function(d, m, crit, agEval){
  require(plotly)
  require(dplyr)
  
  #d <- c(1,3,4)
  #m <- c(1,4)
  #crit <- c("MSE","MAPE")
  
  P <- length(agEval[[1]])
  G <- length(agEval[[1]][[1]])
  
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
            critMat[p,g] <- ag[[d[vd]]][[p]][[g]][[m[vm]]][crit[s],"mean"]
            method_list_names[vm] <- as.character(ag[[d[vd]]][[p]][[g]][[m[vm]]][crit[s], "method"]) 
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
  
  plotList <- lapply(plotList <- vector(mode = 'list', C), function(x)
                x <- vector(mode = 'list', D))
  
  greyList <- list()
  col <- greyList

for(s in 1:C){
  for(vd in 1:D){
    z <- numeric(M)
    for(vm in 1:(M-1)){
      z[vm] <- paste("add_surface(x=prop_vec,y=gap_vec,z=z_list[[",s,"]][[",vm,"]][[",vd,"]], colorscale = col[[",vm,"]]) %>% ",sep="")
      greyList[[vm]] <- z_list[[s]][[vm]][[vd]]/max(z_list[[s]][[vm]][[vd]])
      col[[vm]] <- matrix(rgb(red=greyList[[vm]],blue=greyList[[vm]],green=greyList[[vm]]), ncol = G, nrow = P)
    }
    greyList[[M]] <- z_list[[s]][[M]][[vd]]/max(z_list[[s]][[M]][[vd]])
    col[[M]] <- matrix(rgb(red=greyList[[M]],blue=greyList[[M]],green=greyList[[M]]), ncol = G, nrow = P)
      
    z[M] <- paste("add_surface(x=prop_vec,y=gap_vec,z=z_list[[",s,"]][[",M,"]][[",vd,"]], colorscale = 'Viridis')",sep="")
    
    z <- paste(z, collapse = "")
    
    plotList[[s]][[vd]] <- eval(parse(text = paste("plot_ly(colorscale = 'Viridis') %>%",
                                                   "layout(title = names(z_list)[",s,"]) %>%",z,sep="")))
    }
    names(plotList[[s]]) <- data_list_names
  }
names(plotList) <- crit

}