#' Display Performance Criteria Across Method
#' 
#' Function to display the results of the performance analysis and produce an xtable() for input into LaTeX.
#' 
#' @param d An integer that is the index of the dataset of interest
#' @param m A vector of the interpolation methods of interest
#' @param crit A character string describing the performance metric of interest
#' @param agEval A list object (result of agEval.R) of aggregated performance metrics
#' @param f "mean" or "median" (default); which statistic to use for f(p,g)
#' @param cross_section "g" or "p"; specifies which variable to hold constant in the table (gap width or proportion missing)
#' @param fixedIndex An integer specifying the index position of the variable in "fixed" to hold constant in the table
#' @param collapse logical; Generate a table collapsing across the fixed variable (T), or select a fixed index (over 'fixed') (F)
#' 
bestTable <- function(d=1, 
                      agEval, 
                      m=1:length(agEval[[1]][[1]][[1]]), 
                      crit="MSE",
                      f = "median",
                      cross_section = "g",
                      layer_type = "method",
                      fixedIndex = NULL,
                      collapse = T){
  
  if (!(collapse) && is.null(fixedIndex)) {
    warning(paste("If you do not wish to collapse across ",fixed,
                  ", then you must specify an index at which to fix ", fixed,".",sep="" ))
    stop()
  }
  
  if(collapse && !(is.null(fixedIndex))){
    warning(paste("Since collapse = ",collapse,", fixedIndex is ignored.",sep=""))
  }
  
  if (layer_type == "method" && length(d) > 1) {
    warning(paste("If layer_type = ",layer_type,", d must have length = 1.",sep=""))
    stop()
  }
  
  if (layer_type == "dataset" && length(m) > 1) {
    warning(paste("If layer_type = ",layer_type,", m must have length = 1.",sep=""))
    stop()
  }
  
  
  stopifnot(layer_type == "method" | layer_type == "dataset")
  
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
  q2.5_list <- z_list
  q97.5_list <- z_list
  
  critMat <- matrix(nrow=length(agEval[[1]]),ncol=length(agEval[[1]][[1]]))
  rownames(critMat) <- prop_vec_names
  colnames(critMat) <- gap_vec_names
  
  q2.5mat <- critMat
  q97.5mat <- critMat
  
  data_list_names <- numeric(D)
  method_list_names <- numeric(M)
  
  for(s in 1:C){
    for(vm in 1:M){
      for(vd in 1:D){
        for(p in 1:P){
          for(g in 1:G){
            critMat[p,g] <- agEval[[d[vd]]][[p]][[g]][[m[vm]]][crit[s],f]
            q2.5mat[p,g] <- agEval[[d[vd]]][[p]][[g]][[m[vm]]][crit[s],"q2.5"]
            q97.5mat[p,g] <- agEval[[d[vd]]][[p]][[g]][[m[vm]]][crit[s],"q97.5"]
            
            method_list_names[vm] <- as.character(agEval[[d[vd]]][[p]][[g]][[m[vm]]][crit[s], "method"]) 
          }
        }
        z_list[[s]][[vm]][[vd]] <- critMat
        q2.5_list[[s]][[vm]][[vd]] <- q2.5mat
        q97.5_list[[s]][[vm]][[vd]] <- q97.5mat
        
        data_list_names[vd] <- paste("D",d[vd],sep="") 
      }
      names(z_list[[s]][[vm]]) <- data_list_names
      names(q2.5_list[[s]][[vm]]) <- data_list_names
      names(q97.5_list[[s]][[vm]]) <- data_list_names
    }
    names(z_list[[s]]) <- method_list_names
    names(q2.5_list[[s]]) <- method_list_names
    names(q97.5_list[[s]]) <- method_list_names
  }
  names(z_list) <- crit
  names(q2.5_list) <- crit
  names(q97.5_list) <- crit
  
  prop_vec <- names(agEval[[1]]) # proportions
  gap_vec <- names(agEval[[1]][[1]]) # gaps
  
  theTableList <- list()
  
  if(layer_type == "method"){
  if(!collapse){
    if(cross_section == "g"){
      
      for(p in 1:P){
        theTable <- matrix(nrow = M, ncol = 5)
        for(m in 1:M){
          theTable[m,] <- format(round(
            cbind(
              min(z_list[[crit]][[m]][[1]][,fixedIndex]),
              q2.5_list[[crit]][[m]][[1]][,fixedIndex][p],
              z_list[[crit]][[m]][[1]][,fixedIndex][p],
              q97.5_list[[crit]][[m]][[1]][,fixedIndex][p]),
            max(z_list[[crit]][[m]][[1]][,fixedIndex])
            ,2), nsmall = 2)
        }
        
        theTable[which.min(theTable[,2]),] <- paste0("\\textbf{", theTable[which.min(theTable[,2]),], "}")
        
        theTable <- cbind(gsub("."," ", method_list_names,fixed=TRUE), data.frame(theTable))
        colnames(theTable) = c("method", "min","$Q_{2.5\\%}$","median","$Q_{97.5\\%}$","max")
        
        theTableList[[p]] <- theTable
      }
      
      names(theTableList) <- paste("(",prop_vec_names,",",gap_vec_names[fixedIndex],")", sep = "")
    }
    
    else if(cross_section == "p"){
      
      for(g in 1:G){
        theTable <- matrix(nrow = M, ncol = 5)
        for(m in 1:M){
          theTable[m,] <- format(round(
            cbind(
              min(z_list[[crit]][[m]][[1]][fixedIndex,]),
              q2.5_list[[crit]][[m]][[1]][fixedIndex,][g],
              z_list[[crit]][[m]][[1]][fixedIndex,][g],
              q97.5_list[[crit]][[m]][[1]][fixedIndex,][g],
              max(z_list[[crit]][[m]][[1]][fixedIndex,]))
            ,2), nsmall = 2)
        }
        
        theTable[which.min(theTable[,2]),] <- paste0("\\textbf{", theTable[which.min(theTable[,2]),], "}")
        
        theTable <- cbind(gsub("."," ", method_list_names,fixed=TRUE), data.frame(theTable))
        colnames(theTable) = c("method", "min", "$Q_{2.5\\%}$","median","$Q_{97.5\\%}$","max")
        
        theTableList[[g]] <- theTable
      }
      
      names(theTableList) <- paste("(",prop_vec_names[fixedIndex],",",gap_vec_names,")", sep = "")
    }
  }
  
  else if(collapse){
    if(cross_section == "g"){
      
      for(p in 1:P){
        theTable <- matrix(nrow = M, ncol = 5)
        for(m in 1:M){
          theTable[m,] <- format(round(
            cbind(
              apply(z_list[[crit]][[m]][[1]],1,min)[p],
              apply(q2.5_list[[crit]][[m]][[1]],1,median)[p],
              apply(z_list[[crit]][[m]][[1]],1,median)[p],
              apply(q97.5_list[[crit]][[m]][[1]],1,median)[p],
              apply(z_list[[crit]][[m]][[1]],1,max)[p])
            ,2), nsmall = 2)
        }
        
        theTable[which.min(theTable[,2]),] <- paste0("\\textbf{", theTable[which.min(theTable[,2]),], "}")
        
        theTable <- cbind(gsub("."," ", method_list_names,fixed=TRUE), data.frame(theTable))
        colnames(theTable) = c("method", "min","$Q_{2.5\\%}$","median","$Q_{97.5\\%}$","max")
        
        theTableList[[p]] <- theTable
      }
      
      names(theTableList) <- prop_vec_names
    }
    
    else if(cross_section == "p"){
      
      for(g in 1:G){
        theTable <- matrix(nrow = M, ncol = 5)
        for(m in 1:M){
          theTable[m,] <- format(round(
            cbind(
              apply(q2.5_list[[crit]][[m]][[1]],2,min)[g],
              apply(q2.5_list[[crit]][[m]][[1]],2,median)[g],
              apply(z_list[[crit]][[m]][[1]],2,median)[g],
              apply(q97.5_list[[crit]][[m]][[1]],2,median)[g],
              apply(q2.5_list[[crit]][[m]][[1]],2,max)[g])
            ,2), nsmall = 2)
        }
        
        theTable[which.min(theTable[,2]),] <- paste0("\\textbf{", theTable[which.min(theTable[,2]),], "}")
        
        theTable <- cbind(gsub("."," ", method_list_names,fixed=TRUE), data.frame(theTable))
        colnames(theTable) = c("method", "min","$Q_{2.5\\%}$","median","$Q_{97.5\\%}$","max")
        
        theTableList[[g]] <- theTable
      }
      
      names(theTableList) <- gap_vec_names
    }
  }
  }
  
  
  else if(layer_type == "dataset"){
    if(!collapse){
      if(cross_section == "g"){
        
        for(p in 1:P){
          theTable <- matrix(nrow = D, ncol = 5)
          for(d in 1:D){
            theTable[d,] <- format(round(
              cbind(
                min(z_list[[crit]][[1]][[d]][,fixedIndex]),
                q2.5_list[[crit]][[1]][[d]][,fixedIndex][p],
                z_list[[crit]][[1]][[d]][,fixedIndex][p],
                q97.5_list[[crit]][[1]][[d]][,fixedIndex][p]),
              max(z_list[[crit]][[1]][[d]][,fixedIndex])
              ,2), nsmall = 2)
          }
          
          theTable[which.min(theTable[,2]),] <- paste0("\\textbf{", theTable[which.min(theTable[,2]),], "}")
          
          theTable <- cbind(gsub("."," ", data_list_names,fixed=TRUE), data.frame(theTable))
          colnames(theTable) = c("dataset", "min","$Q_{2.5\\%}$","median","$Q_{97.5\\%}$","max")
          
          theTableList[[p]] <- theTable
        }
        
        names(theTableList) <- paste("(",prop_vec_names,",",gap_vec_names[fixedIndex],")", sep = "")
      }
      
      else if(cross_section == "p"){
        
        for(g in 1:G){
          theTable <- matrix(nrow = D, ncol = 5)
          for(d in 1:D){
            theTable[d,] <- format(round(
              cbind(
                min(z_list[[crit]][[1]][[d]][fixedIndex,]),
                q2.5_list[[crit]][[1]][[d]][fixedIndex,][g],
                z_list[[crit]][[1]][[d]][fixedIndex,][g],
                q97.5_list[[crit]][[1]][[d]][fixedIndex,][g],
                max(z_list[[crit]][[1]][[d]][fixedIndex,]))
              ,2), nsmall = 2)
          }
          
          theTable[which.min(theTable[,2]),] <- paste0("\\textbf{", theTable[which.min(theTable[,2]),], "}")
          
          theTable <- cbind(gsub("."," ", data_list_names,fixed=TRUE), data.frame(theTable))
          colnames(theTable) = c("dataset", "min", "$Q_{2.5\\%}$","median","$Q_{97.5\\%}$","max")
          
          theTableList[[g]] <- theTable
        }
        
        names(theTableList) <- paste("(",prop_vec_names[fixedIndex],",",gap_vec_names,")", sep = "")
      }
    }
    
    else if(collapse){
      if(cross_section == "g"){
        
        for(p in 1:P){
          theTable <- matrix(nrow = D, ncol = 5)
          for(d in 1:D){
            theTable[m,] <- format(round(
              cbind(
                apply(z_list[[crit]][[1]][[d]],1,min)[p],
                apply(q2.5_list[[crit]][[1]][[d]],1,median)[p],
                apply(z_list[[crit]][[1]][[d]],1,median)[p],
                apply(q97.5_list[[crit]][[1]][[d]],1,median)[p],
                apply(z_list[[crit]][[1]][[d]],1,max)[p])
              ,2), nsmall = 2)
          }
          
          theTable[which.min(theTable[,2]),] <- paste0("\\textbf{", theTable[which.min(theTable[,2]),], "}")
          
          theTable <- cbind(gsub("."," ", data_list_names,fixed=TRUE), data.frame(theTable))
          colnames(theTable) = c("dataset", "min","$Q_{2.5\\%}$","median","$Q_{97.5\\%}$","max")
          
          theTableList[[p]] <- theTable
        }
        
        names(theTableList) <- prop_vec_names
      }
      
      else if(cross_section == "p"){
        
        for(g in 1:G){
          theTable <- matrix(nrow = D, ncol = 5)
          for(d in 1:D){
            theTable[d,] <- format(round(
              cbind(
                apply(q2.5_list[[crit]][[1]][[d]],2,min)[g],
                apply(q2.5_list[[crit]][[1]][[d]],2,median)[g],
                apply(z_list[[crit]][[1]][[d]],2,median)[g],
                apply(q97.5_list[[crit]][[1]][[d]],2,median)[g],
                apply(q2.5_list[[crit]][[1]][[d]],2,max)[g])
              ,2), nsmall = 2)
          }
          
          theTable[which.min(theTable[,2]),] <- paste0("\\textbf{", theTable[which.min(theTable[,2]),], "}")
          
          theTable <- cbind(gsub("."," ", data_list_names,fixed=TRUE), data.frame(theTable))
          colnames(theTable) = c("dataset", "min","$Q_{2.5\\%}$","median","$Q_{97.5\\%}$","max")
          
          theTableList[[g]] <- theTable
        }
        
        names(theTableList) <- gap_vec_names
      }
    } 
  }
  
  return(theTableList)
  
}










