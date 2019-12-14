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
  
  if(length(crit) > 1){
    warning("Variable crit can only be of length 1.")
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
  min_list <- z_list
  max_list <- z_list
  
  critMat <- matrix(nrow=length(agEval[[1]]),ncol=length(agEval[[1]][[1]]))
  rownames(critMat) <- prop_vec_names
  colnames(critMat) <- gap_vec_names
  
  q2.5mat <- critMat
  q97.5mat <- critMat
  minmat <- critMat
  maxmat <- critMat
  
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
            minmat[p,g] <- agEval[[d[vd]]][[p]][[g]][[m[vm]]][crit[s],"q0"]
            maxmat[p,g] <- agEval[[d[vd]]][[p]][[g]][[m[vm]]][crit[s],"q100"]
            
            method_list_names[vm] <- as.character(agEval[[d[vd]]][[p]][[g]][[m[vm]]][crit[s], "method"]) 
          }
        }
        z_list[[s]][[vm]][[vd]] <- critMat
        q2.5_list[[s]][[vm]][[vd]] <- q2.5mat
        q97.5_list[[s]][[vm]][[vd]] <- q97.5mat
        min_list[[s]][[vm]][[vd]] <- minmat
        max_list[[s]][[vm]][[vd]] <- maxmat
        
        data_list_names[vd] <- paste("D",d[vd],sep="") 
      }
      names(z_list[[s]][[vm]]) <- data_list_names
      names(q2.5_list[[s]][[vm]]) <- data_list_names
      names(q97.5_list[[s]][[vm]]) <- data_list_names
      names(min_list[[s]][[vm]]) <- data_list_names
      names(max_list[[s]][[vm]]) <- data_list_names
    }
    names(z_list[[s]]) <- method_list_names
    names(q2.5_list[[s]]) <- method_list_names
    names(q97.5_list[[s]]) <- method_list_names
    names(min_list[[s]]) <- method_list_names
    names(max_list[[s]]) <- method_list_names
  }
  names(z_list) <- crit
  names(q2.5_list) <- crit
  names(q97.5_list) <- crit
  names(min_list) <- crit
  names(max_list) <- crit
  
  prop_vec <- names(agEval[[1]]) # proportions
  gap_vec <- names(agEval[[1]][[1]]) # gaps
  
  criterion <- rownames(agEval[[1]][[1]][[1]][[1]])
  maximize <- c(1,1,rep(0,11),1,rep(0,4)) # 1 = yes, 0 = no
  optimal <- maximize
  optimal[which(optimal == "1")] <- "max"
  optimal[which(optimal == "0")] <- "min"
  
  best <- data.frame(criterion = criterion, 
                     maximize = maximize,
                     optimal = optimal) 
  
  optimize <- best$maximize[best$criterion == crit]
  
  theTableList <- list()
  
  if(layer_type == "method"){
  if(!collapse){
    if(cross_section == "g"){
      
      for(p in 1:P){
        theTable <- matrix(nrow = M, ncol = 6)
        for(m in 1:M){
          theTable[m,] <- format(round(
            cbind(
              min_list[[crit]][[m]][[1]][,fixedIndex][p],
              q2.5_list[[crit]][[m]][[1]][,fixedIndex][p],
              z_list[[crit]][[m]][[1]][,fixedIndex][p],
              q97.5_list[[crit]][[m]][[1]][,fixedIndex][p],
              max_list[[crit]][[m]][[1]][,fixedIndex][p], 
              q97.5_list[[crit]][[m]][[1]][,fixedIndex][p] - q2.5_list[[crit]][[m]][[1]][,fixedIndex][p]) # IQR
            ,2), nsmall = 2)
        }
        
        if(optimize == 0){
          theTable[which.min(theTable[,4]),] <- paste0("\\textbf{", theTable[which.min(theTable[,4]),], "}")
        }
        
        else if(optimize == 1){
          theTable[which.max(theTable[,4]),] <- paste0("\\textbf{", theTable[which.max(theTable[,4]),], "}")
        }
        
        theTable <- cbind(gsub("Exponential Weighted Moving Average", "Exp. Weighted Moving Avg.",
                               gsub("."," ", method_list_names,fixed=TRUE), fixed = TRUE), data.frame(theTable))
        colnames(theTable) = c("method", "min","$Q_{2.5\\%}$","median","$Q_{97.5\\%}$","max", "$Q_{97.5\\%} - Q_{2.5\\%}$")
        
        theTableList[[p]] <- theTable
      }
      
      names(theTableList) <- paste("(",prop_vec_names,",",gap_vec_names[fixedIndex],")", sep = "")
    }
    
    else if(cross_section == "p"){
      
      for(g in 1:G){
        theTable <- matrix(nrow = M, ncol = 6)
        for(m in 1:M){
          theTable[m,] <- format(round(
            cbind(
              min_list[[crit]][[m]][[1]][fixedIndex,][g],
              q2.5_list[[crit]][[m]][[1]][fixedIndex,][g],
              z_list[[crit]][[m]][[1]][fixedIndex,][g],
              q97.5_list[[crit]][[m]][[1]][fixedIndex,][g],
              max_list[[crit]][[m]][[1]][fixedIndex,][g],
              q97.5_list[[crit]][[m]][[1]][fixedIndex,][g] - q2.5_list[[crit]][[m]][[1]][fixedIndex,][g])
            ,2), nsmall = 2)
        }
        
        if(optimize == 0){
          theTable[which.min(theTable[,4]),] <- paste0("\\textbf{", theTable[which.min(theTable[,4]),], "}")
        }
        
        else if(optimize == 1){
          theTable[which.max(theTable[,4]),] <- paste0("\\textbf{", theTable[which.max(theTable[,4]),], "}")
        }
        
        theTable <- cbind(gsub("Exponential Weighted Moving Average", "Exp. Weighted Moving Avg.",
                               gsub("."," ", method_list_names,fixed=TRUE), fixed = TRUE), data.frame(theTable))
        colnames(theTable) = c("method", "min", "$Q_{2.5\\%}$","median","$Q_{97.5\\%}$","max", "$Q_{97.5\\%} - Q_{2.5\\%}$")
        
        theTableList[[g]] <- theTable
      }
      
      names(theTableList) <- paste("(",prop_vec_names[fixedIndex],",",gap_vec_names,")", sep = "")
    }
  }
  
  else if(collapse){ # SAMPLING DISTRIBUTION OF THE SAMPLE MEDIANS
    if(cross_section == "g"){
      
      for(p in 1:P){
        theTable <- matrix(nrow = M, ncol = 6)
        for(m in 1:M){
          theTable[m,] <- format(round(
            cbind(
              apply(z_list[[crit]][[m]][[1]],1,min)[p],
              apply(z_list[[crit]][[m]][[1]],1,function (x) quantile(x, probs = c(0.025,0.975))["2.5%"])[p],
              apply(z_list[[crit]][[m]][[1]],1,median)[p],
              apply(z_list[[crit]][[m]][[1]],1,function (x) quantile(x, probs = c(0.025,0.975))["97.5%"])[p],
              apply(z_list[[crit]][[m]][[1]],1,max)[p],
              apply(z_list[[crit]][[m]][[1]],1,function (x) quantile(x, probs = c(0.025,0.975))["97.5%"])[p] - 
                apply(z_list[[crit]][[m]][[1]],1,function (x) quantile(x, probs = c(0.025,0.975))["2.5%"])[p])
            ,2), nsmall = 2)
        }
        
        if(optimize == 0){
          theTable[which.min(theTable[,4]),] <- paste0("\\textbf{", theTable[which.min(theTable[,4]),], "}")
        }
        
        else if(optimize == 1){
          theTable[which.max(theTable[,4]),] <- paste0("\\textbf{", theTable[which.max(theTable[,4]),], "}")
        }
        
        theTable <- cbind(gsub("Exponential Weighted Moving Average", "Exp. Weighted Moving Avg.",
                               gsub("."," ", method_list_names,fixed=TRUE), fixed = TRUE), data.frame(theTable))
        colnames(theTable) = c("method", "min","$Q_{2.5\\%}$","median","$Q_{97.5\\%}$","max","$Q_{97.5\\%} - Q_{2.5\\%}$")
        
        theTableList[[p]] <- theTable
      }
      
      names(theTableList) <- prop_vec_names
    }
    
    else if(cross_section == "p"){
      
      for(g in 1:G){
        theTable <- matrix(nrow = M, ncol = 6)
        for(m in 1:M){
          theTable[m,] <- format(round(
            cbind(
              apply(z_list[[crit]][[m]][[1]],2,min)[g],
              apply(z_list[[crit]][[m]][[1]],2,function (x) quantile(x, probs = c(0.025,0.975))["2.5%"])[g],
              apply(z_list[[crit]][[m]][[1]],2,median)[g],
              apply(z_list[[crit]][[m]][[1]],2,function (x) quantile(x, probs = c(0.025,0.975))["97.5%"])[g],
              apply(z_list[[crit]][[m]][[1]],2,max)[g],
              apply(z_list[[crit]][[m]][[1]],2,function (x) quantile(x, probs = c(0.025,0.975))["97.5%"])[g] - 
                apply(z_list[[crit]][[m]][[1]],2,function (x) quantile(x, probs = c(0.025,0.975))["2.5%"])[g])
            ,2), nsmall = 2)
        }
        
        if(optimize == 0){
          theTable[which.min(theTable[,4]),] <- paste0("\\textbf{", theTable[which.min(theTable[,4]),], "}")
        }
        
        else if(optimize == 1){
          theTable[which.max(theTable[,4]),] <- paste0("\\textbf{", theTable[which.max(theTable[,4]),], "}")
        }
        
        theTable <- cbind(gsub("Exponential Weighted Moving Average", "Exp. Weighted Moving Avg.",
                               gsub("."," ", method_list_names,fixed=TRUE), fixed = TRUE), data.frame(theTable))
        colnames(theTable) = c("method", "min","$Q_{2.5\\%}$","median","$Q_{97.5\\%}$","max", "$Q_{97.5\\%} - Q_{2.5\\%}$")
        
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
          theTable <- matrix(nrow = D, ncol = 6)
          for(d in 1:D){
            theTable[d,] <- format(round(
              cbind(
                min_list[[crit]][[1]][[d]][,fixedIndex][p],
                q2.5_list[[crit]][[1]][[d]][,fixedIndex][p],
                z_list[[crit]][[1]][[d]][,fixedIndex][p],
                q97.5_list[[crit]][[1]][[d]][,fixedIndex][p],
                max_list[[crit]][[1]][[d]][,fixedIndex][p],
                q97.5_list[[crit]][[1]][[d]][,fixedIndex][p] - q2.5_list[[crit]][[1]][[d]][,fixedIndex][p])
              ,2), nsmall = 2)
          }
          
          if(optimize == 0){
            theTable[which.min(theTable[,4]),] <- paste0("\\textbf{", theTable[which.min(theTable[,4]),], "}")
          }
          
          else if(optimize == 1){
            theTable[which.max(theTable[,4]),] <- paste0("\\textbf{", theTable[which.max(theTable[,4]),], "}")
          }
          
          theTable <- cbind(gsub("."," ", data_list_names,fixed=TRUE), data.frame(theTable))
          colnames(theTable) = c("dataset", "min","$Q_{2.5\\%}$","median","$Q_{97.5\\%}$","max", "$Q_{97.5\\%} - Q_{2.5\\%}$")
          
          theTableList[[p]] <- theTable
        }
        
        names(theTableList) <- paste("(",prop_vec_names,",",gap_vec_names[fixedIndex],")", sep = "")
      }
      
      else if(cross_section == "p"){
        
        for(g in 1:G){
          theTable <- matrix(nrow = D, ncol = 6)
          for(d in 1:D){
            theTable[d,] <- format(round(
              cbind(
                min_list[[crit]][[1]][[d]][fixedIndex,][g],
                q2.5_list[[crit]][[1]][[d]][fixedIndex,][g],
                z_list[[crit]][[1]][[d]][fixedIndex,][g],
                q97.5_list[[crit]][[1]][[d]][fixedIndex,][g],
                max_list[[crit]][[1]][[d]][fixedIndex,][g],
                q97.5_list[[crit]][[1]][[d]][fixedIndex,][g] - q2.5_list[[crit]][[1]][[d]][fixedIndex,][g])
              ,2), nsmall = 2)
          }
          
          if(optimize == 0){
            theTable[which.min(theTable[,4]),] <- paste0("\\textbf{", theTable[which.min(theTable[,4]),], "}")
          }
          
          else if(optimize == 1){
            theTable[which.max(theTable[,4]),] <- paste0("\\textbf{", theTable[which.max(theTable[,4]),], "}")
          }
          
          theTable <- cbind(gsub("."," ", data_list_names,fixed=TRUE), data.frame(theTable))
          colnames(theTable) = c("dataset", "min", "$Q_{2.5\\%}$","median","$Q_{97.5\\%}$","max", "$Q_{97.5\\%} - Q_{2.5\\%}$")
          
          theTableList[[g]] <- theTable
        }
        
        names(theTableList) <- paste("(",prop_vec_names[fixedIndex],",",gap_vec_names,")", sep = "")
      }
    }
    
    else if(collapse){ # SAMPLING DISTRIBUTION OF THE SAMPLE MEDIANS
      if(cross_section == "g"){
        
        for(p in 1:P){
          theTable <- matrix(nrow = D, ncol = 6)
          for(d in 1:D){
            theTable[m,] <- format(round(
              cbind(
                apply(z_list[[crit]][[1]][[d]],1,min)[p],
                apply(z_list[[crit]][[1]][[d]],1,function (x) quantile(x, probs = c(0.025,0.975))["2.5%"])[p],
                apply(z_list[[crit]][[1]][[d]],1,median)[p],
                apply(z_list[[crit]][[1]][[d]],1,function (x) quantile(x, probs = c(0.025,0.975))["97.5%"])[p],
                apply(z_list[[crit]][[1]][[d]],1,max)[p],
                apply(z_list[[crit]][[1]][[d]],1,function (x) quantile(x, probs = c(0.025,0.975))["97.5%"])[p] - 
                  apply(z_list[[crit]][[1]][[d]],1,function (x) quantile(x, probs = c(0.025,0.975))["2.5%"])[p])
              ,2), nsmall = 2)
          }
          
          if(optimize == 0){
            theTable[which.min(theTable[,4]),] <- paste0("\\textbf{", theTable[which.min(theTable[,4]),], "}")
          }
          
          else if(optimize == 1){
            theTable[which.max(theTable[,4]),] <- paste0("\\textbf{", theTable[which.max(theTable[,4]),], "}")
          }
          
          theTable <- cbind(gsub("."," ", data_list_names,fixed=TRUE), data.frame(theTable))
          colnames(theTable) = c("dataset", "min","$Q_{2.5\\%}$","median","$Q_{97.5\\%}$","max", "$Q_{97.5\\%} - Q_{2.5\\%}$")
          
          theTableList[[p]] <- theTable
        }
        
        names(theTableList) <- prop_vec_names
      }
      
      else if(cross_section == "p"){
        
        for(g in 1:G){
          theTable <- matrix(nrow = D, ncol = 6)
          for(d in 1:D){
            theTable[d,] <- format(round(
              cbind(
                apply(z_list[[crit]][[1]][[d]],2,min)[g],
                apply(z_list[[crit]][[1]][[d]],2,function (x) quantile(x, probs = c(0.025,0.975))["2.5%"])[g],
                apply(z_list[[crit]][[1]][[d]],2,median)[g],
                apply(z_list[[crit]][[1]][[d]],2,function (x) quantile(x, probs = c(0.025,0.975))["97.5%"])[g],
                apply(z_list[[crit]][[1]][[d]],2,max)[g],
                apply(z_list[[crit]][[1]][[d]],2,function (x) quantile(x, probs = c(0.025,0.975))["97.5%"])[g] - 
                  apply(z_list[[crit]][[1]][[d]],2,function (x) quantile(x, probs = c(0.025,0.975))["2.5%"])[g])
              ,2), nsmall = 2)
          }
          
          if(optimize == 0){
            theTable[which.min(theTable[,4]),] <- paste0("\\textbf{", theTable[which.min(theTable[,4]),], "}")
          }
          
          else if(optimize == 1){
            theTable[which.max(theTable[,4]),] <- paste0("\\textbf{", theTable[which.max(theTable[,4]),], "}")
          }
          
          theTable <- cbind(gsub("."," ", data_list_names,fixed=TRUE), data.frame(theTable))
          colnames(theTable) = c("dataset", "min","$Q_{2.5\\%}$","median","$Q_{97.5\\%}$","max", "$Q_{97.5\\%} - Q_{2.5\\%}$")
          
          theTableList[[g]] <- theTable
        }
        
        names(theTableList) <- gap_vec_names
      }
    } 
  }
  
  return(theTableList)
  
}










