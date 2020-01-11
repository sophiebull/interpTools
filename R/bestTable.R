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
                      m=names(agEval[[1]][[1]][[1]]), 
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
  
  stopifnot((layer_type == "method" | layer_type == "dataset"), class(agEval) == "agEvaluate",
            crit %in% rownames(agEval[[1]][[1]][[1]][[1]]),
            length(d) <= length(agEval), length(m) <= length(agEval[[1]][[1]][[1]]),
            f %in% names(agEval[[1]][[1]][[1]][[1]])[1:11],
            (cross_section =="p" | cross_section == "g")
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
  min_list <- mat[["q0"]]
  max_list <- mat[["q100"]]
  
  data_list_names <- names(z_list[[1]][[1]])[d]
  method_list_names <- m
  
  
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
    if(cross_section == "p"){
      
      for(p in 1:P){
        theTable <- matrix(nrow = M, ncol = 4) # Change to 6 if including the min and max
        for(vm in 1:M){
          theTable[vm,] <- format(round(
            cbind(
              #min_list[[crit]][[m[vm]]][[d]][,fixedIndex][p],
              q2.5_list[[crit]][[m[vm]]][[d]][,fixedIndex][p],
              z_list[[crit]][[m[vm]]][[d]][,fixedIndex][p],
              q97.5_list[[crit]][[m[vm]]][[d]][,fixedIndex][p],
              #max_list[[crit]][[m[vm]]][[d]][,fixedIndex][p], 
              q97.5_list[[crit]][[m[vm]]][[d]][,fixedIndex][p] - q2.5_list[[crit]][[m[vm]]][[d]][,fixedIndex][p]) # IQR
            ,3), nsmall = 3)
        }
        
        if(optimize == 0){
          theTable[which.min(theTable[,3]),] <- paste0("\\textbf{", theTable[which.min(theTable[,3]),], "}") #change to 4 if including min
        }
        
        else if(optimize == 1){
          theTable[which.max(theTable[,3]),] <- paste0("\\textbf{", theTable[which.max(theTable[,3]),], "}") #change to 4 if including min
        }
        
        theTable <- cbind(method_list_names,data.frame(theTable))
        
        colnames(theTable) = c("method", 
                               #"min",
                               "$Q_{2.5\\%}$",
                               "median",
                               "$Q_{97.5\\%}$",
                               #"max", 
                               "$Q_{97.5\\%} - Q_{2.5\\%}$")
        
        theTableList[[p]] <- theTable
      }
      
      names(theTableList) <- paste("(",prop_vec_names,",",gap_vec_names[fixedIndex],")", sep = "")
    }
    
    else if(cross_section == "g"){
      
      for(g in 1:G){
        theTable <- matrix(nrow = M, ncol = 4) # change to 6 if including min and max
        for(vm in 1:M){
          theTable[vm,] <- format(round(
            cbind(
              #min_list[[crit]][[m[vm]]][[d]][fixedIndex,][g],
              q2.5_list[[crit]][[m[vm]]][[d]][fixedIndex,][g],
              z_list[[crit]][[m[vm]]][[d]][fixedIndex,][g],
              q97.5_list[[crit]][[m[vm]]][[d]][fixedIndex,][g],
              #max_list[[crit]][[m[vm]]][[d]][fixedIndex,][g],
              q97.5_list[[crit]][[m[vm]]][[d]][fixedIndex,][g] - q2.5_list[[crit]][[m[vm]]][[d]][fixedIndex,][g])
            ,3), nsmall = 3)
        }
        
        if(optimize == 0){
          theTable[which.min(theTable[,3]),] <- paste0("\\textbf{", theTable[which.min(theTable[,3]),], "}") #change to 4 if including min
        }
        
        else if(optimize == 1){
          theTable[which.max(theTable[,3]),] <- paste0("\\textbf{", theTable[which.max(theTable[,3]),], "}") #change to 4 if including min
        }
        
        theTable <- cbind(method_list_names,data.frame(theTable))
        
        colnames(theTable) = c("method", 
                               #"min",
                               "$Q_{2.5\\%}$",
                               "median",
                               "$Q_{97.5\\%}$",
                               #"max",
                               "$Q_{97.5\\%} - Q_{2.5\\%}$")
        
        theTableList[[g]] <- theTable
      }
      
      names(theTableList) <- paste("(",prop_vec_names[fixedIndex],",",gap_vec_names,")", sep = "")
    }
  }
  
  else if(collapse){ # SAMPLING DISTRIBUTION OF THE SAMPLE MEDIANS
    if(cross_section == "p"){
      
      for(p in 1:P){
        theTable <- matrix(nrow = M, ncol = 4) #change to 6 if including min and max
        for(vm in 1:M){
          theTable[vm,] <- format(round(
            cbind(
              #apply(z_list[[crit]][[m[vm]]][[d]],1,min)[p],
              apply(z_list[[crit]][[m[vm]]][[d]],1,function (x) quantile(x, probs = c(0.025,0.975))["2.5%"])[p],
              apply(z_list[[crit]][[m[vm]]][[d]],1,median)[p],
              apply(z_list[[crit]][[m[vm]]][[d]],1,function (x) quantile(x, probs = c(0.025,0.975))["97.5%"])[p],
              #apply(z_list[[crit]][[m[vm]]][[d]],1,max)[p],
              apply(z_list[[crit]][[m[vm]]][[d]],1,function (x) quantile(x, probs = c(0.025,0.975))["97.5%"])[p] - 
              apply(z_list[[crit]][[m[vm]]][[d]],1,function (x) quantile(x, probs = c(0.025,0.975))["2.5%"])[p])
            ,3), nsmall = 3)
        }
        
        if(optimize == 0){
          theTable[which.min(theTable[,3]),] <- paste0("\\textbf{", theTable[which.min(theTable[,3]),], "}") #change to 4 if including min
        }
        
        else if(optimize == 1){
          theTable[which.max(theTable[,3]),] <- paste0("\\textbf{", theTable[which.max(theTable[,3]),], "}") #change to 4 if including min
        }
        
        theTable <- cbind(method_list_names,data.frame(theTable))
        
        colnames(theTable) = c("method",
                               #"min",
                               "$Q_{2.5\\%}$",
                               "median",
                               "$Q_{97.5\\%}$",
                               #"max",
                               "$Q_{97.5\\%} - Q_{2.5\\%}$")
        
        theTableList[[p]] <- theTable
      }
      
      names(theTableList) <- prop_vec_names
    }
    
    else if(cross_section == "g"){
      
      for(g in 1:G){
        theTable <- matrix(nrow = M, ncol = 4) #change to 6 if including min and max
        for(vm in 1:M){
          theTable[vm,] <- format(round(
            cbind(
              #apply(z_list[[crit]][[m[vm]]][[d]],2,min)[g],
              apply(z_list[[crit]][[m[vm]]][[d]],2,function (x) quantile(x, probs = c(0.025,0.975))["2.5%"])[g],
              apply(z_list[[crit]][[m[vm]]][[d]],2,median)[g],
              apply(z_list[[crit]][[m[vm]]][[d]],2,function (x) quantile(x, probs = c(0.025,0.975))["97.5%"])[g],
              #apply(z_list[[crit]][[m[vm]]][[d]],2,max)[g],
              apply(z_list[[crit]][[m[vm]]][[d]],2,function (x) quantile(x, probs = c(0.025,0.975))["97.5%"])[g] - 
              apply(z_list[[crit]][[m[vm]]][[d]],2,function (x) quantile(x, probs = c(0.025,0.975))["2.5%"])[g])
            ,3), nsmall = 3)
        }
        
        if(optimize == 0){
          theTable[which.min(theTable[,3]),] <- paste0("\\textbf{", theTable[which.min(theTable[,3]),], "}") #change to 4 if including min
        }
        
        else if(optimize == 1){
          theTable[which.max(theTable[,3]),] <- paste0("\\textbf{", theTable[which.max(theTable[,3]),], "}") #change to 4 if including min
        }
        
        theTable <- cbind(method_list_names,data.frame(theTable))
        
        colnames(theTable) = c("method", 
                               #"min",
                               "$Q_{2.5\\%}$",
                               "median",
                               "$Q_{97.5\\%}$",
                               #"max",
                               "$Q_{97.5\\%} - Q_{2.5\\%}$")
        
        theTableList[[g]] <- theTable
      }
      
      names(theTableList) <- gap_vec_names
    }
  }
  }
  
  
  else if(layer_type == "dataset"){
    if(!collapse){
      if(cross_section == "p"){
        
        for(p in 1:P){
          theTable <- matrix(nrow = D, ncol = 4) #change to 6 if including min and max
          for(vd in 1:D){
            theTable[vd,] <- format(round(
              cbind(
                #min_list[[crit]][[m]][[d[vd]]][,fixedIndex][p],
                q2.5_list[[crit]][[m]][[d[vd]]][,fixedIndex][p],
                z_list[[crit]][[m]][[d[vd]]][,fixedIndex][p],
                q97.5_list[[crit]][[m]][[d[vd]]][,fixedIndex][p],
                #max_list[[crit]][[m]][[d[vd]]][,fixedIndex][p],
                q97.5_list[[crit]][[m]][[d[vd]]][,fixedIndex][p] - q2.5_list[[crit]][[m]][[d[vd]]][,fixedIndex][p])
              ,3), nsmall = 3)
          }
          
          if(optimize == 0){
            theTable[which.min(theTable[,3]),] <- paste0("\\textbf{", theTable[which.min(theTable[,3]),], "}") #change to 4 if including min
          }
          
          else if(optimize == 1){
            theTable[which.max(theTable[,3]),] <- paste0("\\textbf{", theTable[which.max(theTable[,3]),], "}") #change to 4 if including min
          }
          
          theTable <- cbind(gsub("."," ", data_list_names,fixed=TRUE), data.frame(theTable))
          colnames(theTable) = c("dataset", 
                                 #"min",
                                 "$Q_{2.5\\%}$",
                                 "median",
                                 "$Q_{97.5\\%}$",
                                 #"max",
                                 "$Q_{97.5\\%} - Q_{2.5\\%}$")
          
          theTableList[[p]] <- theTable
        }
        
        names(theTableList) <- paste("(",prop_vec_names,",",gap_vec_names[fixedIndex],")", sep = "")
      }
      
      else if(cross_section == "g"){
        
        for(g in 1:G){
          theTable <- matrix(nrow = D, ncol = 4) #change to 6 if including min and max
          for(vd in 1:D){
            theTable[vd,] <- format(round(
              cbind(
                #min_list[[crit]][[m]][[d[vd]]][fixedIndex,][g],
                q2.5_list[[crit]][[m]][[d[vd]]][fixedIndex,][g],
                z_list[[crit]][[m]][[d[vd]]][fixedIndex,][g],
                q97.5_list[[crit]][[m]][[d[vd]]][fixedIndex,][g],
                #max_list[[crit]][[m]][[d[vd]]][fixedIndex,][g],
                q97.5_list[[crit]][[m]][[d[vd]]][fixedIndex,][g] - q2.5_list[[crit]][[m]][[d[vd]]][fixedIndex,][g])
              ,3), nsmall = 3)
          }
          
          if(optimize == 0){
            theTable[which.min(theTable[,3]),] <- paste0("\\textbf{", theTable[which.min(theTable[,3]),], "}") #change to 4 if including min
          }
          
          else if(optimize == 1){
            theTable[which.max(theTable[,3]),] <- paste0("\\textbf{", theTable[which.max(theTable[,3]),], "}") #change to 4 if including min
          }
          
          theTable <- cbind(gsub("."," ", data_list_names,fixed=TRUE), data.frame(theTable))
          colnames(theTable) = c("dataset", 
                                 #"min", 
                                 "$Q_{2.5\\%}$",
                                 "median",
                                 "$Q_{97.5\\%}$",
                                 #"max",
                                 "$Q_{97.5\\%} - Q_{2.5\\%}$")
          
          theTableList[[g]] <- theTable
        }
        
        names(theTableList) <- paste("(",prop_vec_names[fixedIndex],",",gap_vec_names,")", sep = "")
      }
    }
    
    else if(collapse){ # SAMPLING DISTRIBUTION OF THE SAMPLE MEDIANS
      if(cross_section == "p"){
        
        for(p in 1:P){
          theTable <- matrix(nrow = D, ncol = 4) #change to 6 if including min and max
          for(vd in 1:D){
            theTable[vd,] <- format(round(
              cbind(
                #apply(z_list[[crit]][[m]][[d[vd]]],1,min)[p],
                apply(z_list[[crit]][[m]][[d[vd]]],1,function (x) quantile(x, probs = c(0.025,0.975))["2.5%"])[p],
                apply(z_list[[crit]][[m]][[d[vd]]],1,median)[p],
                apply(z_list[[crit]][[m]][[d[vd]]],1,function (x) quantile(x, probs = c(0.025,0.975))["97.5%"])[p],
                #apply(z_list[[crit]][[m]][[d[vd]]],1,max)[p],
                apply(z_list[[crit]][[m]][[d[vd]]],1,function (x) quantile(x, probs = c(0.025,0.975))["97.5%"])[p] - 
                apply(z_list[[crit]][[m]][[d[vd]]],1,function (x) quantile(x, probs = c(0.025,0.975))["2.5%"])[p])
              ,3), nsmall = 3)
          }
          
          if(optimize == 0){
            theTable[which.min(theTable[,3]),] <- paste0("\\textbf{", theTable[which.min(theTable[,3]),], "}") #change to 4 if including min
          }
          
          else if(optimize == 1){
            theTable[which.max(theTable[,3]),] <- paste0("\\textbf{", theTable[which.max(theTable[,3]),], "}") #change to 4 if including min
          }
          
          theTable <- cbind(gsub("."," ", data_list_names,fixed=TRUE), data.frame(theTable))
          colnames(theTable) = c("dataset", 
                                 #"min",
                                 "$Q_{2.5\\%}$",
                                 "median",
                                 "$Q_{97.5\\%}$",
                                 #"max",
                                 "$Q_{97.5\\%} - Q_{2.5\\%}$")
          
          theTableList[[p]] <- theTable
        }
        
        names(theTableList) <- prop_vec_names
      }
      
      else if(cross_section == "g"){
        
        for(g in 1:G){
          theTable <- matrix(nrow = D, ncol = 4) #change to 6 if including min and max
          for(vd in 1:D){
            theTable[vd,] <- format(round(
              cbind(
                #apply(z_list[[crit]][[m]][[d[vd]]],2,min)[g],
                apply(z_list[[crit]][[m]][[d[vd]]],2,function (x) quantile(x, probs = c(0.025,0.975))["2.5%"])[g],
                apply(z_list[[crit]][[m]][[d[vd]]],2,median)[g],
                apply(z_list[[crit]][[m]][[d[vd]]],2,function (x) quantile(x, probs = c(0.025,0.975))["97.5%"])[g],
                #apply(z_list[[crit]][[m]][[d[vd]]],2,max)[g],
                apply(z_list[[crit]][[m]][[d[vd]]],2,function (x) quantile(x, probs = c(0.025,0.975))["97.5%"])[g] - 
                apply(z_list[[crit]][[m]][[d[vd]]],2,function (x) quantile(x, probs = c(0.025,0.975))["2.5%"])[g])
              ,3), nsmall = 3)
          }
          
          if(optimize == 0){
            theTable[which.min(theTable[,3]),] <- paste0("\\textbf{", theTable[which.min(theTable[,3]),], "}") #change to 4 if including min
          }
          
          else if(optimize == 1){
            theTable[which.max(theTable[,3]),] <- paste0("\\textbf{", theTable[which.max(theTable[,3]),], "}") #change to 4 if including min
          }
          
          theTable <- cbind(gsub("."," ", data_list_names,fixed=TRUE), data.frame(theTable))
          colnames(theTable) = c("dataset", 
                                 #"min",
                                 "$Q_{2.5\\%}$",
                                 "median",
                                 "$Q_{97.5\\%}$",
                                 #"max",
                                 "$Q_{97.5\\%} - Q_{2.5\\%}$")
          
          theTableList[[g]] <- theTable
        }
        
        names(theTableList) <- gap_vec_names
      }
    } 
  }
  
  return(theTableList)
  
}










