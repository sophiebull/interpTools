#'  Compile agEvaluate Objects into Matrices
#' 
#'  A function to compile objects of class "agEvaluate" (from agEvaluate()) into matrices of dimension PxG.\cr
#'  Elements of agEvaluate are arranged into this matrix corresponding to (p,g).
#'  
#' @param agEval A list object (result of agEvaluate.R) of aggregated performance metrics
#' 

compileMatrix <-  function(agEval){ 
  
D <- length(agEval)  
P <- length(agEval[[1]])
G <- length(agEval[[1]][[1]])
M <- length(agEval[[1]][[1]][[1]])
C <- nrow(agEval[[1]][[1]][[1]][[1]])
stat <- names(agEval[[1]][[1]][[1]][[1]])[1:11]

prop_vec_names <- names(agEval[[1]])
gap_vec_names <- names(agEval[[1]][[1]])

theVals <- lapply(theVals <- vector(mode = 'list', length(stat)),function(x)
  lapply(theVals <- vector(mode = 'list', C),function(x) 
    lapply(theVals <- vector(mode = 'list', M),function(x) 
      x <- vector(mode = 'list', D))))

critMat <- matrix(nrow=length(agEval[[1]]),ncol=length(agEval[[1]][[1]]))
rownames(critMat) <- prop_vec_names
colnames(critMat) <- gap_vec_names

data_list_names <- numeric(D)
method_list_names <- numeric(M)

  for(st in 1:length(stat)){  
    for(s in 1:C){
      for(m in 1:M){
        for(d in 1:D){
          for(p in 1:P){
            for(g in 1:G){
              critMat[p,g] <- agEval[[d]][[p]][[g]][[m]][s,stat[st]]
              method_list_names[m] <- as.character(agEval[[d]][[p]][[g]][[m]][s, "method"]) 
            }
          }
          theVals[[st]][[s]][[m]][[d]] <- critMat
          data_list_names[d] <- paste("D",d,sep="") 
        }
        names(theVals[[st]][[s]][[m]]) <- data_list_names
      }
      names(theVals[[st]][[s]]) <- method_list_names
    }
    names(theVals[[st]]) <- rownames(agEval[[d]][[p]][[g]][[m]])
  }

  names(theVals) <- stat
  return(theVals)
}
