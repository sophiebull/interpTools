#' Split Data
#' 
#' Function to K-split the Gappy Data
#' 
#' @param split The number of groups to divide K into (ie. K/split). Value must be an integer and evenly divide K.
#' @param GappyData A list object of dimension DxPxGxK 

splitData <- function(GappyData, split = split){
  
  D <- length(GappyData)
  P <- length(GappyData[[1]])
  G <- length(GappyData[[1]][[1]])
  K <- length(GappyData[[1]][[1]][[1]])
  
  if(split %% 1 != 0) stop("split must be an integer.")
  if((K/split) %%1 != 0) stop(paste0("The split parameter must be an integer and evenly divide K, where K = ",K,"."))
  
  Nind <- 1:split
  
  splitList <- lapply(splitList <- vector(mode = 'list',D),function(x)
    lapply(splitList <- vector(mode = 'list', split),function(x) 
      lapply(splitList <- vector(mode = 'list',P),function(x) 
        lapply(splitList <- vector(mode = 'list', G), function(x)
          x<-vector(mode='list',K)))))
  
  splitData = GappyData 
  
  Ksplit <- length(GappyData[[1]][[1]][[1]])/split
  vecSplit <- list()
  splitK <- list()
  
  for(d in 1:length(GappyData)){ 
    for(n in 1:split){
      vecSplit[[n]] <- ((n-1)*Ksplit + 1):(n*Ksplit) 
      
      for(p in 1:length(GappyData[[1]])){
        for(g in 1:length(GappyData[[1]][[1]])){
          splitK <- GappyData[[d]][[p]][[g]][vecSplit[[n]]]
          splitData[[d]][[p]][[g]] <- splitK
          splitList[[d]][[n]][[p]][[g]] <- splitData[[d]][[p]][[g]]
        }
        names(splitList[[d]][[n]][[p]]) <- names(GappyData[[1]][[1]])
      }
      names(splitList[[d]][[n]]) <- names(GappyData[[1]])
    }
    names(splitList[[d]])<- paste0((Nind-1)*Ksplit+1,":",Nind*Ksplit)
  }
  names(splitList) <- names(GappyData)
  
  return(splitList)
}
