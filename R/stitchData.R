#' Stitch Data 
#' 
#' Function to stitch together data
#' 
#' @param directory The directory location of the .rda files to stitch. Must be devoid of other files, and each .rda must be identical in structure. 


stitchData <- function(directory){
  
  if(!all(grepl(".rda", list.files(directory)))) stop(c("Directory must only contain .rda files that are identical in structure. Please relocate or delete the following files: ",
                                                             
                                                             paste(list.files(directory)[which(!grepl(".rda",list.files(directory)))], 
                                                                   collapse = ", ")))
  
  files <- list.files(directory, pattern=".rda", full.names = TRUE, recursive = TRUE)
  split <- length(files)
  
  fileList <- list()
  for(i in 1:split){
  fileList[[i]] <- load_obj(files[i]) 
  }
  
  names(fileList) <- letters[1:split]
  
  M <- length(fileList[[1]])
  P <- length(fileList[[1]][[1]])
  G <- length(fileList[[1]][[1]][[1]])
  K <- length(fileList[[1]][[1]][[1]][[1]])*split
    
  stitchList <-  lapply(stitchList <- vector(mode = 'list',M), function(x)
    lapply(stitchList <- vector(mode = 'list', P), function(x) 
        x<-vector(mode='list',G)))
  
  names(stitchList) <- names(fileList[[1]])
  names(stitchList[[1]]) <- names(fileList[[1]][[1]])
  names(stitchList[[1]][[1]]) <- names(fileList[[1]][[1]][[1]])

    for(m in 1:M){
      for(p in 1:P){
        for(g in 1:G){
          for(i in 1:split){
          stitchList[[m]][[p]][[g]] <- append(stitchList[[m]][[p]][[g]],fileList[[i]][[m]][[p]][[g]])
          }
          names(stitchList[[m]][[p]]) <- names(fileList[[1]][[1]][[1]])
      }
    }
  }

  return(stitchList)
  }


