gradEval <- function(agEval, crit, m = names(agEval[[1]][[1]][[1]]), d = 1:length(names(agEval)), cm, cd, f = "median"){
  
  stopifnot(cm %in% m)
  
  gradList <- gradient(agEval = newag, crit = crit, f = f, d = d, m = m)

  om = m[!(m %in% cm)]
  
  cmGrad <- gradList[[crit]][[cm]][[cd]]
  
  omList <- list()
  
  for(vm in 1:length(om)){
    omList[[vm]] <- eval(parse(text = paste0("gradList[[crit]][[om[",vm,"]]][[cd]]"))) 
  }
  names(omList) = om

  #Find maximum slope of chosen gradient
  ## max or min
  #gradIndex <- which(abs(grad1$slope) == max(abs(grad1$slope)))
  ## just max
  gradIndex <- which.max(cmGrad[,"slope"])

  #Compare to other methods at that point
  string <- paste0(om[1:(length(om)-1)]," = omList[[",1:(length(om)-1),"]][gradIndex,],",  collapse = "")
  string <- c("list(",paste0(cm," = cmGrad[gradIndex,],"),string, paste0(om[length(om)]," = omList[[",length(om),"]][gradIndex,]"),")")

  comList <- eval(parse(text = string))
  
  string <- paste0(om[1:(length(om)-1)]," = comList[[",2:(length(om)),"]]['slope']/comList[[1]]['slope'],")
  string <- c("list(",string,paste0(om[length(om)]," = comList[[",length(om)+1,"]]['slope']/comList[[1]]['slope']"),")")
  
  divs<- eval(parse(text = string))  
  divs <- sort(unlist(divs), decreasing = FALSE)
  
  om <- gsub(".slope","",names(divs))
  
  #Statements
  
  string1 <- paste0("Region of max instability (",cm,", dataset ", cd,", ", crit,") : (p1 = ",cmGrad[gradIndex,'p1'],", g1 = ", cmGrad[gradIndex,'g1'],") to (p2 = ", cmGrad[gradIndex,'p2'],", g2 = ", cmGrad[gradIndex,'g2'],")")
  
  reportList <- list(string1)
  
  for(vm in 1:length(om)){
    reportList[[vm+1]] <- paste0("The ", om[vm], " is ", round(divs[[vm]],2)," times less stable.")
  }
  
  return(reportList)
}





