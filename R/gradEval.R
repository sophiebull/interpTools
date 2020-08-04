gradEval <- function(agEval, crit, m = names(agEval[[1]][[1]][[1]]), d = 1:length(agEval), cm, cd = 1, f = "median"){
  
  if(!(cm %in% m)) stop(paste0("Chosen method '", cm, "' must be one of ", paste0(m, collapse = ", ") ,"." ))

  criteria <- rownames(agEval[[1]][[1]][[1]][[1]])
  maximize <- c(1,1,rep(0,11),1,rep(0,4)) # 1 = yes, 0 = no
  optimal <- maximize
  optimal[which(optimal == "1")] <- "max"
  optimal[which(optimal == "0")] <- "min"
  
  best <- data.frame(criteria = criteria, 
                     maximize = maximize,
                     optimal = optimal)
  
  
  gradList <- gradient(agEval = agEval, crit = crit, f = f, d = d, m = m)

  om = m[!(m %in% cm)] # other method
  
  cmGrad <- gradList[[crit]][[cm]][[cd]] # chosen gradient
  
  omList <- list() # other gradients
  
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
  
  string <- paste0(cm, "= cmGrad[gradIndex,]")
  string <- c("list(", paste0(om," = omList[['",names(omList),"']][gradIndex,]", sep = ","), string,")")
  
  comList <- eval(parse(text = string)) # combined gradient list (chosen and other)
  
  string <- c("list(", paste0(om, " = comList[['",om,"']]['slope']/comList[['", cm,"']]['slope']", sep = ","))
  string <- paste(string, collapse = "")
  string <- c(substr(string, 1, nchar(string)-1),")")
  
  divs<- eval(parse(text = string))  
  divs <- lapply(divs, FUN = abs)
  
  #om <- gsub(".slope","",names(divs))
  
  #Statements
  

  cat(paste0("Method under inspection: '", cm, "'"),
      paste0("Region of max instability:(p1 = ",cmGrad[gradIndex,'p1'],", g1 = ", cmGrad[gradIndex,'g1'],") to (p2 = ", cmGrad[gradIndex,'p2'],", g2 = ", cmGrad[gradIndex,'g2'],")"),
      paste0("Corresponding slope: ", round(comList[[cm]]['slope'], 2)),
      paste0(" "),
      paste0("REPORT:"),sep = "\n")
  
  for(vm in 1:length(om)){
    
    if(abs(divs[[vm]]) < 1){
      cat(paste0("slope of ", om[vm], ": ", comList[[om[vm]]]['slope']),
          paste0("The '", om[vm], "' algorithm has ", round(divs[[vm]],2)," times the slope of '",cm,"': ", cm, " is ", 100 - round(divs[[vm]],2)*100,"% more stable"),
          paste0(" "),
          sep = '\n')
    }
    
    else if(abs(divs[[vm]]) > 1 ){
      cat(paste0("slope of ", om[vm], ": ", comList[[om[vm]]]['slope']),
          paste0("The '", om[vm], "' algorithm has ", round(divs[[vm]],2)," times the slope of '",cm,"': ", om[vm], " is ", abs(100 - round(divs[[vm]],2)*100),"% more unstable"),
          paste0(" "),
          sep = '\n')
    }
  }
  
}





