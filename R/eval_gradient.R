#' Report on the Stability of Interpolation Algorithms
#' 
#' A function that will produce a report on the stability of the input interpolation algorithms according to the gradient.
#' 
#' @param agObject \code{aggregate_pf}; A list object (result of \code{aggregate_pf()}) of aggregated performance metrics
#' @param d \code{character}; A vector of the datasets of interest
#' @param m \code{character}; A vector of the interpolation methods of interest 
#' @param cm \code{character}; The chosen method(s) of interest 
#' @param cd \code{character}; The chosen dataset(s) of interest
#' @param metric \code{character}; A character vector describing the performance metrics of interest
#' @param f \code{character}; "median" (default): which statistic will be represented by the gradient

eval_gradient <- function(agObject, metric, m = names(agObject[[1]][[1]][[1]]), d = names(agObject), cm, cd, f = "median"){
  
  #####################
  # LOGICAL CHECKS
  #####################
  
  if(length(cm) != 1) stop("Chosen method of interest must be a single character element.")
  if(length(cd) != 1) stop("Chosen dataset of interest must be a single character element.")
  
  
  if(!all(m %in%  names(agObject[[1]][[1]][[1]]))) stop("Method(s) '", paste0(m[!m %in% names(agObject[[1]][[1]][[1]])], collapse = ", ' "),"' not found. Possible choices are: '", paste0(names(agObject[[1]][[1]][[1]]), collapse = "', '"),"'.")
  if(!all(d %in% names(agObject))) stop("Dataset(s) ", paste0(d[!d %in% names(agObject)], collapse = ", ")," not found. Possible choices are: ", paste(names(agObject), collapse = ','))
  if(!f %in% names(agObject[[1]][[1]][[1]][[1]])) stop(paste0(c("f must be one of: '",paste0(names(agObject[[1]][[1]][[1]][[1]]), collapse = "', '"),"'."), collapse = ""))
  if(!all(metric %in% rownames(agObject[[1]][[1]][[1]][[1]]))) stop(paste0("Metric(s) '",paste(metric[which(!metric %in% rownames(agObject[[1]][[1]][[1]][[1]]))], collapse = ","),"' must be one of ", paste(rownames(agObject[[1]][[1]][[1]][[1]]),collapse = ", "),"."))
  
  
  if(!(cm %in% m)) stop(paste0("Chosen method '", cm, "' must be one of ", paste0(m, collapse = ", ") ,"." ))
  if(!(cd %in% d)) stop(paste0("Chosen method '", cd, "' must be one of ", paste0(d, collapse = ", ") ,"." ))
  if(class(agObject) != "aggregate_pf") stop("'agObject' must be of class 'aggregate_pf'")
  
  if(length(f) != 1) stop("'f' must contain only a single character element.")

  
  if(class(agObject) != "aggregate_pf") stop("'agObject' object must be of class 'aggregate_pf'. Please use aggregate_pf().")
  
  
  
  
  
  
  #####################
  # Defining 'optimal'
  #####################
  
  metrics <- rownames(agObject[[1]][[1]][[1]][[1]])
  maximize <- c(1,1,rep(0,11),1,rep(0,4)) # 1 = yes, 0 = no
  optimal <- maximize
  optimal[which(optimal == "1")] <- "max"
  optimal[which(optimal == "0")] <- "min"
  
  if(length(metrics) != length(optimal)){
    
    need_define <- metrics[!metrics %in% metrics[1:length(optimal)]] 
    
    # ask for user input
    prompt = paste0("No definition of 'optimal' for metric(s): ", paste(need_define, collapse = ","),". Please enter vector of character elements 'max' and 'min' corresponding to the definition(s) of 'optimal':\n")
    message(prompt)
    user_define <- scan(what = "character", nmax = length(need_define)) 
    
    if(!all(user_define %in% c("max","min"))){
      invalid <- user_define[which(!user_define %in% c("max","min"))]
      message(paste0("Element: ", paste(invalid, collapse = ",")," not valid input(s). Please try again: \n"))
      token = TRUE
    } else{
      token = FALSE
    }
    
    
    if(token){
      message(prompt)
      user_define <- scan(what = "character", nmax = length(need_define)) 
    }
    
    optimal <- c(optimal, user_define)
    
  }
  
  best <- data.frame(metrics = metrics, 
                     maximize = maximize,
                     optimal = optimal)
  
  
  #####################
  # Computing gradient
  #####################
  
  gradList <- gradient(agObject = agObject, metric = metric, f = f, d = d, m = m)

  om = m[!(m %in% cm)] # other method
  
  cmGrad <- gradList[[metric]][[cm]][[cd]] # chosen gradient
  
  omList <- list() # other gradients
  
  for(vm in 1:length(om)){
    omList[[vm]] <- eval(parse(text = paste0("gradList[[metric]][[om[",vm,"]]][[cd]]"))) 
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
  
  ###########################
  # Building report
  ###########################
  

  cat(paste0("Method under inspection: '", cm, "'"),
      paste0("Dataset under inspection: '", cd, "'"),
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





