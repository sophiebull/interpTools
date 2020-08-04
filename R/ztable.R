#'  Create a Table of Values (LaTeX) 
#' 
#'  A function to produce tables of data compatible with LaTeX. \cr
#'  
#' @param agEval \code{agEvaluate}; An object containing the aggregated performance metrics (result of \code{agEvaluate()})
#' @param d \code{numeric}; A single value to indicate the dataset of interest
#' @param crit \code{character}; A single element describing the performance metric of interest
#' @param m \code{character}; A single element describing the interpolation method of interest
#' @param sdist \code{logical}; \code{TRUE} returns a table of the sampling distribution \code{(Q2.5, median, Q97.5)} of the chosen metric at every combination of \code{(p,g)}. \code{FALSE} returns a table of values corresponding to \code{f(p,g)}, where each data point is the chosen sample statistic at each \code{(p,g)}.
#' @param f \code{character}; If \code{sdist = F}, the sample statistic of interest defining \code{f(p,g)}. Possible choices are listed in \code{?agEvaluate}.

ztable <- function(agEval, d = 1, crit, m, sdist = F, f = NULL){
  
  if(!is.logical(sdist)) stop("Object 'sdist' must either be TRUE or FALSE. TRUE returns the sampling distribution, FALSE returns the raw data according to 'crit' and 'f'.")
  
  
  if(!is.null(f) && sdist) warning(paste0("If 'sdist = T', the returned object is a table of the (Q2.7, median, Q97.5) sampling distribution. Ignoring user input of '", f,"' for 'f'."))
  if(!is.null(f) && sdist) { f <- NULL}
  if(is.null(f) && !sdist) stop("If 'sdist = F' then the user must specify the sample statistic of interest.")
  
  
  if(class(agEval) != "agEvaluate") stop("'agEval' object must be of class 'agEvaluate'. Please use agEvaluate().")
  if(length(d) != 1 | class(d) != "numeric") stop("Object 'd' must be of class 'numeric' and of length one.")
  if(length(m) != 1 | class(m) != "character") stop("Object 'm' must be of class 'character' and of length one.")
  if(!sdist && (length(f) != 1 | class(f) != "character")) stop("Object 'f' must be of class 'character' and of length one.")
  if(length(crit) != 1 | class(crit) != "character") stop("Object 'crit' must be of class 'character' and of length one.")
  
  if(!all(paste0("D",d) %in% names(agEval))) stop("Dataset(s) ", paste0(d[!paste0("D",d) %in% names(agEval)], collapse = ", ")," not found. Possible choices are: ", paste0(gsub("D", "",names(agEval)), collapse = ", "))
  if(!all(f %in% names(agEval[[1]][[1]][[1]][[1]]))) stop(paste0(c("f must be one of: '",paste0(names(agEval[[1]][[1]][[1]][[1]]), collapse = "', '"),"'."), collapse = ""))
  if(!crit %in% rownames(agEval[[1]][[1]][[1]][[1]])) stop(paste0("Criterion '",crit,"' must be one of ", paste(rownames(agEval[[1]][[1]][[1]][[1]]),collapse = ", "),"."))
  if(!all(m %in%  names(agEval[[1]][[1]][[1]]))) stop("Method(s) '", paste0(m[!m %in% names(agEval[[1]][[1]][[1]])], collapse = ", ' "),"' not found. Possible choices are: '", paste0(names(agEval[[1]][[1]][[1]]), collapse = "', '"),"'.")
  
  zlist <- compileMatrix(agEval)
  
  zlist_q2.5 <- round(zlist[["q2.5"]][[crit]][[m]][[d]],digits = 2)
  zlist_median <- round(zlist[["median"]][[crit]][[m]][[d]], digits = 2)
  zlist_q97.5 <- round(zlist[["q97.5"]][[crit]][[m]][[d]], digits = 2)
  
  # building multicol footer
  footer <- rep(colnames(zlist[[1]][[1]][[1]][[1]]), each = 3)
  n = 1:(length(colnames(zlist[[1]][[1]][[1]][[1]]))-1)
  index <- c(2, 2 + 3*n)
  
  index_str <- 1:length(footer)
  index_str[index] <- NA
  
  footer[index_str] <- " "
  
  if(sdist){
    G <- length(agEval[[1]][[1]])
    
    string <- paste0("zlist_q2.5[,",1:(G-1),"], zlist_median[,",1:(G-1),"], zlist_q97.5[,",1:(G-1),"], ")
    
    string <-  c("cbind(",string, paste0("zlist_q2.5[,",G,"], zlist_median[,",G,"], zlist_q97.5[,",G,"]"),")")
    
    tableMat <- eval(parse(text = string))
    
    bold_row <- c(2,5,8)
  
    for(i in 1:length(bold_row)){
      tableMat[,bold_row[i]] <- paste0("\\textbf{",tableMat[,bold_row[i]],"}")
    }
    
    tableMat <- rbind(tableMat, footer)
    rownames(tableMat)[nrow(tableMat)] <- ""
    
    colnames(tableMat) <- rep(c("$Q_{2.5\\%}$", "median", "$Q_{97.5\\%}$"),G)
    
    call <- "print(xtable(tableMat), include.rownames = T,  sanitize.text.function = function(x) x)"
  }
  
  else if(!sdist){
    
    zlist_f <-  round(zlist[[f]][[crit]][[m]][[d]],digits = 2)
    
    call <- "print(xtable(zlist_f), include.rownames = T,  sanitize.text.function = function(x) x)"

  }
  
  return(eval(parse(text = call)))
}
