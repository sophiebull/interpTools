ztable <- function(agEval, d, crit, m, combine = T, output = NULL){

  zlist <- compileMatrix(agEval)
  
  zlist_q2.5 <- round(zlist[["q2.5"]][[crit]][[m]][[d]],digits = 2)
  zlist_median <- round(zlist[["median"]][[crit]][[m]][[d]], digits = 2)
  zlist_q97.5 <- round(zlist[["q97.5"]][[crit]][[m]][[d]], digits = 2)
  
  
  if(combine){
    G <- length(agEval[[1]][[1]])
    
    string <- paste0("zlist_q2.5[,",1:(G-1),"], zlist_median[,",1:(G-1),"], zlist_q97.5[,",1:(G-1),"], ")
    
    string <-  c("cbind(",string, paste0("zlist_q2.5[,",G,"], zlist_median[,",G,"], zlist_q97.5[,",G,"]"),")")
    
    tableMat <- eval(parse(text = string))
    
    bold_row <- c(2,5,8)
  
    for(i in 1:length(bold_row)){
      tableMat[,bold_row[i]] <- paste0("\\textbf{",tableMat[,bold_row[i]],"}")
    }
    
    colnames(tableMat) <- rep(c("$Q_{2.5\\%}$", "median", "$Q_{97.5\\%}$"),G)
    
    call <- "print(xtable(tableMat), include.rownames = T,  sanitize.text.function = function(x) x)"
  }
  
  else if(!combine){
    if(output == "q2.5"){
      call <- "print(xtable(zlist_q2.5), include.rownames = T,  sanitize.text.function = function(x) x)"
    }
    else if(output == "median"){
      call <- "print(xtable(zlist_median), include.rownames = T,  sanitize.text.function = function(x) x)"
    }
    else if(output == "q97.5"){
      call <- "print(xtable(zlist_q97.5), include.rownames = T,  sanitize.text.function = function(x) x)"
    }
  }
  
  return(eval(parse(text = call)))
}
