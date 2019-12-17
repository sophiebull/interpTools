#' Aggregate and plot the skewness statistics over all simulations
#' 
#' Function to display the sampling distributions of the skewness statistics for each criterion across all simulations. 
#' Sampling distributions with a sample mean roughly centered at zero indicate that on average the criterion has a reliably 
#' symmetric distribution.  Distributions of skewness values whose center is not in the neighbourhood of zero (-s/3 <= Xbar <= s/3) indicate that 
#' on average the criterion has a skewed distribution, and the median will better capture the essence of the data.
#' 
#' @param agEval A list object (result of agEval.R) of aggregated performance metrics
#' @param cptwise logical; Whether to display plots individually (histograms) or together as a density plot
#' @param symmetric logical; TRUE = Display only symmetric criteria, FALSE = Display only asymmetric criteria, NULL = Display all criteria
#' @param output character; "plots" (default) or "table".

plotSkew <- function(agEval, cptwise=F, symmetric= NULL, output = "plots"){
  
  D <- length(agEval)
  P <- length(agEval[[1]])
  G <- length(agEval[[1]][[1]])
  M <- length(agEval[[1]][[1]][[1]])
  C <- nrow(agEval[[1]][[1]][[1]][[1]])

  stopifnot((is.logical(cptwise) | is.logical(symmetric)), class(agEval) == "agEvaluate")
  
  skews <- matrix(ncol = D*P*G*M, nrow = nrow(agEval[[1]][[1]][[1]][[1]]))
  q1s <- skews
  q3s <- q1s
  
  skew <- function(x, na.rm = TRUE){
    stopifnot(is.numeric(x))
    
    if(na.rm){
      x <- x[!is.na(x)]
      sk <- (sum((x-mean(x))^3)/(length(x)*sd(x)^3))
    }
    
    else if(!na.rm){
      sk <- (sum((x-mean(x))^3)/(length(x)*sd(x)^3))
    }
    
    return(sk)
  }
  
  i <- 1
  
  for(d in 1:D){
    for(p in 1:P){
      for(g in 1:G){
        for(m in 1:M){
          
          skewcol <- agEval[[d]][[p]][[g]][[m]][,"skewness"]
          q1col <- agEval[[d]][[p]][[g]][[m]][,"q25"]
          q3col <- agEval[[d]][[p]][[g]][[m]][,"q75"]
          skews[,i] <- skewcol
          q1s[,i] <- q1col
          q3s[,i] <- q3col
          
          i <- i + 1
        }
      }
    }
  }
  
  rownames(skews) = rownames(agEval[[1]][[1]][[1]][[1]])
  rownames(q1s) = rownames(agEval[[1]][[1]][[1]][[1]])
  rownames(q3s) = rownames(agEval[[1]][[1]][[1]][[1]])
  
  skews <- data.frame(t(skews))
  
  skewMeans <- data.frame(key = colnames(skews), value = apply(skews,2,mean))
  skewMeds <- data.frame(key = colnames(skews), value = apply(skews,2,median))
  skewSkews <- data.frame(key = colnames(skews), value = apply(skews,2,skew))
  
  
  gather <- gather(skews)
  
  symCrit <- rownames(skewMeans[which(skewMeans$value >= -sd(skewMeans$value)/3 & skewMeans$value <= sd(skewMeans$value)/3),])
  symCritIn <-match(symCrit,rownames(skewMeans))

  skewCols <- data.frame(key = colnames(skews), value = rep("white",length(colnames(skews))))
  skewCols$value = as.character(skewCols$value)
  skewCols[symCritIn,]$value = c("grey63")
  
  sym <- gather[gather$key %in% symCrit,]
  asym <- gather[!(gather$key %in% symCrit),]
  
  skewPlot <- list()
  
  if(cptwise){
    if(is.null(symmetric)){
      Cc = c(1:C)
      my.data = gather
    }
    
    else if(symmetric){
      Cc = c(1:C)[symCritIn]
      my.data = sym
    }
    
    else if(!symmetric){
      Cc = c(1:C)[-symCritIn]
      my.data = asym
    }
    
      my.means = skewMeans[Cc,]
      my.meds = skewMeds[Cc,]
      my.cols = skewCols[Cc,]
      
      my.data$colour = rep("white",nrow(my.data))
      my.data$colour[!my.data$key %in% symCrit] = "white"
      my.data$colour[my.data$key %in% symCrit] = "grey"
    
      p <- ggplot(my.data, aes(x = value)) + 
        theme_light() + 
        facet_wrap(~ key, strip.position = "top", scales = "free", ncol = 3) + 
        theme(strip.background = element_rect(fill="white"),
              strip.text = element_text(colour = 'black'),
              strip.placement = "outside") + 
        geom_histogram(aes(y=..density.., col= colour), binwidth = 1, fill = "white", show.legend=FALSE) + 
        geom_vline(data = my.means, aes(xintercept = value), lty = 1, lwd = 0.25, col = "black") + 
        geom_vline(data = my.meds, aes(xintercept = value), lty = 2, lwd = 0.25, col = "black") +
        labs(x="skewness")+
        scale_colour_grey(start=0.1,end=0.6)
      
  return(p)
  }
  
  else if(!cptwise){
    if(!is.null(symmetric)){
      if(symmetric){
      thePlot <- ggplot(sym) + geom_density(aes(x = sym$value, color = sym$key), lwd = 0.25) + theme_light() + 
        xlim(-5,5) + xlab("skewness") + labs(color = "criteria") + 
        scale_colour_manual(values = colorRampPalette(c("blue","pink","turquoise"))(length(symCrit))) #tidyr
      }
      else if(!symmetric){
      thePlot <- ggplot(asym) + geom_density(aes(x = asym$value, color = asym$key), lwd = 0.25) + theme_light() + 
        xlim(-5,5) + xlab("skewness") + labs(color = "criteria") + 
        scale_colour_manual(values = colorRampPalette(c("blue","pink","turquoise"))(C-length(symCrit))) #tidyr
      }
    }
    
    else if(is.null(symmetric)){
      thePlot <- ggplot(gather) + geom_density(aes(x = gather$value, color = gather$key), lwd = 0.25) + theme_light() + 
        xlim(-5,5) + xlab("skewness") + labs(color = "criteria") + 
        scale_colour_manual(values = colorRampPalette(c("blue","pink","turquoise"))(C)) #tidyr
    }
    

    vals <- format(round(skewMeans$value,2),nsmall = 2)
    
    vals[symCritIn] <- paste0("\\textbf{", vals[symCritIn], "}") # bold non-skewed criteria
    
    theTable <- data.frame(rownames(skewMeans),vals)
    colnames(theTable) <- c("criterion","mean skewness")
    
    if(output == "plots"){
    return(thePlot)
    }
    
    else if(output == "table"){
      return(theTable)
    }
  }

}






