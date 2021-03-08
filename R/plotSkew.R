#' Aggregate and plot the skewness statistics over all simulations
#' 
#' Function to display the sampling distributions of the skewness statistics for each metric across all simulations. 
#' Sampling distributions with a sample mean roughly centered at zero indicate that on average the metric has a reliably 
#' symmetric distribution.  Distributions of skewness values whose center is not in the neighbourhood of zero (-s/3 <= Xbar <= s/3) indicate that 
#' on average the metric has a skewed distribution, and the median will better capture the essence of the data.
#' 
#' @param agObject \code{aggregate}; A list object (result of \code{aggregate.R}) of aggregated performance metrics
#' @param cptwise \code{logical}; Whether to display plots individually (histograms) or together as a density plot
#' @param symmetric \code{logical}; TRUE = Display only symmetric metrics, FALSE = Display only asymmetric metrics, NULL = Display all metrics
#' @param output \code{character}; "plots" (default) or "table".

plotSkew <- function(agObject, cptwise=F, show_symmetric= NULL, output = "plots", metric = rownames(agObject[[1]][[1]][[1]][[1]])){
  
  D <- length(agObject)
  P <- length(agObject[[1]])
  G <- length(agObject[[1]][[1]])
  M <- length(agObject[[1]][[1]][[1]])
  C <- nrow(agObject[[1]][[1]][[1]][[1]][metric,])

  stopifnot((is.logical(cptwise) | is.logical(show_symmetric)), class(agObject) == "aggregate")
  
  skews <- matrix(ncol = D*P*G*M, nrow = nrow(agObject[[1]][[1]][[1]][[1]][metric,]))
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
          
          skewcol <- agObject[[d]][[p]][[g]][[m]][metric,"skewness"]
          q1col <- agObject[[d]][[p]][[g]][[m]][metric,"q25"]
          q3col <- agObject[[d]][[p]][[g]][[m]][metric,"q75"]
          skews[,i] <- skewcol
          q1s[,i] <- q1col
          q3s[,i] <- q3col
          
          i <- i + 1
        }
      }
    }
  }
  
  names <- rownames(agObject[[1]][[1]][[1]][[1]])
  names <- names[which(names %in% metric)]
  
  rownames(skews) = names
  rownames(q1s) = names
  rownames(q3s) = names
  
  skews <- data.frame(t(skews))
  
  skewMeans <- data.frame(key = colnames(skews), value = apply(skews,2,mean))
  skewMeds <- data.frame(key = colnames(skews), value = apply(skews,2,median))
  skewSkews <- data.frame(key = colnames(skews), value = apply(skews,2,skew))
  
  
  gather <- gather(skews)
  
  symmetric <- rownames(skewMeans[which(skewMeans$value >= -sd(skewMeans$value)/3 & skewMeans$value <= sd(skewMeans$value)/3),])
  symmetricIn <-match(symmetric,rownames(skewMeans))

  skewCols <- data.frame(key = colnames(skews), value = rep("white",length(colnames(skews))))
  skewCols$value = as.character(skewCols$value)
  skewCols[symmetricIn,]$value = c("grey63")
  
  sym <- gather[gather$key %in% symmetric,]
  asym <- gather[!(gather$key %in% symmetric),]
  
  skewPlot <- list()
  
  if(cptwise){
    if(is.null(show_symmetric)){
      Cc = c(1:C)
      my.data = gather
    }
    
    else if(show_symmetric){
      Cc = c(1:C)[symmetricIn]
      my.data = sym
    }
    
    else if(!show_symmetric){
      Cc = c(1:C)[-symmetricIn]
      my.data = asym
    }
    
      my.means = skewMeans[Cc,]
      my.meds = skewMeds[Cc,]
      my.cols = skewCols[Cc,]
      
      
      my.data$colour = rep("white",nrow(my.data))
      my.data$colour[!my.data$key %in% symmetric] = "white"
      my.data$colour[my.data$key %in% symmetric] = "grey"
    
      thePlot <- ggplot(my.data, aes(x = value)) + 
        theme_light() + 
        facet_wrap(~ key, strip.position = "top", scales = "free", ncol = 3) + 
        theme(strip.background = element_rect(fill="white"),
              strip.text = element_text(colour = 'black'),
              strip.placement = "outside",
              panel.grid.major.x = element_blank(),
              panel.grid.minor.x = element_blank(),
              panel.grid.major.y = element_blank()
              ) + 
        geom_histogram(aes(y=..density.., col= colour), binwidth = 1, fill = "white", show.legend=FALSE) + 
        #scale_y_continuous(breaks=c(seq(-30,30, by = 5)))+
      
        geom_vline(data = my.means, aes(xintercept = value), lty = 1, lwd = 0.25, col = "black") + 
        geom_vline(data = my.meds, aes(xintercept = value), lty = 2, lwd = 0.25, col = "black") +
        labs(x="skewness")+
        scale_colour_grey(start=0.1,end=0.6)
  
  }
  
  else if(!cptwise){
    if(!is.null(show_symmetric)){
      if(show_symmetric){
      thePlot <- ggplot(sym) + geom_density(aes(x = sym$value, color = sym$key), lwd = 0.25) + theme_light() + 
        xlim(-5,5) + xlab("skewness") + labs(color = "metriceria") + 
        scale_colour_manual(values = colorRampPalette(c("blue","pink","turquoise"))(length(symmetric))) #tidyr
      }
      else if(!show_symmetric){
      thePlot <- ggplot(asym) + geom_density(aes(x = asym$value, color = asym$key), lwd = 0.25) + theme_light() + 
        xlim(-5,5) + xlab("skewness") + labs(color = "metriceria") + 
        scale_colour_manual(values = colorRampPalette(c("blue","pink","turquoise"))(C-length(symmetric))) #tidyr
      }
    }
    
    else if(is.null(show_symmetric)){
      thePlot <- ggplot(gather) + geom_density(aes(x = gather$value, color = gather$key), lwd = 0.25) + theme_light() + 
        xlim(-5,5) + xlab("skewness") + labs(color = "metriceria") + 
        scale_colour_manual(values = colorRampPalette(c("blue","pink","turquoise"))(C)) #tidyr
    }
  }

    vals <- format(round(skewMeans$value,2),nsmall = 2)
    
    vals[symmetricIn] <- paste0("\\textbf{", vals[symmetricIn], "}") # bold non-skewed metriceria
    
    theTable <- data.frame(rownames(skewMeans),vals)
    colnames(theTable) <- c("metricerion","mean skewness")
    
    if(output == "plots"){
      return(thePlot)
    }
    
    else if(output == "table"){
      return(theTable)
    }
  

}






