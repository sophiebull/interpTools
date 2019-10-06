#' Plot Grid of Performance Metrics
#' 
#' Function to generate a grid of plots for visualizing a selection of performance metrics (median). 
#'  
#' @param agEval A list containing the aggregated performance metrics (result of agEval.R)
#' @param d A vector of datasets of interest (must be a subset of agEval)
#' @param p A vector of 'proportion missings' of interest (must be a subset of agEval)
#' @param g A vector of 'gap length' of interest (must be a subset of agEval)
#' @param m A vector of interpolation methods of interest (must be a subset of agEval)
#' @param crit A character vector of performance metrics of interest

plotMetrics <- function(agEval,d=1:length(agEval),
                        p=1:length(agEval[[1]]),
                        m=1:length(agEval[[1]][[1]][[1]]),
                        g=1:length(agEval[[1]][[1]]),
                        crit){

  require(ggplot2)
  
D <- length(d)
P <- length(p)
M <- length(m)
G <- length(g)
C <- length(crit)

criterion <- rownames(agEval[[1]][[1]][[1]][[1]])
maximize <- c(1,1,rep(0,11),1,rep(0,3)) # 1 = yes, 0 = no
optimal <- maximize
optimal[which(optimal == "1")] <- "max"
optimal[which(optimal == "0")] <- "min"

best <- data.frame(criterion = criterion, 
                   maximize = maximize,
                   optimal = optimal) 

metDF <- lapply(plotList <- vector(mode = 'list', D),function(x)
  lapply(plotList <- vector(mode = 'list', P),function(x) 
    lapply(plotList <- vector(mode = 'list', G),function(x) 
      x<-vector(mode='list',C))))

metPlot <- metDF

  for(vd in 1:D){
    for(vp in 1:P){
        for(vg in 1:G){
              for(s in 1:C){
                
                median <- numeric(M)
                sd <- numeric(M)
                method <- numeric(M)
                criteria <- numeric(M)
                
                for(vm in 1:M){
                median[vm] <- agEval[[d[vd]]][[p[vp]]][[g[vg]]][[m[vm]]][crit[s],"median"]
                sd[vm] <- agEval[[d[vd]]][[p[vp]]][[g[vg]]][[m[vm]]][crit[s],"sd"]
                method[vm] <- as.character(agEval[[d[vd]]][[p[vp]]][[g[vg]]][[m[vm]]][crit[s],"method"])
                criteria[vm] <- crit[s]
              }
          
            metDF[[vd]][[vp]][[vg]][[s]] <-   data.frame(median = median, sd = sd, method = method, criteria = criteria)
      
            metPlot[[vd]][[vp]][[vg]][[s]] <- ggplot(metDF[[vd]][[vp]][[vg]][[s]],aes(x=method,y=median)) +

              geom_col() + 
              
              geom_bar(data=subset(metDF[[vd]][[vp]][[vg]][[s]], 
                                   eval(parse(text = paste("median==",best[(criterion==crit[s]),'optimal'],"(median)", sep = "")))), 
                       aes(method, median),
                       fill="mistyrose2", stat="identity") + 
              
              geom_errorbar(aes(ymin = median-sd, ymax = median+sd), width=0.2)+
            
              ggtitle(paste(crit[s],", optimal = ",best[(criterion == crit[s]),'optimal'],sep=""),
                      subtitle = paste("D",d[vd],", p=",prop_vec[p[vp]],", g=",gap_vec[g[vg]],sep=""))+
              labs(y="value", x = "")+ 
              theme(axis.text.x = element_text(angle = 45, hjust = 1))
              }
          
          
          names(metDF[[vd]][[vp]][[vg]]) <- crit
          names(metPlot[[vd]][[vp]][[vg]]) <- crit
        }
      names(metDF[[vd]][[vp]]) <- names(agEval[[1]][[1]])[g]
      names(metPlot[[vd]][[vp]]) <- names(agEval[[1]][[1]])[g]
    }
    names(metDF[[vd]]) <- names(agEval[[1]])[p]
    names(metPlot[[vd]]) <- names(agEval[[1]])[p]
  }
names(metDF) <- names(agEval)[d]
names(metPlot) <- names(agEval)[d]

# PLOT GRID
require(gridExtra)

flattenlist <- function(x){  
  morelists <- sapply(x, function(xprime) class(xprime)[1]=="list")
  out <- c(x[!morelists], unlist(x[morelists], recursive=FALSE))
  if(sum(morelists)){ 
    Recall(out)
  }else{
    return(out)
  }
}

plot.grid <- do.call(grid.arrange, args = list(grobs = flattenlist(metPlot), ncol = C, nrow = P*G*D))

return(plot.grid)
}

