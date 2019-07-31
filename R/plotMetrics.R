plotMetrics <- function(agEval,d,p,m,g,metrics = c("MSE","MAPE","abs_differences")){

D <- length(d)
P <- length(p)
M <- length(m)
G <- length(g)
Met <- length(metrics)

criterion <- rownames(agEval[[1]][[1]][[1]][[1]])
maximize <- c(1,1,rep(0,11),1,rep(0,3)) # 1 = yes, 0 = no
optimal <- maximize
optimal[which(optimal == "1")] <- "maximize"
optimal[which(optimal == "0")] <- "minimize"

best <- data.frame(criterion = criterion, 
                   maximize = maximize,
                   optimal = optimal) 

metDF <- lapply(plotList <- vector(mode = 'list', D),function(x)
  lapply(plotList <- vector(mode = 'list', P),function(x) 
    lapply(plotList <- vector(mode = 'list', G),function(x) 
      x<-vector(mode='list',Met))))

metPlot <- metDF

  for(vd in 1:D){
    for(vp in 1:P){
        for(vg in 1:G){
              for(met in 1:Met){
                
                mean <- numeric(M)
                sd <- numeric(M)
                method <- numeric(M)
                metric <- numeric(M)
                
                for(vm in 1:M){
                mean[vm] <- agEval[[d[vd]]][[p[vp]]][[g[vg]]][[m[vm]]][metrics[met],"mean"]
                sd[vm] <- agEval[[d[vd]]][[p[vp]]][[g[vg]]][[m[vm]]][metrics[met],"sd"]
                method[vm] <- as.character(agEval[[d[vd]]][[p[vp]]][[g[vg]]][[m[vm]]][metrics[met],"method"])
                metric[vm] <- metrics[met]
              }
          
            metDF[[vd]][[vp]][[vg]][[met]] <-   data.frame(mean = mean, sd = sd, method = method, metric = metric)
      
            metPlot[[vd]][[vp]][[vg]][[met]] <- ggplot(metDF[[vd]][[vp]][[vg]][[met]],aes(x=method,y=mean)) +

              geom_col() + 
              geom_errorbar(aes(ymin = mean-sd, ymax = mean+sd), width=0.2)+
              ggtitle(paste(metrics[met],", optimal = ",best[(criterion == metrics[met]),'optimal'],sep=""),
                      subtitle = paste("D",d[vd],", p=",prop_vec[p[vp]],", g=",gap_vec[g[vg]],sep=""))+
              labs(y="value")
              }
          
          
          names(metDF[[vd]][[vp]][[vg]]) <- metrics
          names(metPlot[[vd]][[vp]][[vg]]) <- metrics
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

plot.grid <- do.call(grid.arrange, args = list(grobs = flattenlist(metPlot), ncol = Met, nrow = P*G*D))

return(plot.grid)
}

