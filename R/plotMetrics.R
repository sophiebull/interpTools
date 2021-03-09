#' Plot Grid of Performance Metrics
#' 
#' Function to generate a grid of plots for visualizing a selection of performance metrics (median). 
#'  
#' @param agObject \code{aggregate_pf}; A list containing the aggregated performance metrics (result of \code{aggregate_pf()})
#' @param d \code{character}; A vector of datasets of interest
#' @param p \code{numeric}; A vector of 'proportion missings' of interest
#' @param g \code{numeric}; A vector of 'gap length' of interest 
#' @param m \code{character}; A vector of interpolation methods of interest 
#' @param metric \code{character}; A character vector of performance metrics of interest

plotMetrics <- function(agObject,
                        d = names(agObject),
                        p,
                        g,
                        m = names(agObject[[1]][[1]][[1]]),
                        metric,
                        highlight_color = "mistyrose2"){

  
  ######################
  # LOGICAL CHECKS
  ######################
  
  prop_vec_names <- names(agObject[[1]])
  gap_vec_names <- names(agObject[[1]][[1]])
  
  prop_vec <- as.numeric(gsub("p", "", prop_vec_names))
  gap_vec <- as.numeric(gsub("g", "", gap_vec_names))
  
  if(!all(p %in% prop_vec)) {stop(paste0("Selected proportion missing value(s): ", paste(p[which(!p %in% prop_vec)], collapse = " ,"), " not found in agObject."))}
  if(!all(g %in% gap_vec)) {stop(paste0("Selected gap width value(s): ", paste(g[which(!g %in% gap_vec)], collapse = " ,"), " not found in agObject."))}
  
  if(!all(m %in%  names(agObject[[1]][[1]][[1]]))) stop("Method(s) '", paste0(m[!m %in% names(agObject[[1]][[1]][[1]])], collapse = ", ' "),"' not found. Possible choices are: '", paste0(names(agObject[[1]][[1]][[1]]), collapse = "', '"),"'.")
  if(!all(d %in% names(agObject))) stop("Dataset(s) ", paste0(d[!d %in% names(agObject)], collapse = ", ")," not found. Possible choices are: ", paste(names(agObject), collapse = ','))
  if(!all(metric %in% rownames(agObject[[1]][[1]][[1]][[1]]))) stop(paste0("Metric(s) '",paste(metric[which(!metric %in% rownames(agObject[[1]][[1]][[1]][[1]]))], collapse = ","),"' must be one of ", paste(rownames(agObject[[1]][[1]][[1]][[1]]),collapse = ", "),"."))
  
  if(class(agObject) != "aggregate_pf") stop("'agObject' must be of class 'aggregate_pf'")
  
  #######################
  # Initializing indices
  #######################
  
  
  D <- length(d)
  P <- length(p)
  M <- length(m)
  G <- length(g)
  C <- length(metric)
  
  #######################
  # Defining 'optimal'
  #######################
  
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
  
  ##########################
  # Building plots
  ##########################
  
  metDF <- lapply(plotList <- vector(mode = 'list', D),function(x)
    lapply(plotList <- vector(mode = 'list', P),function(x) 
      lapply(plotList <- vector(mode = 'list', G),function(x) 
        x<-vector(mode='list',C))))
  
  metPlot <- metDF
  
  the_props <- prop_vec_names[which(prop_vec %in% p)]
  the_gaps <-  gap_vec_names[which(gap_vec %in% g)]
    
    for(vd in 1:D){
      for(vp in 1:P){
          for(vg in 1:G){
                for(s in 1:C){
                  
                  median <- numeric(M)
                  sd <- numeric(M)
                  method <- numeric(M)
                  metrics <- numeric(M)
                  
                  for(vm in 1:M){
                  median[vm] <- agObject[[d[vd]]][[the_props[vp]]][[the_gaps[vg]]][[m[vm]]][metric[s],"median"]
                  sd[vm] <- agObject[[d[vd]]][[the_props[vp]]][[the_gaps[vg]]][[m[vm]]][metric[s],"sd"]
                  method[vm] <- as.character(agObject[[d[vd]]][[the_props[vp]]][[the_gaps[vg]]][[m[vm]]][metric[s],"method"])
                  metrics[vm] <- metric[s]
                }
            
              metDF[[vd]][[vp]][[vg]][[s]] <-   data.frame(median = median, sd = sd, method = method, metrics = metrics)
        
              metPlot[[vd]][[vp]][[vg]][[s]] <- ggplot(metDF[[vd]][[vp]][[vg]][[s]],aes(x=method,y=median)) +
  
                geom_col() + 
                
                geom_bar(data=subset(metDF[[vd]][[vp]][[vg]][[s]], 
                                     eval(parse(text = paste("median==",best[(best$metrics == metric[s]),'optimal'],"(median)", sep = "")))), 
                         aes(method, median),
                         fill=highlight_color, stat="identity") + 
                
                geom_errorbar(aes(ymin = median-sd, ymax = median+sd), width=0.2)+
              
                ggtitle(paste(metric[s],", optimal = ",best[(best$metrics == metric[s]),'optimal'],sep=""),
                        subtitle = paste(d[vd], ", p=", p[vp],", g=", g[vg],sep=""))+
                labs(y="value", x = "")+ 
                theme(axis.text.x = element_text(angle = 45, hjust = 1))
                }
            
            
            names(metDF[[vd]][[vp]][[vg]]) <- metric
            names(metPlot[[vd]][[vp]][[vg]]) <- metric
          }
        names(metDF[[vd]][[vp]]) <- names(agObject[[1]][[1]][the_gaps])
        names(metPlot[[vd]][[vp]]) <- names(agObject[[1]][[1]][the_gaps])
      }
      names(metDF[[vd]]) <- names(agObject[[1]][the_props])
      names(metPlot[[vd]]) <- names(agObject[[1]][the_props])
    }
  names(metDF) <- names(agObject[d])
  names(metPlot) <- names(agObject[d])
  
  ##########################
  # Plotting as a grid
  ##########################
  
  
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
  
  if(P*G*D > 3){
    message("Encountering layout issues.  Please consider reducing the number of elements in p, g, or d.")
  }
  if(C > 4){
    message("Encountering layout issues. Please consider reducing the number of metrics.")
  }
    
  return(plot.grid)
}

