# Want to build a plot function where you can select multiple parameters (d,p,g,m) and compare across one. cross section parameter? 
# crossSec = p, where Each plot is a different proportion missing, aggregate everything else
# crossSec = g, where Each plot is a different gap length, aggregate everything else
# crossSec = m, where Each plot is a different method, aggregate everything else
# crossSec = d, where Each plot is a different dataset, aggregate everything else.
# If any p,g,m,d is a vector (length>1) then do multiple plots under each value of that vector. 

multiPlot <- function(avIntList, crossSec = NULL){
  
  D <- length(avIntList)
  M <- length(avIntList[[1]])
  P <- length(avIntList[[1]][[1]])
  G <- length(avIntList[[1]][[1]][[1]])
  
  d_mean <- list()
  m_mean <- list()
  p_mean <- list()
  g_mean <- list()
  
  df <- data.frame(avIntList)
  
  if(crossSec == "d"){
    for(d in 1:D){
      # take the mean across all p,g,m
      d_mean[[d]] <- apply(df[,grepl(paste("D",d,sep=""), colnames(df))],1,mean)
    }
  }
  
  else if(crossSec == "p"){
    # take the mean across all d, g, m
    for(p in 1:P){
      p_mean[[p]] <- apply(df[,grepl(paste(".",prop_vec_names[p],".",sep=""), colnames(df))],1,mean)
    }
  }
  
  else if(crossSec == "g"){
    # take the mean across all d,p,m
    for(g in 1:G){
      g_mean[[g]] <- apply(df[,grepl(paste(".",gap_vec_names[g],sep=""), colnames(df))],1,mean)
    }
  }
  
  else if(crossSec == "m"){
    # take the mean across all d, p, g
    for(m in 1:M){
      m_mean[[m]] <- apply(df[,grepl(paste(method_list_names[m],sep=""), colnames(df))],1,mean)
    }
  }
  

}
  
  