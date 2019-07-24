#### performance.R ####
# function to calculate the performance metrics for each method (compare original series w/ interpolated series.)
# class(x) == list; original time series 
# class(X) == list; interpolated time series

performance <- function(x,X){
  
  D <- length(X)
  M <- length(X[[1]])
  P <- length(X[[1]][[1]])
  G <- length(X[[1]][[1]][[1]])
  N <- length(X[[1]][[1]][[1]][[1]])
  
  # Initializing nested list object
  Performance <- lapply(Performance <- vector(mode = 'list', D),function(x)
    lapply(Performance <- vector(mode = 'list', M),function(x) 
      lapply(Performance <- vector(mode = 'list', P),function(x) 
        lapply(Performance <- vector(mode = 'list', G),function(x)
          x<-vector(mode='list', N)))))
  
  prop_vec_names <- numeric(P)
  gap_vec_names <- numeric(G)
  method_names <- numeric(M)
  
  # Evaluate the performance criteria for each sample in each (d,m,p,g) specification
  for(d in 1:D){
    for(m in 1:M){
      method_names[m] <- algorithm_names[methods[m]]
      for(p in 1:P){
        prop_vec_names[p] <- c(paste("p", prop_vec[p],sep="")) # vector of names
        for(g in 1:G){
          gap_vec_names[g] <- c(paste("g", gap_vec[g],sep="")) # vector of names
          for(k in 1:N) { 
            Performance[[d]][[m]][[p]][[g]][[k]] <- unlist(eval_performance(x = x[[d]], X = X[[d]][[m]][[p]][[g]][[k]]))
          }
          names(Performance[[d]][[m]][[p]]) <- gap_vec_names
        }
        names(Performance[[d]][[m]]) <- prop_vec_names
      }
      names(Performance[[d]]) <- method_names
    }
    names(Performance) <- names(IntData)
  }
  return(Performance) 
}