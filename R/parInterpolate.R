#### parInterpolate ####
### Function to perform interpolation on the gappy series in parallel using user-specified methods and datasets
### class(gappyTS) == numeric; Gappy time series vector
### class(methods) == numeric; vector of IDs for selected interpolation methods (1:18 for all)

parInterpolate <- function(gappyTS, methods){ 
  
  #Creating a list object to store interpolated series
  int_series <- lapply(int_series <- vector(mode = 'list',length(methods)),function(x)
    lapply(int_series <- vector(mode = 'list', length(gappyTS)),function(x) 
      lapply(int_series <- vector(mode = 'list',length(gappyTS[[1]])),function(x) 
        x<-vector(mode='list',length(gappyTS[[1]][[1]])))))
  
  ## Would be nice to wrap function in mclapply() instead of for()... 
  # but the irony is that it will take too much time to learn how to do! :) 
  
  method_names <- numeric(length(methods))
  
  for(m in 1:length(methods)){ 
    method_names[m] <- algorithm_names[methods[m]]
    
    if(methods[m] == 18){
      function_call <- paste(algorithm_calls[methods[m]], "x", ")","[[1]]", sep = "")
    }
    else{
      function_call <- paste(algorithm_calls[methods[m]], "x", ")", sep = "")
    }
    
    int_series[[m]] <- mclapply(gappyTS, function(x){
      lapply(x, function(x){
        lapply(x, function(x){
          eval(parse(text = function_call))}
        )}
      )}, 
      mc.cores = detectCores())
  }
  
  names(int_series) <- method_names
  return(int_series)
}
