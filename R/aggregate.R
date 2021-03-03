#' Aggregate the Performance Matrices of Multiple Interpolations
#' 
#' Function to aggregate the set of performance matrices, by criterion, using sample statistics of the sampling distribution across K. Resulting object is of class \code{'aggregate'}.
#'  
#' @param pf \code{pf}; A nested list of dimension D x M x P x G x K (result of \code{performance()}), where the terminal node is a performance matrix.
#' @param custom \code{character}; A vector of names of user-defined functions used to perform aggregation with custom statistics (see details) 
#' 
#' @details The base statistics provided in the output are as follows:
#' \itemize{
#' \item \code{(mean)}; mean
#' \item \code{(sd)}; standard deviation 
#' \item \code{(q0)}; minimum (0\% quantile)
#' \item \code{(q2.5)}; 2.5\% quantile
#' \item \code{(q25)}; 25\% quantile
#' \item \code{(median)}; median (50\% quantile)
#' \item \code{(q75)}; 75\% quantile
#' \item \code{(q97.5)}; 97.5\% quantile
#' \item \code{(q100)}; maximum (100\% quantile)
#' \item \code{(iqr)}; IQR (75\% quantile - 25\% quantile)
#' \item \code{(skewness)}; skewness
#' \item \code{(dip)}; p-value of dip test for unimodality (see \code{?dip} for details)
#' }
#' 
#' @details 
#' Users can define and pass-in their own custom statistics used for the aggregation of the performance metrics, but must adhere to the following rules:
#' \itemize{
#'   \item Inputs are limited to *ONLY* single numeric vectors \cr
#'   \item Outputs must be a single numeric value
#'   }
#'   
#' @examples
#' 
#'  # User-defined functions to calculate a custom aggregation statistic (see Details for rules)
#'  
#'  my_stat1 <- function(x){
#'   
#'   val <- sum(x)/length(x) + 34
#'   
#'   return(val) # return value must be a single numeric element
#'   
#'   }
#'   
#'  my_metric2 <- function(x){
#'  
#'   val <- (sum(x)-min(x))/6
#'  
#'   return(val) # return value must be a single numeric element
#'  
#'  } 
#'  
#'  # Implementing in aggregate()
#'  
#'  aggregate(pf = pf, custom = c("my_stat1", "my_stat2"))
#'      

aggregate <- function(pf, custom = NULL){
  
  if(class(pf) != "pf") stop("'pf' object must be of class 'pf'. Use performance() to generate such objects.")
  
  if(!is.null(custom)){
    
    ####################
    ### LOGICAL CHECKS
    ####################
    
    n_custom <- length(custom)
    
    if(n_custom == 1){
      is_fn <- !inherits(try(match.fun(custom), silent = TRUE), "try-error") # FALSE if not a function
    }
    else if(n_custom > 1){
      is_fn <- logical(length(custom))
      for(k in 1:n_custom){
        is_fn[k] <- !inherits(try(match.fun(custom[k]), silent = TRUE), "try-error") # FALSE if not a function
      }
    }
    
    if(!all(is_fn)){
      not <- which(!is_fn)
      stop(c("Custom statistic(s): ", paste0(custom[not], sep = " ") ,", are not of class 'function'."))
    }
    
    # Check that the output of the function is a single value
    
    check_single <- function(fn){
      
      x <- rnorm(10)
      
      val <- match.fun(fn)(x = x)
      
      return(all(length(val) == 1, is.numeric(val)))
    }
    
    logic <- logical(n_custom)
    
    for(k in 1:n_custom){
      logic[k] <- check_single(custom[k])
    }
    
    if(!all(logic)){
      stop(c("Custom function(s): ", paste0(custom[!logic], sep = " "), ", do not return a single numeric value."))
    }
  }
    

    ########################
    # DEFINING SKEW FUNCTION
    ########################
      

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
    
  ###############################
  # CONSTRUCTING AGGREGATED DATA
  ###############################  

  D <- length(pf)
  M <- length(pf[[1]])
  P <- length(pf[[1]][[1]])
  G <- length(pf[[1]][[1]][[1]])
  K <- length(pf[[1]][[1]][[1]][[1]])
  C <- length(pf[[1]][[1]][[1]][[1]][[1]])
  
  dataset <- 1:D
  
  # Initializing nested list object
  
  agObject <- lapply(agObject <- vector(mode = 'list', D),function(x)
    lapply(agObject <- vector(mode = 'list', P),function(x) 
      lapply(agObject <- vector(mode = 'list', G),function(x) 
        x<-vector(mode='list',M))))
  
  prop_vec_names <- numeric(P)
  gap_vec_names <- numeric(G)
  method_names <- character(M)
  
  prop_vec <- as.numeric(gsub("p","",names(pf[[1]][[1]])))
  gap_vec <- as.numeric(gsub("g","",names(pf[[1]][[1]][[1]])))
  
  
  for(d in 1:D){
    for(p in 1:P){
      prop_vec_names[p] <- c(paste("p", prop_vec[p],sep="")) # vector of names
      for(g in 1:G){
        gap_vec_names[g] <- c(paste("g", gap_vec[g],sep="")) # vector of names
        for(m in 1:M){
          method_names[m] <- names(pf[[1]])[m]
          
            # compute the mean and distribution of the performance criteria in each (d,m,p,g) specification across all k pairs of (x,X) and 
            # store results in a list of data frames
          
            quantiles <- apply(sapply(pf[[d]][[m]][[p]][[g]],unlist),1, 
                               FUN=function(x) quantile(x, probs = c(0, 0.025, 0.25, 0.75, 0.975, 1.0), na.rm = TRUE))
            
            agObject[[d]][[p]][[g]][[m]] <- data.frame(
              
              mean = rowMeans(sapply(pf[[d]][[m]][[p]][[g]],unlist), na.rm = TRUE),
              
              sd = apply(sapply(pf[[d]][[m]][[p]][[g]],unlist),1,sd, na.rm = TRUE),
              
              #q0 = apply(sapply(pf[[d]][[m]][[p]][[g]],unlist),1,quantile, na.rm = TRUE)["0%",],
              q0 = quantiles["0%",],
              
              #q2.5 = apply(sapply(pf[[d]][[m]][[p]][[g]],unlist),1, 
                           #FUN=function(x) quantile(x, probs = c(0.025,0.975), na.rm = TRUE))["2.5%",],
              q2.5 = quantiles["2.5%",],
              
              #q25 = apply(sapply(pf[[d]][[m]][[p]][[g]],unlist),1,quantile, na.rm = TRUE)["25%",],
              q25 = quantiles["25%",],
              
              median = apply(sapply(pf[[d]][[m]][[p]][[g]],unlist),1,median, na.rm = TRUE),
              
              #q75 = apply(sapply(pf[[d]][[m]][[p]][[g]],unlist),1,quantile, na.rm = TRUE)["75%",],
              q75 = quantiles["75%",],
              
              #q97.5 = apply(sapply(pf[[d]][[m]][[p]][[g]],unlist),1, 
                            #FUN=function(x) quantile(x, probs = c(0.025,0.975), na.rm = TRUE))["97.5%",],
              q97.5 = quantiles["97.5%",],
              
              #q100 = apply(sapply(pf[[d]][[m]][[p]][[g]],unlist),1,quantile, na.rm = TRUE)["100%",],
              q100 = quantiles["100%",],
              
              iqr = quantiles["75%",] - quantiles["25%",],
              
              skewness = apply(sapply(pf[[d]][[m]][[p]][[g]],unlist),1,skew), 
              
              dip = apply(sapply(pf[[d]][[m]][[p]][[g]],unlist),1,
                          FUN = function(x){dip.test(x,simulate.p.value = TRUE)$p.value
                            
                            
              }),
              
              gap_width = c(rep(gap_vec[g], C)),
              prop_missing = c(rep(prop_vec[p],C)),
              dataset = c(rep(dataset[d],C)), 
              method = rep(method_names[m],C) 
            )
            
            if(!is.null(custom)){
              
              # Computing custom aggregate statistics
              
              return_call <- character(n_custom)
              
              for(k in 1:n_custom){
              
                return_call[k] <- paste0("agObject[[d]][[p]][[g]][[m]] <- cbind(agObject[[d]][[p]][[g]][[m]], ",
                                         custom[k]," = apply(sapply(pf[[d]][[m]][[p]][[g]], unlist), 1, match.fun(",custom[k],")))")
                
                eval(parse(text = return_call[k]))
              
              }
            }
          
            
        }
        names(agObject[[d]][[p]][[g]]) <- method_names 
      }
      names(agObject[[d]][[p]]) <- gap_vec_names 
    }
    names(agObject[[d]]) <- prop_vec_names 
  }
  names(agObject) <- names(pf)
  
  class(agObject) <- "aggregate"
    return(agObject)

  
} 
