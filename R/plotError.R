#' Plot Interpolation Error
#' 
#' Function to generate a plot to visualize the absolute difference between the original time series and the interpolated time series under a specific gap structure.  If more than one value of \code{k} is selected, the resulting plot represents the average interpolation.
#' 
#' @param OriginalData \code{list}; A list containing the original time series
#' @param GappyData \code{list}; A list containing the gappy time series
#' @param IntData \code{list}; A list containing the interpolated time series
#' @param d \code{character}; A single character element describing the dataset of interest
#' @param p \code{numeric}; A single numeric element describing the of 'proportion missing' of interest
#' @param g \code{numeric}; A single numeric element describing the 'gap length' of interest
#' @param m \code{character}; A single character element describing the interpolation method of interest 
#' @param output \code{character}; "plot" outputs a plot object, "numeric" outputs a numeric vector corresponding to the plot
#' @param error_color \code{character}; A character element describing the color (HTML: "#XXXXXX") to use for the error ribbon
#' @param alpha \code{numeric}; A numeric value describing the opacity of the error ribbon
#' @param k \code{numeric}; Numeric element(s) describing individual time series interpolations to apply the averaging over
#' 

plotError <- function(OriginalData, IntData, GappyData, d, p, g, m, output = "plot", error_color = "black", k = NULL, alpha = 0.3){
  
  ###################
  # LOGICAL CHECKS
  ###################
  
  stopifnot(is.list(OriginalData), 
            is.list(IntData), 
            is.list(GappyData),
            length(m) == 1, 
            length(p) == 1, 
            length(g) == 1, 
            length(d) == 1,
            is.numeric(alpha),
            length(alpha) == 1)

  if(output != "plot" & output != "numeric") stop("'output' must equal either 'plot' or 'numeric'.")
  
  if(alpha < 0 | alpha > 1){ stop("Please choose a value of 'alpha' between 0 and 1.")}
  if(!m %in%  names(IntData[[1]])) stop("Method(s) '", paste0(m[!m %in% names(IntData[[1]])], collapse = ", ' "),"' not found. Possible choices are: '", paste0(names(IntData[[1]]), collapse = "', '"),"'.")
  if(!d %in% names(IntData)) stop("Dataset(s) ", paste0(d[!d %in% names(IntData)], collapse = ", ")," not found. Possible choices are: ", paste(names(IntData), collapse = ','))

  prop_vec_names <- names(IntData[[1]][[1]])
  gap_vec_names <- names(IntData[[1]][[1]][[1]])
  
  prop_vec <- as.numeric(gsub("p", "", prop_vec_names))
  gap_vec <- as.numeric(gsub("g", "", gap_vec_names))
  
  if(!p %in% prop_vec) {stop(paste0("Selected proportion missing value: ", paste(p[which(!p %in% prop_vec)], collapse = " ,"), " not found in IntData. Possible choices are: ", paste(prop_vec, collapse = ",")))}
  if(!g %in% gap_vec) {stop(paste0("Selected gap width value: ", paste(g[which(!g %in% gap_vec)], collapse = " ,"), " not found in IntData. Possible choices are: ", paste(gap_vec, collapse = ",")))}

  wrong_k <- k[which(k>length(IntData[[1]][[1]][[1]][[1]]) | k < 1)]
  
  if(!is.null(k)){
    if(any(k > length(IntData[[1]][[1]][[1]][[1]])) | any(k <1)) { stop(paste0("Selected index/indices: k = ", paste(wrong_k, collapse = ", "), " are outside the range of possible time series. Please make your selection(s) from integers 1 to ", length(IntData[[1]][[1]][[1]][[1]]),"."))}
  }
  
  ###################
  # Initialization
  ###################

  avInt <- list()
  sdInt <- list()
  
  # Initializing names
  gap__names <- numeric(G)
  prop_names <- numeric(P)
  method_names <- m
  data_names <- d

  the_props <- prop_vec_names[which(prop_vec %in% p)]
  the_gaps <-  gap_vec_names[which(gap_vec %in% g)]
  
  if(is.null(k)){
    k <- 1:length(IntData[[d]][[m]][[the_props]][[the_gaps]])
  }
  
  avInt <- apply(sapply(IntData[[d]][[m]][[the_props]][[the_gaps]][k],unlist),1,mean)
  sdInt <- apply(sapply(IntData[[d]][[m]][[the_props]][[the_gaps]][k],unlist),1,sd)
  
  #############################
  # Constructing plot
  #############################
  
    ###########################
    # Retrieving gap data
    ###########################
  
    gappy <- GappyData[[d]][[the_props]][[the_gaps]][k] # the gappy data
    int <- IntData[[d]][[m]][[the_props]][[the_gaps]][k] # the interpolated data
    orig <- OriginalData[[d]] # the original data
    
    K <- length(k)
    N <- length(orig)
    
    which_removed <- list() # index positions of NA values
    which_n_removed <- list() # index positions of NA values not removed
    removed_vals <- list() # Corresponding original values that are now NA
    int_values <- list() # Corresponding interpolated values
    sandwich <- list()
    int_connect <- list()
    orig_connect <- list()
    
    for(vk in 1:K){
      
      # Index position of removed values
      
      which_removed[[vk]] <- which(is.na(gappy[[vk]]))
      
      # Removed values from original time series (black marker, pch hollow)
      
      removed_vals[[vk]] <- rep(NA, N)
      removed_vals[[vk]][which_removed[[vk]]] <- orig[which_removed[[vk]]]
      
      # Interpolated values (red marker, pch solid)
      
      int_values[[vk]] <- rep(NA, N)
      int_values[[vk]][which_removed[[vk]]] <- int[[vk]][which_removed[[vk]]]
      
      # Including index positions before and after gap
      
      sandwich[[vk]] <- c((which_removed[[vk]] - 1), (which_removed[[vk]] + 1))
      
      # Finding unique index positions
      
      sandwich[[vk]] <- c(which_removed[[vk]], sandwich[[vk]])
      sandwich[[vk]] <- unique(sandwich[[vk]])
      sandwich[[vk]] <- sandwich[[vk]][order(sandwich[[vk]], decreasing = FALSE)]
      
      # Red line, int values
      
      int_connect[[vk]] <- rep(NA, N)
      int_connect[[vk]][sandwich[[vk]]] <- int[[vk]][sandwich[[vk]]] 
      
      # Black line, orig values
      
      which_n_removed[[vk]] <- c(1:N)[!c(1:N) %in% which_removed[[vk]]]
      
      orig_connect[[vk]] <- rep(NA, N)
      orig_connect[[vk]][which_n_removed[[vk]]] <- orig[which_n_removed[[vk]]]
      
    }
    
    
    ####################
    # collapse across k
    ####################
    
    ## INDEX POSITIONS
    
    # Values that were removed 
    
    which_removed <- unique(unlist(which_removed))
    which_removed <- which_removed[order(which_removed, decreasing = FALSE)]
    
    # Values that were not removed
    
    which_n_removed <- c(1:N)[which(!c(1:N) %in% which_removed)]
    
    # Values to make red line contiguous with black line
    
    sandwich <- unique(unlist(sandwich))
    sandwich <- sandwich[order(sandwich, decreasing = FALSE)]
    
    #### Y VALUES
    
    # lines
    black_line <- rep(NA, N)
    black_line[which_n_removed] <- orig[which_n_removed]
    
    red_line <- rep(NA,N)
    red_line[sandwich] <- avInt[sandwich]
    
    # markers
    black_solid <- rep(NA, N)
    black_solid[which_n_removed] <- orig[which_n_removed]
    
    black_hollow <- rep(NA, N)
    black_hollow[which_removed] <- orig[which_removed]
    
    red_solid <- rep(NA, N)
    red_solid[which_removed] <- avInt[which_removed]

    ############################
    # Constructing plot
    ############################
    
    if(K == 1){
      subtitle = paste("p =", gsub("p","",the_props),
                       ", g =", gsub("g","",the_gaps),
                       ", k =", k)
    }
    
    if(K > 1){
      subtitle = paste("p =", gsub("p","",the_props),
                       ", g =", gsub("g","",the_gaps),
                       "\n","averaged across ",
                       K,
                       " simulations")
    }
    
    
    data <- data.frame(t = 0:(N-1), black_solid, black_hollow, black_line, red_solid, red_line, avInt)
    
    plot <-   ggplot(data)+
      
      # red ribbon
      geom_ribbon(aes(x = t, ymin = as.numeric(orig), ymax = avInt), fill = error_color, alpha = alpha) +
      
      # black solid
      geom_point(shape = 20, size = 1, aes(x = t, y = black_solid, col = "original"), color = "black") +
      
      # black hollow
      geom_point(shape = 1, size = 1, aes(x = t, y = black_hollow, col = "missing"), color = "black") + 
      
      # red solid
      geom_point(shape = 20, size = 1, aes(x = t, y = red_solid, col = "interpolated"), color = error_color) + 
      
      # red line
      geom_line(aes(x = t, red_line), color = error_color, size = 0.2) + 
      
      # black line
      geom_line(aes(x = t, black_line), color = "black", size = 0.2) + 
      
      # red errors, dashed
      geom_errorbar(aes(x = t, ymin = black_hollow, ymax = red_solid), size = 0.1, color = error_color, linetype = "dashed") + 
    
      ggtitle(paste(d,": Original v.s. Interpolated Data (", m, ")"), 
              subtitle = subtitle)+
      
      labs(x = "time", 
           y = "value",
           label = "Legend Text") + 
      
      theme(plot.title = element_text(hjust = 0.5), 
            plot.subtitle = element_text(hjust = 0.5),
            panel.background = element_rect(fill = "gray96", color = "gray90"))
  
  
  if(output == "plot"){
    return(plot)
  }
  else if(output == "numeric"){
    return(avInt)
  }

}


