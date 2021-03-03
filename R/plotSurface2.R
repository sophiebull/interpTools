# take all data sets
# take selection of methods
# take one metric


plotSurface2 <- function(m = names(agObject[[1]][[1]][[1]]),
                         metric = "MSE",
                         f = "median",
                         highlight = NULL,
                         highlight_color = NULL,
                         colours = c("red","blue")){
  
  
  ##########################
  # LOGICAL CHECKS
  ##########################
  
  
  
  
  
  ##########################
  # Retrieving surface vals
  ##########################
  
  z_list <- compileMatrix(agObject = agObject)[[f]]
  
  
  ##########################
  # Initializing indices
  ##########################
  
  M <- length(m)
  P <- nrow()
  
  
  ##########################
  # Creating colour palette
  ##########################
  
  colorList <- colorRampPalette(colors)(M)
  
  colorListMatch <- colorList[1:M]
  names(colorListMatch) <- method_list_names
  
  if(!is.null(highlight)){
    colorListMatch[[highlight]] <- highlight_color
  }
  
  colorListMatch <- rep(colorListMatch, each = P*G)
  palette <- lapply(split(colorListMatch, names(colorListMatch)), unname)
  palette <- palette[method_list_names]
  
  
  
  
  
  
}