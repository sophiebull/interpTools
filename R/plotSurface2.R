#' Plot Performance Surfaces over a Discrete Mesh of Gap Structures
#' 
#' Function to generate surface plots (using the \code{plotly} package) to visualize changes in the performance metrics of interest
#' as gap structure changes. 
#' \itemize{
#' \item The x-axis represents \code{p}, the \strong{proportion of missing data}.\cr 
#' \item The y-axis represents \code{g}, the \strong{gap width}.\cr
#' \item The z-axis represents \code{f(p,g)}, the \strong{value of the performance metric} of interest, according to some statistic.
#' }
#' 
#' @param agObject \code{agObject}; An object containing the aggregated performance metrics (result of \code{agObject()})
#' @param m \code{character}; A vector of interpolation methods of interest
#' @param d \code{character}; A vector to indicate datasets of interest
#' @param toggle \code{character}; How to group the surfaces (either by "dataset" or "method")
#' @param metric \code{character}; An element describing the performance metric of interest
#' @param f \code{character}; The statistic of interest defining the surface \code{f(p,g)}. Possible choices are listed in \code{?aggregate}.
#' @param highlight \code{character/numeric}; A single method (if \code{layer_type = "method"}) or dataset (if \code{layer_type = "dataset"}) to highlight.
#' @param highlight_color \code{character}; An HTML color of format \code{"#xxxxxx"} to apply to \code{highlight}
#' @param colors \code{character}; A vector of the desired color palette, with entries in HTML format (\code{"#xxxxxx"}) 
#' 
#' 

plotSurface2 <- function(agObject,
                         
                         m = names(agObject[[1]][[1]][[1]]),
                         d = names(agObject),
                         
                         toggle = "dataset",
                         
                         metric = "MSE",
                         f = "median",
                         
                         highlight = NULL,
                         highlight_color = "yellow",
                         colors = c("red","blue")){
  
  
  ##########################
  # LOGICAL CHECKS
  ##########################
  
  ## LOGICAL CHECKS ############
  
  if(sum(duplicated(d) != 0)) stop(paste0("'d' contains redundant elements at position(s): ", paste0(c(1:length(d))[duplicated(d)], collapse = ", ") ))
  if(sum(duplicated(m) != 0)) stop(paste0("'m' contains redundant elements at position(s): ", paste0(c(1:length(m))[duplicated(m)], collapse = ", ") ))
  
  if(toggle != "method" & toggle != "dataset") stop("'toggle' must equal either 'method' or 'dataset'.")
  
  if(length(metric) != 1) stop("'metric' must contain only a single character element.")
  if(length(f) != 1) stop("'f' must contain only a single character element.")
  if(length(toggle) != 1) stop("'toggle' must contain only a single character element.")
  if(length(highlight_color) != 1 & !is.null(highlight_color)) stop("'highlight_color' must contain only a single character element.")
  
  if(!all(m %in%  names(agObject[[1]][[1]][[1]]))) stop("Method(s) '", paste0(m[!m %in% names(agObject[[1]][[1]][[1]])], collapse = ", ' "),"' not found. Possible choices are: '", paste0(names(agObject[[1]][[1]][[1]]), collapse = "', '"),"'.")
  if(!all(d %in% names(agObject))) stop("Dataset(s) ", paste0(d[!d %in% names(agObject)], collapse = ", ")," not found. Possible choices are: ", paste(names(agObject), collapse = ','))
  if(!all(f %in% names(agObject[[1]][[1]][[1]][[1]]))) stop(paste0(c("f must be one of: '",paste0(names(agObject[[1]][[1]][[1]][[1]]), collapse = "', '"),"'."), collapse = ""))
  if(!metric %in% rownames(agObject[[1]][[1]][[1]][[1]])) stop(paste0("Metric '",metric,"' must be one of ", paste(rownames(agObject[[1]][[1]][[1]][[1]]),collapse = ", "),"."))
  

  if(class(agObject) != "aggregate") stop("'agObject' object must be of class 'aggregate'. Please use aggregate().")
  
  if(!is.null(highlight)){
    if(length(highlight) != 1) stop("'highlight' must contain only a single character element.")
    if(toggle == "method" & !highlight %in% d) stop(paste0(c("'highlight' must be an element of 'd'. Choose one of: '", paste0(d, collapse = "', '"),"'."), collapse = ""))
    if(toggle == "dataset" & !highlight %in% m) stop(paste0(c("'highlight' must be an element of 'm'. Choose one of: '", paste0(m, collapse = "', '"),"'."), collapse = ""))
  }
  
  if(toggle == "method" & length(m) > 1 & length(colors) == 1) warning(paste0("'colors' should contain at least ", length(m), " elements (each in HTML format: '#xxxxxx') if layering more than one dataset."))
  if(toggle == "dataset" & length(d) > 1 & length(colors) == 1) warning(paste0("'colors' should contain at least ", length(d), " elements (each in HTML format: '#xxxxxx') if layering more than one method."))
  
  
  
  ##########################
  # Retrieving surface vals
  ##########################
  
  z_list <- compileMatrix(agObject = agObject)[[f]]
  
  ##########################
  # Initializing indices
  ##########################
  
  M <- length(m)
  D <- length(d)
  P <- nrow(z_list[[1]][[1]][[1]])
  G <- ncol(z_list[[1]][[1]][[1]])
  
  ##########################
  # Initializing axes
  ##########################
  
  prop_vec <- as.numeric(gsub("p", "", rownames(z_list[[1]][[1]][[1]])))
  gap_vec <- as.numeric(gsub("g", "", colnames(z_list[[1]][[1]][[1]])))
  
  
  axx <- list(
    nticks = length(gap_vec),
    range = c(min(gap_vec),max(gap_vec)),
    title = "g"
  )
  
  axy <- list(
    nticks = length(prop_vec),
    range = c(min(prop_vec),max(prop_vec)),
    title = "p"
  )
  
  axz <- list(title = "f(p,g)",
              nticks = 4)
  
  ##########################
  # Creating color palettes
  ##########################
  
  if(toggle == "method"){
    
    data_names <- d
    
    colorList <- colorRampPalette(colors)(D)
    
    colorListMatch <- colorList[1:D]
    names(colorListMatch) <- data_names
    
    if(!is.null(highlight)){
      colorListMatch[[highlight]] <- highlight_color
    }
    
    colorListMatch <- rep(colorListMatch, each = P*G)
    palette <- lapply(split(colorListMatch, names(colorListMatch)), unname)
    palette <- palette[data_names]
  }
  
  else if(toggle == "dataset"){
    
    method_names <- m
    
    colorList <- colorRampPalette(colors)(M)
    
    colorListMatch <- colorList[1:M]
    names(colorListMatch) <- method_names
    
    if(!is.null(highlight)){
      colorListMatch[[highlight]] <- highlight_color
    }
    
    colorListMatch <- rep(colorListMatch, each = P*G)
    palette <- lapply(split(colorListMatch, names(colorListMatch)), unname) # solid color palettes for each method
    palette <- palette[method_names]
  }
  
  
  ############################
  # Initializing plot list
  ############################
  
  plotList <- list() 
  
  ############################
  # Building plots
  ############################
  
  if(toggle == "dataset"){
    z_d <- list()
    
    for(vd in 1:D){
      
      ###################
      # Surface metadata
      ###################
      
      txt_list <- list()
      z_m <- list()
      
      for(vm in 1:M){
        
        data <- z_list[[metric]][[m[vm]]][[d[vd]]]
        txt <- array(dim = dim(data))
        
        for(p in 1:length(rownames(data))){
          for(g in 1:length(colnames(data))){
            txt[p,g] <- paste0("p = ", prop_vec[p]*100,"%<br />g = ", gap_vec[g], "<br />f = ", round(data[p,g],2))
          }
        }
        
        z_m[[vm]] <- paste("add_surface(x=gap_vec,y=prop_vec,z=z_list[['",metric,"']][['",m[vm],"']][['",d[vd],"']],
                          hovertemplate = t(txt),
                          colorscale = list(seq(0,1,length.out=P*G), palette[[",vm,"]]),
                          name = '", m[vm],"', opacity = 1, visible = F)", sep="")
        
      }
      
      z_m <- paste(z_m, collapse = " %>% \n")
      z_d[[vd]] <- z_m
      
    }
    
    z <- paste(unlist(z_d), collapse = " %>% \n")
  }
  
  else if(toggle == "method"){
    z_m <- list()
    
    for(vm in 1:M){
      
      ###################
      # Surface metadata
      ###################
      
      txt_list <- list()
      z_d <- list()
      
      for(vd in 1:D){
        
        data <- z_list[[metric]][[m[vm]]][[d[vd]]]
        txt <- array(dim = dim(data))
        
        for(p in 1:length(rownames(data))){
          for(g in 1:length(colnames(data))){
            txt[p,g] <- paste0("p = ", prop_vec[p]*100,"%<br />g = ", gap_vec[g], "<br />f = ", round(data[p,g],2))
          }
        }
        
        z_d[[vd]] <- paste("add_surface(x=gap_vec,y=prop_vec,z=z_list[['",metric,"']][['",m[vm],"']][['",d[vd],"']],
                          hovertemplate = t(txt),
                          colorscale = list(seq(0,1,length.out=P*G), palette[[",vd,"]]),
                          name = '",d[vd],"', opacity = 1, visible = F)", sep="")
        
      }
      
      z_d <- paste(z_d, collapse = " %>% \n")
      z_m[[vm]] <- z_d
      
    }
    
    z <- paste(unlist(z_m), collapse = " %>% \n")
  }
  
  
  ##########################
  # Toggle buttons
  ##########################
  
    ##########
    # dataset
    ##########
    
    if(toggle == "dataset"){
      data_vec <- d
      data_logic <- list()
      data_buttons <- list()
      
      for(vd in 1:D){
        data_logic[[vd]] <- grepl(d[vd], data_vec)
      }
      
      data_logic <- lapply(data_logic, FUN = function(x) { rep(x, each = M)}) # turn off and on dataset 
      
      for(vd in 1:D){
        data_buttons[[vd]] <- c("list(
                                  label = '", d[vd],"',
                                  method = 'restyle', 
                                  args = list('visible', list(",paste(data_logic[[vd]], collapse = ','), "))),")
        
        
        data_buttons[[vd]] <- paste(data_buttons[[vd]], collapse = "")                                                  
        
      }
      
      
      data_buttons <- paste(unlist(data_buttons), collapse = "")
      
      buttons <- gsub(".{1}$", "", data_buttons)

    }
  
    ##########
    # method
    ##########
    
    else if(toggle == "method"){
      method_vec <- m
      method_logic <- list()
      method_buttons <- list()
      
      for(vm in 1:M){
        method_logic[[vm]] <- grepl(m[vm], method_vec)
      }
      
      method_logic <- lapply(method_logic, FUN = function(x) { rep(x, each = D)}) # turn off and on dataset 
      
      for(vm in 1:M){
        method_buttons[[vm]] <- c("list(
                                    label = '", m[vm],"',
                                    method = 'restyle', 
                                    args = list('visible', list(",paste(method_logic[[vm]], collapse = ','), "))),")
        
        
        method_buttons[[vm]] <- paste(method_buttons[[vm]], collapse = "")                                                  
        
      }
      
      
      method_buttons <- paste(unlist(method_buttons), collapse = "")
      
      buttons <- gsub(".{1}$", "", method_buttons)
    }
    
  updatemenus <- paste0("list(
                          list(
                              type = 'buttons',
                              active = 0,
                              showactive = TRUE,
                              direction = 'left',
                              yanchor = 'top',
                              xanchor = 'center',
                              x = 0.5,
                            buttons = list(", buttons,"))))")
  
  
  
  #####################
  # Building plots
  #####################
  
  
  p1 <- paste("plot_ly() %>% layout(scene = list(xaxis = axx, yaxis = axy, zaxis = axz)) %>% \n", z, sep="") 
  
  p2 <- paste("%>% \n layout(title = list(text = '", metric , "(",f,")'), updatemenus = ", updatemenus, collapse = '')
  
  
  plot <- eval(parse(text = paste(p1,p2, collapse = "")))
  
  plot<- hide_colorbar(plot) 
  
  message(paste("Please make your ",toggle,"selection on the Viewer."))
  
  return(plot)
}
