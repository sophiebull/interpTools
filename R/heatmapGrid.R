heatmapGrid <- function(agEval, f = "median", crit, m, d, type = "z", output = "graphic", col = c("red","yellow")){
  
  stopifnot((output == "graphic" | output == "matrix"),
            (type == "z" | type == "gradient"),
            f %in% names(agEval[[1]][[1]][[1]][[1]])[1:11],
            length(d) <= length(agEval), 
            length(m) <= length(agEval[[1]][[1]][[1]]),
            length(crit) <=  length(rownames(agEval[[1]][[1]][[1]][[1]])),
            crit %in% rownames(agEval[[1]][[1]][[1]][[1]]))
  
  if(type == "z" && output == "matrix"){
    warning(paste("If type =",type,", output is coerced to 'graphic'. The matrix of z values is available using the function compileMatrix().",sep=""))
    output = "graphic"
  }
  
  P <- length(agEval[[1]])
  G <- length(agEval[[1]][[1]])
  C <- length(crit)
  M <- length(m)
  D <- length(d)
  
  data_list_names <- names(agEval)
  prop_vec_names <- names(agEval[[1]])
  gap_vec_names <- names(agEval[[1]][[1]])
  method_list_names <- names(agEval[[1]][[1]][[1]])
  
  heatmap_matrix <-  lapply(heatmap_matrix <- vector(mode = 'list', C),function(x)
    lapply(heatmap_matrix <- vector(mode = 'list', M),function(x)
      x <- vector(mode = 'list', D)))
  
  heatmap_graphic <- heatmap_matrix
  
  if(type == "z"){
    
    z_list <- compileMatrix(agEval)[[f]]
    
    # Create a list of heatmaps    
    for(s in 1:C){
      for(vm in 1:M){
        for(vd in 1:D){
          
          heatmap_graphic[[s]][[vm]][[vd]] <-  stats::heatmap(z_list[[crit[s]]][[m[vm]]][[d[vd]]], Rowv = NA, Colv = NA, revC = T, scale = "none", 
                                                              labRow = gsub("p","",prop_vec_names, fixed=TRUE),
                                                              labCol = gsub("g","",gap_vec_names, fixed=TRUE),
                                                              xlab = "gap width", ylab = "proportion missing",
                                                              main =  paste("Criterion = ",crit[s],", f = ",f,", type = ", type,sep=""),
                                                              col = colorRampPalette(col)(10000))
        }
        names(heatmap_graphic[[s]][[vm]]) <- data_list_names[d]
      }
      names(heatmap_graphic[[s]]) <- method_list_names[m]
    }
    names(heatmap_graphic) <- crit
    
  }
  
  else if(type == "gradient"){
    
    heatmap_grid_template <- matrix(nrow = 2*(P+1 + (P)*2), ncol = 2*(G+1 +(G)*2))
    
    prop_vec_names <- c("0",prop_vec_names)
    gap_vec_names <- c("0",gap_vec_names)
    
    prop_vec <- c(0,as.numeric(gsub("p","",prop_vec_names, fixed = TRUE)),0)
    gap_vec <- c(0,as.numeric(gsub("g","",gap_vec_names, fixed = TRUE)),0)
    
    # Initialize heatmap grid
    
    rownames_vec <- numeric(nrow(heatmap_grid_template))
    colnames_vec <- numeric(ncol(heatmap_grid_template))
    
    for(i in 1:P+1){
      rownames_vec[i + (i-1)*2] <- prop_vec_names[i]
      for(j in 1:G+1){
        colnames_vec[j + (j-1)*2] <- gap_vec_names[j]
      }  
    }
    
    rownames(heatmap_grid_template) <- rownames_vec
    colnames(heatmap_grid_template) <- colnames_vec
    
    # SCHEMATIC
    
    #  for(i in 1:P){
    
    #    for(j in 1:G){
    #      
    #      heatmap_grid[i+(i-1)*2,j+(j-1)*2] <- paste("(",rownames(heatmap_grid)[i+(i-1)*2],",",
    #                                                 colnames(heatmap_grid)[j+(j-1)*2],")",sep="")
    #      
    #      heatmap_grid[2*i+(i-1),2*j+(j-1)] <- paste("TL, (",i,",",j,"a)",sep="") # top left corners
    #      heatmap_grid[2*i+i,2*j+j] <- paste("BR, (",i,",",j,"a)",sep="") # bottom right corners
    #      
    #      heatmap_grid[2*i+(i-1),2*j+(j)] <- paste("TR, (",i,",",j,"b)",sep="") # top right corners
    #      heatmap_grid[2*i+i,2*j+j-1] <- paste("BL, (",i,",",j,"b)",sep="") # bottom left corners
    #      
    #      heatmap_grid[(i+1)+(i-1)*2,3*j-2] <- paste("VT, (",i,",",j,")",sep="") # vertical tops
    #      heatmap_grid[(i+2)+(i-1)*2,3*j-2] <- paste("VB, (",i,",",j,")",sep="") # vertical bottoms
    #      
    #      heatmap_grid[3*i-2,(j+1)+(j-1)*2] <-  paste("HL, (",i,",",j,")",sep="") # horizontal lefts
    #      heatmap_grid[3*i-2,(j+2)+(j-1)*2] <-  paste("HR, (",i,",",j,")",sep="") # horizontal rights
    #      
    #    }
    #  }
    
    # Function to pull out slope based on each adjacent (p1,g1) (p2,g2) pair
    
    findSlope <- function(theGradientObject, p1,g1,p2,g2){
      logic <- which(apply(theGradientObject[,c("g1","p1","g2","p2")], 1, function(x) all(x == c(g1,p1,g2,p2))))
      if(length(theGradientObject[logic,"slope"]) == 0){
        return(NA)
      }
      else if(length(theGradientObject[logic,"slope"]) !=0 ){
        return(theGradientObject[logic,"slope"])
      }
    }
    
    gradientObject <- gradient(d=d,m=m,crit=crit,agEval=agEval,f=f)
    prop_vec <- prop_vec[-1]
    gap_vec <- gap_vec[-1]
    
    # Create a list of heatmaps
    for(s in 1:C){
      for(vm in 1:M){
        for(vd in 1:D){
          
          theG <- gradientObject[[s]][[vm]][[vd]]
          
          # Populate the matrix of values based on gradient() object
          heatmap_grid <- heatmap_grid_template
          for(i in 1:P+1){
            for(j in 1:G+1){
              
              heatmap_grid[i+(i-1)*2,j+(j-1)*2] <- 0
              
              # top left corners
              heatmap_grid[2*i+(i-1),2*j+(j-1)] <- findSlope(theG,p1=prop_vec[i],g1=gap_vec[j],
                                                             p2=prop_vec[i+1],g2=gap_vec[j+1])
              # bottom right corners
              heatmap_grid[2*i+i,2*j+j] <- findSlope(theG,p1=prop_vec[i+1],g1=gap_vec[j+1],
                                                     p2=prop_vec[i],g2=gap_vec[j])  
              
              # top right corners
              heatmap_grid[2*i+(i-1),2*j+(j)] <- findSlope(theG,p1=prop_vec[i],g1=gap_vec[j+1],
                                                           p2=prop_vec[i+1],g2=gap_vec[j])  
              
              # bottom left corners
              heatmap_grid[2*i+i,2*j+j-1] <- findSlope(theG,p1=prop_vec[i+1],g1=gap_vec[j],
                                                       p2=prop_vec[i],g2=gap_vec[j+1])  
              
              # vertical tops
              heatmap_grid[(i+1)+(i-1)*2,3*j-2] <- findSlope(theG,p1=prop_vec[i],g1=gap_vec[j],
                                                             p2=prop_vec[i+1],g2=gap_vec[j])  
              
              # vertical bottoms
              heatmap_grid[(i+2)+(i-1)*2,3*j-2] <- findSlope(theG,p1=prop_vec[i+1],g1=gap_vec[j],
                                                             p2=prop_vec[i],g2=gap_vec[j])  
              
              # horizontal lefts
              heatmap_grid[3*i-2,(j+1)+(j-1)*2] <- findSlope(theG,p1=prop_vec[i],g1=gap_vec[j],
                                                             p2=prop_vec[i],g2=gap_vec[j+1])  
              
              # horizontal rights
              heatmap_grid[3*i-2,(j+2)+(j-1)*2] <- findSlope(theG,p1=prop_vec[i],g1=gap_vec[j+1],
                                                             p2=prop_vec[i],g2=gap_vec[j])  
              
            }
          }
          
          heatmap_grid <- heatmap_grid[4:((P+1)+2*(P)), 4:((G+1)+2*(G))]
          heatmap_matrix[[s]][[vm]][[vd]] <- heatmap_grid
          
        }
        data_list_names <- names(agEval)
        names(heatmap_matrix[[s]][[vm]]) <- data_list_names[d]
      }
      method_list_names <- names(agEval[[1]][[1]][[1]])      
      names(heatmap_matrix[[s]]) <- method_list_names[m]
    }
    names(heatmap_matrix) <- crit
    
    if(output == "graphic"){
      for(s in 1:C){
        for(vm in 1:M){
          for(vd in 1:D){
            
            heatmap_graphic[[s]][[vm]][[vd]] <-  stats::heatmap(heatmap_matrix[[s]][[vm]][[vd]], Rowv = NA, Colv = NA, revC = T, scale = "none", 
                                                                labRow = rownames(heatmap_matrix[[s]][[vm]][[vd]]),
                                                                labCol = colnames(heatmap_matrix[[s]][[vm]][[vd]]),
                                                                xlab = "gap width", ylab = "proportion missing",
                                                                main =  paste("Criterion = ",crit[s],", f = ",f,", type = ", type,sep=""),
                                                                col = colorRampPalette(col)(10000))
          }
          names(heatmap_graphic[[s]][[vm]]) <- data_list_names[d]
        }
        names(heatmap_graphic[[s]]) <- method_list_names[m]
      }
      names(heatmap_graphic) <- crit
      
    }
  }
  
  if(output == "graphic"){
    return(heatmap_graphic)
  }
  
  else if(output == "matrix"){
    return(heatmap_matrix)
  }
}





