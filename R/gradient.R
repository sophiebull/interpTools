#' Approximate the Gradient
#' 
#' A function that will calculate the slope between every combination of points in a grid.
#' 
#' @param matrix A grid (I rows, J columns) of z=f(i,j) values.
#' 

gradient <- function(mat){
  
  I <- nrow(mat)
  J <- ncol(mat)
  
  scheme <- matrix(nrow = I, ncol = J)
  for(i in 1:I){
    for (j in 1:J){
      
      if(i == 1 && j == 1){
        scheme[i,j] = "tlcorner"
      }
      
      else if(i == 1 && j == J){
        scheme[i,j] = "trcorner"
      }
      
      else if(i == I && j == 1){
        scheme[i,j] = "blcorner"
      }
      
      else if(i == I && j == J){
        scheme[i,j] = "brcorner" 
      }
      
      else if((i >=2 && i <= (I-1)) && (j >= 2 && j <= J-1)){
        scheme[i,j] = "interior"
      }
      
      else if((i >=2 && i <= (I-1)) && j == 1){
        scheme[i,j] = "ledge"
      }
      
      else if((j >=2 && j <= (J-1)) && i == 1){
        scheme[i,j] = "tedge"
      }
      
      else if((i >=2 && i <= (I-1)) && j == J){
        scheme[i,j] = "redge"
      }
      
      else if((j >=2 && j <= (J-1)) && i == I){
        scheme[i,j] = "bedge"
      }
    }
  }
  
  # FUNCTION TO CALCULATE 3D SLOPE VECTOR
  slope <- function(x1,x2,y1,y2,z1,z2){
    
    run = sqrt((x2-x1)^2 + (y2-y1)^2)
    rise = y2-y1
    
    return(rise/run)
  }
  
  # FUNCTIONS TO CALCULATE DIRECTIONAL SLOPE
  slope_l <- function(mat,i,j){
    vals <- list()
    
    vals$x1 <- gap_vec[j]
    vals$y1 <- prop_vec[i]
    vals$z1 <- mat[i,j]
    
    vals$x2 <- gap_vec[j-1]
    vals$y2 <- prop_vec[i]
    vals$z2 <- mat[i,j-1]
    
    vals$u <- slope(x1=vals$x1, x2=vals$x2, y1=vals$y1, y2=vals$y2, z1=vals$z1, z2=vals$z2)
    
    return(vals)
  }
  
  slope_ldd <- function(mat,i,j){
    vals <- list()
    
    vals$x1 <- gap_vec[j]
    vals$y1 <- prop_vec[i]
    vals$z1 <- mat[i,j]
    
    vals$x2 <- gap_vec[j-1]
    vals$y2 <- prop_vec[i+1]
    vals$z2 <- mat[i+1,j-1]
    
    vals$u <- slope(x1=vals$x1, x2=vals$x2, y1=vals$y1, y2=vals$y2, z1=vals$z1, z2=vals$z2)
    
    return(vals)
  }
  
  slope_d <- function(mat,i,j){
    vals <- list()
    
    vals$x1 <- gap_vec[j]
    vals$y1 <- prop_vec[i]
    vals$z1 <- mat[i,j]
    
    vals$x2 <- gap_vec[j]
    vals$y2 <- prop_vec[i+1]
    vals$z2 <- mat[i+1,j]
    
    vals$u <- slope(x1=vals$x1, x2=vals$x2, y1=vals$y1, y2=vals$y2, z1=vals$z1, z2=vals$z2)
    
    return(vals)
  }
  
  slope_rdd <- function(mat,i,j){
    vals <- list()
    
    vals$x1 <- gap_vec[j]
    vals$y1 <- prop_vec[i]
    vals$z1 <- mat[i,j]
    
    vals$x2 <- gap_vec[j+1]
    vals$y2 <- prop_vec[i+1]
    vals$z2 <- mat[i+1,j+1]
    
    vals$u <- slope(x1=vals$x1, x2=vals$x2, y1=vals$y1, y2=vals$y2, z1=vals$z1, z2=vals$z2)
    
    return(vals)
  }
  
  slope_r <- function(mat,i,j){
    vals <- list()
    
    vals$x1 <- gap_vec[j]
    vals$y1 <- prop_vec[i]
    vals$z1 <- mat[i,j]
    
    vals$x2 <- gap_vec[j+1]
    vals$y2 <- prop_vec[i]
    vals$z2 <- mat[i,j+1]
    
    vals$u <- slope(x1=vals$x1, x2=vals$x2, y1=vals$y1, y2=vals$y2, z1=vals$z1, z2=vals$z2)
    
    return(vals)
  }
  
  slope_rud <- function(mat,i,j){
    vals <- list()
    
    vals$x1 <- gap_vec[j]
    vals$y1 <- prop_vec[i]
    vals$z1 <- mat[i,j]
    
    vals$x2 <- gap_vec[j+1]
    vals$y2 <- prop_vec[i-1]
    vals$z2 <- mat[i-1,j+1]
    
    vals$u <- slope(x1=vals$x1, x2=vals$x2, y1=vals$y1, y2=vals$y2, z1=vals$z1, z2=vals$z2)
    
    return(vals)
  }
  
  slope_u <- function(mat,i,j){
    vals <- list()
    
    vals$x1 <- gap_vec[j]
    vals$y1 <- prop_vec[i]
    vals$z1 <- mat[i,j]
    
    vals$x2 <- gap_vec[j]
    vals$y2 <- prop_vec[i-1]
    vals$z2 <- mat[i-1,j]
    
    vals$u <- slope(x1=vals$x1, x2=vals$x2, y1=vals$y1, y2=vals$y2, z1=vals$z1, z2=vals$z2)
    
    return(vals)
  }
  
  slope_lud <- function(mat,i,j){
    vals <- list()
    
    vals$x1 <- gap_vec[j]
    vals$y1 <- prop_vec[i]
    vals$z1 <- mat[i,j]
    
    vals$x2 <- gap_vec[j-1]
    vals$y2 <- prop_vec[i-1]
    vals$z2 <- mat[i-1,j-1]
    
    vals$u <- slope(x1=vals$x1, x2=vals$x2, y1=vals$y1, y2=vals$y2, z1=vals$z1, z2=vals$z2)
    
    return(vals)
  }  
  
  # Based on an element's (i,j)th position in the matrix, compute slopes to relevant adjacent points
  
  L <- 12 + 10*((I-2)+(J-2))+8*((I-2)*(J-2))
  slopeMat <- matrix(ncol = 7, nrow = L)
  colnames(slopeMat) <- c("g1","p1","z1","g2","p2","z2","slope")
  
  S_lengths <- c(t(scheme))
  
  S_lengths[grepl("corner",S_lengths)] <- 3
  S_lengths[grepl("edge",S_lengths)] <- 5
  S_lengths[grepl("interior",S_lengths)] <- 8
  
  S_lengths <- as.numeric(S_lengths)
  
  l = 1
  for(i in 1:I){
    for(j in 1:J){
      stopifnot(l<=length(S_lengths))
      
      lower <- sum(S_lengths[0:(l-1)])+1
      upper <- sum(S_lengths[0:l]) 
      
      if(scheme[i,j] == "tlcorner"){
        
        vals1 <- slope_d(mat,i,j)
        vars1 = c(vals1$x1,vals1$y1,vals1$z1,vals1$x2,vals1$y2,vals1$z2)
        
        vals2 <- slope_rdd(mat,i,j)
        vars2 = c(vals2$x1,vals2$y1,vals2$z1,vals2$x2,vals2$y2,vals2$z2)
        
        vals3 <- slope_r(mat,i,j)
        vars3 = c(vals3$x1,vals3$y1,vals3$z1,vals3$x2,vals3$y2,vals3$z2)
        
        slopeMat[lower:upper,1:6] = rbind(vars1,vars2,vars3)
        slopeMat[lower:upper,7] = c(vals1$u,vals2$u,vals3$u)
      }
      
      else if(scheme[i,j] == "tedge"){
        
        vals1 <- slope_l(mat,i,j)
        vars1 <- c(vals1$x1,vals1$y1,vals1$z1,vals1$x2,vals1$y2,vals1$z2)
        
        vals2 <- slope_ldd(mat,i,j)
        vars2 = c(vals2$x1,vals2$y1,vals2$z1,vals2$x2,vals2$y2,vals2$z2)
        
        vals3 <- slope_d(mat,i,j)
        vars3 = c(vals3$x1,vals3$y1,vals3$z1,vals3$x2,vals3$y2,vals3$z2)
        
        vals4 <- slope_rdd(mat,i,j)
        vars4 = c(vals4$x1,vals4$y1,vals4$z1,vals4$x2,vals4$y2,vals4$z2)
        
        vals5 <- slope_r(mat,i,j)
        vars5 = c(vals5$x1,vals5$y1,vals5$z1,vals5$x2,vals5$y2,vals5$z2)
        
        slopeMat[lower:upper,1:6] = rbind(vars1,vars2,vars3,vars4,vars5)
        slopeMat[lower:upper,7] = c(vals1$u,vals2$u,vals3$u,vals4$u,vals5$u)
      }
      
      else if(scheme[i,j] == "trcorner"){
        
        vals1 <- slope_l(mat,i,j)
        vars1 = c(vals1$x1,vals1$y1,vals1$z1,vals1$x2,vals1$y2,vals1$z2)
        
        vals2 <- slope_ldd(mat,i,j)
        vars2 = c(vals2$x1,vals2$y1,vals2$z1,vals2$x2,vals2$y2,vals2$z2)
        
        vals3 <- slope_d(mat,i,j)
        vars3 = c(vals3$x1,vals3$y1,vals3$z1,vals3$x2,vals3$y2,vals3$z2)
        
        slopeMat[lower:upper,1:6] = rbind(vars1,vars2,vars3)
        slopeMat[lower:upper,7] = c(vals1$u,vals2$u,vals3$u)
        
      }
      
      else if(scheme[i,j] == "ledge"){
        
        vals1 <- slope_d(mat,i,j)
        vars1 = c(vals1$x1,vals1$y1,vals1$z1,vals1$x2,vals1$y2,vals1$z2)
        
        vals2 <- slope_rdd(mat,i,j)
        vars2 = c(vals2$x1,vals2$y1,vals2$z1,vals2$x2,vals2$y2,vals2$z2)
        
        vals3 <- slope_r(mat,i,j)
        vars3 = c(vals3$x1,vals3$y1,vals3$z1,vals3$x2,vals3$y2,vals3$z2)
        
        vals4 <- slope_rud(mat,i,j)
        vars4 = c(vals4$x1,vals4$y1,vals4$z1,vals4$x2,vals4$y2,vals4$z2)
        
        vals5 <- slope_u(mat,i,j)
        vars5 = c(vals5$x1,vals5$y1,vals5$z1,vals5$x2,vals5$y2,vals5$z2)
        
        slopeMat[lower:upper,1:6] = rbind(vars1,vars2,vars3,vars4,vars5)
        slopeMat[lower:upper,7] = c(vals1$u,vals2$u,vals3$u,vals4$u,vals5$u)
        
      }
      
      else if(scheme[i,j] == "interior"){
        
        vals1 <- slope_l(mat,i,j)
        vars1 = c(vals1$x1,vals1$y1,vals1$z1,vals1$x2,vals1$y2,vals1$z2)
        
        vals2 <- slope_ldd(mat,i,j)
        vars2 = c(vals2$x1,vals2$y1,vals2$z1,vals2$x2,vals2$y2,vals2$z2)
        
        vals3 <- slope_d(mat,i,j)
        vars3  = c(vals3$x1,vals3$y1,vals3$z1,vals3$x2,vals3$y2,vals3$z2)
        
        vals4 <- slope_rdd(mat,i,j)
        vars4  = c(vals4$x1,vals4$y1,vals4$z1,vals4$x2,vals4$y2,vals4$z2)
        
        vals5 <- slope_r(mat,i,j)
        vars5  = c(vals5$x1,vals5$y1,vals5$z1,vals5$x2,vals5$y2,vals5$z2)
        
        vals6 <- slope_rud(mat,i,j)
        vars6  = c(vals6$x1,vals6$y1,vals6$z1,vals6$x2,vals6$y2,vals6$z2)
        
        vals7 <- slope_u(mat,i,j)
        vars7  = c(vals7$x1,vals7$y1,vals7$z1,vals7$x2,vals7$y2,vals7$z2)
        
        vals8 <- slope_lud(mat,i,j)
        vars8  = c(vals8$x1,vals8$y1,vals8$z1,vals8$x2,vals8$y2,vals8$z2)
        
        slopeMat[lower:upper,1:6] = rbind(vars1,vars2,vars3,vars4,vars5,vars6,vars7,vars8)
        slopeMat[lower:upper,7] = c(vals1$u,vals2$u,vals3$u,vals4$u,vals5$u,vals6$u,vals7$u,vals8$u)
        
      }
      
      else if(scheme[i,j] == "redge"){
        
        vals1 <- slope_l(mat,i,j)
        vars1  = c(vals1$x1,vals1$y1,vals1$z1,vals1$x2,vals1$y2,vals1$z2)
        
        vals2 <- slope_ldd(mat,i,j)
        vars2  = c(vals2$x1,vals2$y1,vals2$z1,vals2$x2,vals2$y2,vals2$z2)
        
        vals3 <- slope_d(mat,i,j)
        vars3  = c(vals3$x1,vals3$y1,vals3$z1,vals3$x2,vals3$y2,vals3$z2)
        
        vals4 <- slope_u(mat,i,j)
        vars4  = c(vals4$x1,vals4$y1,vals4$z1,vals4$x2,vals4$y2,vals4$z2)
        
        vals5 <- slope_lud(mat,i,j)
        vars5  = c(vals5$x1,vals5$y1,vals5$z1,vals5$x2,vals5$y2,vals5$z2)
        
        slopeMat[lower:upper,1:6] = rbind(vars1,vars2,vars3,vars4,vars5)
        slopeMat[lower:upper,7] = c(vals1$u,vals2$u,vals3$u,vals4$u,vals5$u)
        
      } 
      
      else if(scheme[i,j] == "blcorner"){
        
        vals1 <- slope_r(mat,i,j)
        vars1 = c(vals1$x1,vals1$y1,vals1$z1,vals1$x2,vals1$y2,vals1$z2)
        
        vals2 <- slope_rud(mat,i,j)
        vars2 = c(vals2$x1,vals2$y1,vals2$z1,vals2$x2,vals2$y2,vals2$z2)
        
        vals3 <- slope_u(mat,i,j)
        vars3 = c(vals3$x1,vals3$y1,vals3$z1,vals3$x2,vals3$y2,vals3$z2)
        
        slopeMat[lower:upper,1:6] = rbind(vars1,vars2,vars3)
        slopeMat[lower:upper,7] = c(vals1$u,vals2$u,vals3$u)
        
      }
      
      else if(scheme[i,j] == "bedge"){
        
        vals1 <- slope_l(mat,i,j)
        vars1 = c(vals1$x1,vals1$y1,vals1$z1,vals1$x2,vals1$y2,vals1$z2)
        
        vals2 <- slope_r(mat,i,j)
        vars2 = c(vals2$x1,vals2$y1,vals2$z1,vals2$x2,vals2$y2,vals2$z2)
        
        vals3 <- slope_rud(mat,i,j)
        vars3 = c(vals3$x1,vals3$y1,vals3$z1,vals3$x2,vals3$y2,vals3$z2)
        
        vals4 <- slope_u(mat,i,j)
        vars4 = c(vals4$x1,vals4$y1,vals4$z1,vals4$x2,vals4$y2,vals4$z2)
        
        vals5 <- slope_lud(mat,i,j)
        vars5 = c(vals5$x1,vals5$y1,vals5$z1,vals5$x2,vals5$y2,vals5$z2)
        
        slopeMat[lower:upper,1:6] = rbind(vars1,vars2,vars3,vars4,vars5)
        slopeMat[lower:upper,7] = c(vals1$u,vals2$u,vals3$u,vals4$u,vals5$u)
        
      }
      
      else if(scheme[i,j] == "brcorner"){
        
        vals1 <- slope_l(mat,i,j)
        vars1 = c(vals1$x1,vals1$y1,vals1$z1,vals1$x2,vals1$y2,vals1$z2)
        
        vals2 <- slope_u(mat,i,j)
        vars2 = c(vals2$x1,vals2$y1,vals2$z1,vals2$x2,vals2$y2,vals2$z2)
        
        vals3 <- slope_lud(mat,i,j)
        vars3 = c(vals3$x1,vals3$y1,vals3$z1,vals3$x2,vals3$y2,vals3$z2)
        
        slopeMat[lower:upper,1:6] = rbind(vars1,vars2,vars3)
        slopeMat[lower:upper,7] = c(vals1$u,vals2$u,vals3$u)
      }
      
      l <- l+1
      
    }
  }
  
}

