MAPE <- function(x,X, gappyx){
  index <- which(is.na(gappyx))
  x <- x[index]
  X <- X[index]
  
  100*(1/length(x)*sum(abs((x - X) / x)))
}


# recompute the pmats from scratch

# function to pull out best and worst indices  d p m g 
# function to plost comparison (original - int) 
# plot the series on top of one another (red and black) 

# function to pull out best and worst metrics

# manually compute MAPES
