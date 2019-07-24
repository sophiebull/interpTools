#### simWt.R ####
# Function to simulate W_t: the noise component of X_t
# class(p) == numeric; The AR order
# class(q) == numeric; The MA order
# class(n) == numeric; The length of the noise series
simWt <- function(n=1000,p=0,q=0){
  stopifnot(p>=0,q>=0,n>=0,n%%1==0)
  
  if(p > 0){
    repeat{
      ar<-numeric(p)
      for(i in 1:p){
        ar[i] <- c(sample(seq(-p,p,length.out=1000),1))
      }
      minroots <- min(Mod(polyroot(c(1, -ar))))
      if(minroots > 1){
        break
      }
    }
    
    if(q>0){
      ma <- c(sample(seq(0,1,length.out=1000),q))
      model <- list(order = c(p,0,q), ar = ar, ma = ma)
    }
    
    else if(q == 0){
      model <- list(order = c(p,0,q), ar = ar)
    }
  }
  
  else if(p == 0){
    if(q==0){
      model <- list(order = c(p,0,q))
    }
    
    else if(q > 0){
      ma <- c(sample(seq(0,1,length.out=1000),q))
      model <- list(order = c(p,0,q), ma = ma)
    }
  }
  
  Wt = arima.sim(model, n = n)
  return(Wt)
}
