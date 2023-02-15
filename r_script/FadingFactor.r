setwd('/media/lxt/TOSHIBA EXT/moa/')


ff <- function(arr,fadingFactor){
  res <- 0
  first_flag <- T
  list_res <- c()
  for(x in arr){
    if(first_flag){
      first_flag <- F
      res <- x
    }else{
      res <- fadingFactor*x + (1-fadingFactor)*res
    }
    list_res <- append(list_res,res)
  }
  return(list_res)
}

ff2 <- function(arr,fadingFactor){
  res <- 0
  first_flag <- T
  list_res <- c()
  
  for(x in arr){
    if(first_flag){
      first_flag <- F
      S <- x
      B <- 1
    }else{
      S <- x + fadingFactor * S
      B <- 1 + fadingFactor * B
    }
    res <- S/B
    list_res <- append(list_res,res)
  }
  return(list_res)
}



long <- 5000
per1 <- rnorm(long,mean=0.5,sd=0.1)
per2 <- rnorm(long,mean=0.8,sd=0.1)
tail <- rnorm(long,mean=0.9,sd=0.1)
res1 <- ff(arr=c(per1,tail),fadingFactor=0.01)
res2 <- ff(c(per2,tail),fadingFactor=0.01)

res1 <- ff2(arr=c(per1,tail),fadingFactor=0.99)
res2 <- ff2(c(per2,tail),fadingFactor=0.99)


# Create data:
a=c(1:(2*long))
length(res1)
# Make a basic graph
plot( res1~a , type="b" , bty="l" , xlab="instance" , ylab="performance" , col=rgb(0.2,0.4,0.1,0.7) , lwd=1 , pch=17,ylim=c(0,1) )
lines(res2~a , col=rgb(0.8,0.4,0.1,0.7) , lwd=3 , pch=19 , type="b" )

# Add a legend
legend("bottomleft", 
       legend = c("Group 1", "Group 2"), 
       col = c(rgb(0.2,0.4,0.1,0.7), 
               rgb(0.8,0.4,0.1,0.7)), 
       pch = c(17,19), 
       bty = "n", 
       pt.cex = 2, 
       cex = 1.2, 
       text.col = "black", 
       horiz = F , 
       inset = c(0.1, 0.1))

#TODO: multiple models ranking experiment
# which models to choose?




