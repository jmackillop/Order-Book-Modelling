par(mfrow=c(2,3))
for (i in c(8,9,16)){

  x1 <- datalevels[[j]][[i]][[1]]$Size[limit_indices[[j]][[i]]] 
  sizesin100s <- numeric(50) 
  for (k in 1:50){
    sizesin100s[k] <- length(which(x1==100*k))
  }
  sizesin100s
  plot(100*1:50, sizesin100s/sum(sizesin100s),log = "xy", xlab="", ylab="", main=paste(tickers[i], "limit"))
}
for (i in c(8,9,16)){
  
  x <- datalevels[[j]][[i]][[1]]$Size[market_indices[[j]][[i]]] 
  sizesin100s <- numeric(50) 
  for (k in 1:50){
    sizesin100s[k] <- length(which(x==100*k))
  }
  sizesin100s
  plot(100*1:50, sizesin100s/sum(sizesin100s),log = "xy", xlab="", ylab="", main=paste(tickers[i], "market"))
}
   


par(mfrow=c(1,1))
