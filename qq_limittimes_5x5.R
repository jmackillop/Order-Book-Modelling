# Compare limit interarrival times for 25 different stocks at specified queue with qq plots

par(mfrow=c(5,5))
par(mar=c(2,2,2,2))
for (i in 1:25){
  times <- limit_interarrival_times[[j]][[i]]
  # Take sample to improve render times - without sampling of the order of tens of minutes
  sampledtimes <- sample(times, min(length(times), 1000))
  qqplot(qexp(ppoints(length(sampledtimes)), rate = 1/mean(times)), sampledtimes, xlab = "", ylab = "", main = tickers[i])
  abline(0,1, col="red")
}

# Return graphics parameters to default
par(mfrow=c(1,1))
par(mar= c(5, 4, 4, 2) + 0.1)
