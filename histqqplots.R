# Plots histograms and qq-plots of limit, cancellation, & market order times

par(mfcol=c(2,3))

# Limit interarrival times
hist(limit_interarrival_times[[j]][[i]], breaks = 100, xlim = c(0,0.25), main = "Histogram for Limit Order Interarrival Times", xlab = "Limit interarrival times (s)")

qqplot(qexp(ppoints(length(limit_interarrival_times[[j]][[i]])), rate = 1/mean(limit_interarrival_times[[j]][[i]])), limit_interarrival_times[[j]][[i]], main="Q-Q plot for Limit Order Interarrival Times", xlab = "Theoretical Quantiles", ylab = "Observed Quantiles")

abline(0,1)

# Cancellation interarrival times
hist(cancellation_interarrival_times[[j]][[i]], breaks = 200, xlim = c(0,0.25), main = "Histogram for Cancellation Interarrival Times", xlab = "Cancellation interarrival times (s)")

qqplot(qexp(ppoints(length(cancellation_interarrival_times[[j]][[i]])), rate = 1/mean(cancellation_interarrival_times[[j]][[i]])), cancellation_interarrival_times[[j]][[i]], main="Q-Q plot for Cancellation Interarrival Times", xlab = "Theoretical Quantiles", ylab = "Observed Quantiles")

abline(0,1)

# Market interarrival times
hist(market_interarrival_times[[j]][[i]], breaks = 400, xlim = c(0,.25), main = "Histogram for Market Order Interarrival Times", xlab = "Market interarrival times (s)")

qqplot(qexp(ppoints(length(market_interarrival_times[[j]][[i]])), rate = 1/mean(market_interarrival_times[[j]][[i]])), market_interarrival_times[[j]][[i]], main="Q-Q plot for Market Order Interarrival Times", xlab = "Theoretical Quantiles", ylab = "Observed Quantiles")

abline(0,1)


par(mfcol=c(1,1))