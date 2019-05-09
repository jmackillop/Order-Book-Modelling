# Effects of stock price (bid price at open rounded to nearest integer) on order size and order value

meansizes <- numeric(25)
for (k in 1:25){
  meansizes[k] <- mean(datalevels[[1]][[k]][[1]]$Size)
}
meansizes
meanvalue <- meansizes*prices

par(mfrow=c(1,2))
# Mean order size decreases as price increases
plot(log10(prices), log10(meansizes), pch=16, col="blue", xlab="Log10 (stock price)", ylab = "log10 (mean size)")
abline(lm(log10(meansizes)~log10(prices))$coefficients[1],lm(log10(meansizes)~log10(prices))$coefficients[2], col="red")

# But mean value of order increases as price increases
plot(log10(prices), log10(meanvalue), pch=16, col="blue", xlab="Log10 (stock price)", ylab = "Log10 (mean value)")
abline(lm(log10(meanvalue)~log10(prices))$coefficients[1],lm(log10(meanvalue)~log10(prices))$coefficients[2], col="red")
par(mfrow=c(1,1))