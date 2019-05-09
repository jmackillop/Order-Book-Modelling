# Order size exploration
# Histograms, proportions and plots are created to get an intuition for the data

# Plot histogram of order sizes
hist(x, prob=F, breaks = 70000, xlim=c(0,1100), main = paste("Histogram of Order Sizes for",tickers[i], "on 1/11/2018"), xlab = "Order Size")

# Plot histogram with log order sizes
par(mfrow=c(1,2))
hist(log10(x), prob=F, breaks = 50, ylim = c(0, 500000), xlab = "log size", main = "Frequency of log size for limit orders")
hist(log10(x), prob=F, breaks = 50, ylim = c(0, 5000), xlab = "log size", main = "Frequency of log size for limit orders")
par(mfrow=c(1,1))


# Proportion of orders of size exactly 100
length(which(x==100))/length(x) 

# Proportion of orders of size a multiple of 100
length(which(x%%100 == 0))/length(x)

# Proportion of orders of size less than 100 or a multiple of 100
(length(which(x<100))+length(which(x%%100 == 0)))/length(x) 


# Following plots order sizes for size<100
sizesbelow100 <- numeric(99)
for (i in 1:99){
  sizesbelow100[i] <- length(which(x==i))
}
sizesbelow100
plot(1:99, sizesbelow100, xlab = "Order Size", ylab = "Number of orders")

# Following calculates a vector giving the number of orders of size 100, 200, ..., 5000
sizes <- x[which(x%%100==0)]
sizesin100s <- numeric(50) 
for (i in 1:50){
  sizesin100s[i] <- length(which(sizes==100*i))
}
sizesin100s

# Plots probability of orders being of size 100, 200, ..., 5000, and plot a potential best fit line for comparison
plot(100*1:50, sizesin100s/sum(sizesin100s),log = "xy", xlab = "market order size", ylab = "P(x)")
exponent <- 2.5
curve((100^exponent)*x^(-exponent), from = 100, to = 5000, log= "xy", add=TRUE, col="red", lty= 2)
