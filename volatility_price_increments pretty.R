# This file calculates an estimation of volatility by using the standard deviation of the 10-minute bid-price logarithmic returns

# We calculate the bid-price logarithmic returns between t_n and t_(n+1) where t_(n+1)-t_n = 10 minutes, for all 10 minute intervals in the continuous trading period
# r(t_1,t_2) := log(b(t_1)/b(t_2)) is definiion of logarithmic return, for bid price b(t)
# we then calculate volatility as the standard deviation of the bid-price logarithmic return series

# we first find the indices of the ten minute intervals in the message book
tenminutes <- seq(from = 34200, to =57600, by=600) 
tenminutes[40] <- 57599.9 #we want to include final entry, so not 57600
tmindices <- numeric() # this will be the indices

# Following for loop finds finds indices of ten minute times
u <- dataM_part$Time
for (i in tenminutes){
  tmindices <- c(tmindices, which(u>i)[1])
}

logreturns <- numeric(39) # note length(tmindices)=40, so want length(logreturns)=39
# Following for loop calculates the log returns
for (i in 1:39){
  logreturns[i] <- log(dataOB_part$BIDp1[tmindices[i]] / dataOB_part$BIDp1[tmindices[i+1]])
}
# Realised volatility is the standard deviation of the log returns so
tm_realised_volatility <- sd(logreturns)

# Pass the calculated realised volatility to global variable vols
vols <- c(vols,tm_realised_volatility)
