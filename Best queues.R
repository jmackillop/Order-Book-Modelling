#____________________________________________________
#
# Setup
#____________________________________________________
#
setwd("C:/Users/jamie/Dropbox/Maths Y3/Extended Essay/Code/Best queue investigation")
install.packages("ggplot2")
install.packages("gridExtra")
install.packages("poweRlaw")
library(graphics)
library(ggplot2)
library(gridExtra)
library(poweRlaw)
tickers  <- c("AAPL", "ADBE", "AMGN", "AMZN", "AVGO", #5
              "BKNG", "CHTR", "CMCSA", "CME", "COST", #10
              "CSCO", "FB", "FOX", "GILD", "GOOG",    #15
              "INTC", "MSFT", "NFLX", "NVDA", "PEP",  #20
              "PYPL", "QCOM", "SBUX", "TXN", "WBA")   #25
demodate <- "2018-11-01" #"2012-06-21"
starttime <- 34200000 
endtime <- 57600000
nlevels <- 20

len <- length(tickers)
datalist <- vector("list", len)
for (i in 1:len) {
  source("C:/Users/jamie/Dropbox/Maths Y3/Extended Essay/Code/C&L/load_and_prep_files.R")
  datalist[[i]] <- list(dataM_part, dataOB_part)
}
names(datalist) <- tickers
# Can use 'datalist' to access dataM_part and dataOB_part for any stock without having to load the stocks individually every time
# e.g. datalist$AAPL[[1]] gives AAPL message book , datalist$MSFT[[2]] gives MSFT order book


#___________________________________________
#
# Rebuilding data to only level 1/2/3/10
#___________________________________________

# Rebuild data to include only level j (for j=1,2,3,10,all) in 'datalevelj', 'datalevels' stores these as a list
source("combined_rebuilds.R")

#____________________________________________
#
# Inter-order arrival times are poisson process?
#____________________________________________

# We investigate whether order times are governed by a Poisson process
# Begin by creating lists of lists containing interarrival times for different levels and stocks
list1 <- vector("list", len)

limit_indices <- list(list1, list1, list1, list1, list1) #5 times ie length of datalevels
cancellation_indices <- list(list1, list1, list1, list1, list1)
market_indices <- list(list1, list1, list1, list1, list1)
limit_interarrival_times <- list(list1, list1, list1, list1, list1) 
cancellation_interarrival_times <- list(list1, list1, list1, list1, list1)
market_interarrival_times <- list(list1, list1, list1, list1, list1)

for (j in 1:length(datalevels)){
  for (i in 1:len){
    source("interarrival_times.R")
  }
}
# So e.g. limit_interarrival_times[[2]][[1]] gives limit order interarrival times for 2nd best queue for first ticker which is AAPL

# Plots histograms and qq plots for limit, cancellation, market orders
i <- 17 # choose from 1 to 25, corresponding to the stock in 'tickers'
j <- 4 # choose from 1 to 5, corresponding to: only level 1,only level 2, only level 3, only level 10, all data
source("histqqplots.R")

# Compare the histograms of observed limit order interarrival times to a simulated exponential distribution
source("hist_exp_obs.R")

# Compare 25 different stocks at specified queues (by setting j) with qq plot, for limit, cancellation, and market interarrival times
source("qq_limittimes_5x5.R")
source("qq_cancellationtimes_5x5.R")
source("qq_markettimes_5x5.R")



# Compare a given stock (stock i) across levels 1,2,3,10
source("histtimesdifferentlevels.R")


source("plot_hist_limittimes_10bestq_MSFT_2x2.R")

# To get approximate price level of the different tickers:
prices <- numeric(25)
for(k in 1:25){
  prices[k] <- round(datalevels[[1]][[k]][[2]][3,1], 0)
}
prices
# Consider differences for high and low price stocks  
bigindices <- which(prices>1000)
tickers[bigindices]
smallindices <- which(prices<47)
tickers[smallindices]

for (i in bigindices){
  source("histtimesdifferentlevels.R")
}

for (i in smallindices){
  source("histtimesdifferentlevels.R")
}



#____________________________________________
#
# Order size modelling
#____________________________________________

# Compare high price stocks to low price stocks
par(mfrow=c(2,3))
for (i in c(bigindices, smallindices)){
  x <- datalevels[[j]][[i]][[1]]$Size#[limit_indices[[j]][[i]]] #uncomment for only limit orders (or change 'limit' to 'cancellation' or 'market' for only those orders) - overall distribution shape difference remains same
  hist(x, prob=F, breaks = 1:100000, xlim=c(0,1100), main = paste(tickers[i], "on 1/11/2018"), xlab = "Order Size", bty = "L")
}
par(mfrow=c(1,1))


# Effects of stock price (bid price at open rounded to nearest integer) on order size
source("plot_pricevs_size&value.R")

# Exploration and power law fitting, goodness of fit
source("order_size_modelling pretty.R")
# For [MSFT best queues market orders], rounded down to nearest hundred
round_down <- function(x){
  b <- round(x-49.9,-2)
  return(b)
}
# Round down to nearest hundred and divide by 100 so estimation procedure works correctly
y <- round_down(x)[-which(round_down(x)==0)] /100 #in effect ignore sizes below 100
parameter_estimation_pl(y)



# Scaling parameters for all 25 stocks
# Without data adjustments
  scaling_par_est_raw <- numeric(25)
  pb <- txtProgressBar(min = 1, max = 25, style = 3)
  for (i in 1:25){
    x <- datalevels[[j]][[i]][[1]]$Size[market_indices[[j]][[i]]]
    # Without sample the for loop takes of the order of hours to run
    x1 <- sample(x, min(10000, length(x)))
    scaling_par_est_raw[i] <- parameter_estimation_pl(x1)$pars
    setTxtProgressBar(pb, i)
  }
  close(pb)
  scaling_par_est_raw
# With adjustments: round down to nearest hundred and remove 0
  par_est_rounded <- numeric(25)
  xmin_est_rounded <- numeric(25)
  for (i in 1:25){
    x <- datalevels[[j]][[i]][[1]]$Size[market_indices[[j]][[i]]]
    x2 <- round_down(x)[-which(round_down(x)==0)]/100
    par_est_rounded[i] <- parameter_estimation_pl(x2)$pars
    xmin_est_rounded[i] <- parameter_estimation_pl(x2)$xmin
  }
  par_est_rounded
  xmin_est_rounded
  pvalues
  
  
# Compare loglog linearity of limit to market orders
source("plot_limit_market_compare.R")