setwd("C:/Users/jamie/Dropbox/Maths Y3/Extended Essay/Code/C&L") #sets working directory
library(graphics)
tickers  <- c("AAPL", "ADBE", "AMGN", "AMZN", "AVGO", "BKNG", "CHTR", "CMCSA", "CME", "COST", "cSCO", "FB", "FOX", "GILD", "GOOG", "INTC", "MSFT", "NFLX", "NVDA", "PEP", "PYPL", "QCOM", "SBUX", "TXN", "WBA")
demodate <- "2018-11-01"
starttime <- 34200000 #9:30am (in milliseconds from midnight) 
endtime <- 57600000 #4pm (in milliseconds from midnight)
nlevels <- 20 #levels of data either side of the book

#-----------------
# Spread of 1 tick
#-----------------
# We determine which stocks have a large proportion of their time with a spread of 1 tick, and discard those which generally trade with spread greater than 1 tick

# Create matrix with 25 rows and 2 columns - each row will contain the ticker name and proportion of time spent with a spread of 1 tick
proportions <- matrix(rep(0, len = 2*length(tickers)), nrow = length(tickers))

for (i in 1:length(tickers)){
  print(c("now working on", tickers[i])) #prints how far the for loop is through the tickers
  # Following loads and preps the data files
  source("C:/Users/jamie/Dropbox/Maths Y3/Extended Essay/Code/C&L/load_and_prep_files.R")
  # Following rebuilds data to include only best queues
  source("C:/Users/jamie/Dropbox/Maths Y3/Extended Essay/Code/Best queue investigation/rebuild_to_level1.R")
  # FOllowing calculates the proportion of time the stock has a spread of 1 tick
  source("C:/Users/jamie/Dropbox/Maths Y3/Extended Essay/Code/C&L/proportion_spread1tick.R")
  cat("proportion for", tickers[i], "is", proportion_spread_one_tick, "\n")
  proportions[i,1] <- tickers[i] #ticker name put into first column 
  proportions[i,2] <- proportion_spread_one_tick #proportion put into second column
}

proportions #displays proportions
# The only stocks of the 25 available having over 50% of the time a spread of 0.01 are the 8 stocks CMCSA, CSCO, FOX, INTC, MSFT, QCOM, SBUX, WBA
high_proportions <- proportions[c(8,11,13,16,17,22,23, 25),] #displays proportions more succintly for these 8

# Reassign tickers variable to only include these 8 stocks
tickers <- c("CMCSA", "CSCO", "FOX", "INTC", "MSFT", "QCOM", "SBUX", "WBA")

#-----------------------------------------
# Estimating sqrt(lambda/D(f)) and volatility from order flows
#-----------------------------------------
# Set sqrts and vols as empty vectors - these will store sqrt(lambda/D(f)) and volatilities
sqrts <- numeric()
vols <- numeric()

# Following for loop runs over each stock ticker
for (i in 1:length(tickers)) {
  print(c("now working on", tickers[i])) #says which ticker is currently being worked on
  # Following loads and preps the data files
  source("C:/Users/jamie/Dropbox/Maths Y3/Extended Essay/Code/C&L/load_and_prep_files.R") 
  # Following rebuilds data to include only best queues
  source("C:/Users/jamie/Dropbox/Maths Y3/Extended Essay/Code/Best queue investigation/rebuild_to_level1.R")
  # source("plot_lob_volume_random_time.R") #uncomment to get a plot of LOB volumes at a random time
  # Following calculates lambda
  source("C:/Users/jamie/Dropbox/Maths Y3/Extended Essay/Code/C&L/calculate_lambda.R")
  # Following calculates D(f)
  source("C:/Users/jamie/Dropbox/Maths Y3/Extended Essay/Code/C&L/calculate_D(f).R")
  # Following calculates the volatility by taking the standard deviation of the price increments
  source("C:/Users/jamie/Dropbox/Maths Y3/Extended Essay/Code/C&L/calculate_volatility_by_price_increment_stddev.R")
}

# Plot scatter plot of sqrt(lambda/D(f)) against volatility, where each point represents one stock
plot(main="Plot of sqrt(lambda/D(f)) against volatility", vols, sqrts, ylab = "estimate of sqrt(lambda/D(f)) from order flow", xlab = "std deviation of 10 minute price increments")
# Plot best fit line on the same plot
abline(lm(sqrts~vols), col = "red")
