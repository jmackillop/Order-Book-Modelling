# This file finds the proportion of time the stock spends with a spread of 1 tick

# Create vector of indices of where spread is 1 tick
minspread_indices <- which(dataOB_part$ASKp1 - dataOB_part$BIDp1 < 0.015)

minspread_totaltime <- 0
# For each index in the vector in line 4, say k, taking time from index k to k+1 gives the time until the order book transitions
# These are summed in the following for loop
for (j in minspread_indices[-length(minspread_indices)]){
  additional_time <- dataM_part[j+1,1] - dataM_part[j,1]
  minspread_totaltime <- minspread_totaltime + additional_time
}
proportion_spread_one_tick <- minspread_totaltime/ ((endtime-starttime)/1000) #divide by 1000 for s instead of ms