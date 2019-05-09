# File calculates lambda
# Cont et. al. assumed all orders of unit size, where one unit is taken to be the average size of a limit order
# But we are only interested in calculating limit order arrival rates
# So lambda can be estimated simply as N/T, where N=(total number of limit orders at best queues) and T=(total time)

# Create vector of indices of which events in message book are from limit orders
index_of_los <- which(dataM_part$Type == 1)
# Calculate lambda as N/T
lambda <- length(index_of_los) / ((endtime-starttime)/1000) # /1000 for s instead of ms