# Calculates interarrival times for limit, cancellation and market orders for stock i at specified level j

# FOllowing gives indices of limit, cancellation, and market orders
limit_indices[[j]][[i]] <- which(datalevels[[j]][[i]][[1]]$Type == 1) #indices of limit orders (l.o.s) in message book
cancellation_indices[[j]][[i]] <- which(datalevels[[j]][[i]][[1]]$Type == 2 | datalevels[[j]][[i]][[1]]$Type ==3) #partial and total deletions of l.o.s
market_indices[[j]][[i]] <- which(datalevels[[j]][[i]][[1]]$Type == 4 | datalevels[[j]][[i]][[1]]$Type ==5) #execution of visible and hidden l.o.s

# Following finds times(since midnight) which correspond to the above indices
limit_times <- datalevels[[j]][[i]][[1]]$Time[limit_indices[[j]][[i]]]
cancellation_times <- datalevels[[j]][[i]][[1]]$Time[cancellation_indices[[j]][[i]]]
market_times <- datalevels[[j]][[i]][[1]]$Time[market_indices[[j]][[i]]]

# Following are limit, cancellation, and market order times shifted by one index place
limit_shifted_by_1_times <- datalevels[[j]][[i]][[1]]$Time[c(1,limit_indices[[j]][[i]][-length(limit_indices[[j]][[i]])])] 
cancellation_shifted_by_1_times <- datalevels[[j]][[i]][[1]]$Time[c(1,cancellation_indices[[j]][[i]][-length(cancellation_indices[[j]][[i]])])]
market_shifted_by_1_times <- datalevels[[j]][[i]][[1]]$Time[c(1,market_indices[[j]][[i]][-length(market_indices[[j]][[i]])])]

# Creates vectors of interarrival times for limit, cancellation, and market orders
limit_interarrival_times[[j]][[i]] <- limit_times - limit_shifted_by_1_times
cancellation_interarrival_times[[j]][[i]] <- cancellation_times - cancellation_shifted_by_1_times
market_interarrival_times[[j]][[i]] <- market_times - market_shifted_by_1_times