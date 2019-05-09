# This file rebuilds data to only include the level-1 data


datadim <- dim(dataOB_part)[1] #number of rows in order book file

# Create vector of differences from one event to next of [prices and volumes of first levels]
differences <- dataOB_part[-datadim,1:4] - dataOB_part[-1,1:4] 
# Create vector of the indices of the changes to the best queues
index_of_changes <- which(differences[,1] != 0 | differences[,2]!=0 |differences[,3] != 0 |differences[,4] != 0) 

# Note k'th entry in MSGBOOK describes event causing k-1'th to k'th entry in ORDERBOOK
# we see that e.g. 1 is in 'index_of_changes', so difference between 1st and 2nd line in ORDERBOOK is from an event in best queues, so 2nd event in MSGBOOK will be for an event in best queues

# We now only take events at these indices
dataM_part <- dataM_part[c(1,index_of_changes+1),] # +1 as msgbook is offset by 1
dataOB_part <- dataOB_part[c(1,index_of_changes+1), 1:4] #this gives original state and the states after changes to best queues

# Reassigns row names so 'k' is at row k
rownames(dataM_part) <- 1: dim(dataM_part)[1]
rownames(dataOB_part) <- 1:dim(dataOB_part)[1] 

# Now dataM_part and dataOB_part are now our msg book and order book files for level-1 data