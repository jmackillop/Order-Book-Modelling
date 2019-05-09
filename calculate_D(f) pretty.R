# This file calculates D(f)

# In the first half of this file, the data is reworked to include only entries where the spread has just closed to 1 tick following a price increase. This is achieved by creating new data frames for dataOB and dataM with entries satisfying the conditions
# In the second half of this file, D(f) is estimated from the data by considering teh volumes at the entries previously calculated

# Recall D(f) = sum_i sum_j [ i*j*f(i,j)].....this is a measure of market depth
# f(x,y) represents the probability of observing (q_b t,q_a t ) = (x,y) right after a price increase. 

#---------------------------
# Reworking data
# --------------------------
# Aim to create new dataOB and dataM with entries such that only spread=0.01 & price increase from last entry.

# First remove entries from dataOB and dataM with spread > 0.01
spreads <- round(dataOB_part$ASKp1 - dataOB_part$BIDp1 , 5) #round to 5dp to remove errors of order E-15
index_of_unitspreads <- (spreads == 0.01) 
# remove entries NOT at these indices because they have wider spreads
dataOB_minspread <- dataOB_part[index_of_unitspreads,]
dataM_minspread <- dataM_part[index_of_unitspreads,]

# Aim for only entries immediately after price increases
askprices <- dataOB_minspread$ASKp1[-(dim(dataOB_minspread)[1])]
shiftedaskprices <- dataOB_minspread$ASKp1[-1]
price_changes <-  shiftedaskprices - askprices # this gives the change in the ask price between entries in dataOB_minspread. Positive numbers indicate an increase in price
# Now need to take only entries of dataOB and dataM just after price increase. Note need a +1 as want entry _after_ price increase
dataOB_afterincreases <- dataOB_minspread[1 + which(price_changes > 0),] 
dataM_afterincreases <- dataM_minspread[1 + which(price_changes > 0),]

# Record best bid and ask queue volumes from the above data
times <- rownames(dataOB_afterincreases)
A <- matrix(c(dataOB_afterincreases$BIDs1, dataOB_afterincreases$ASKs1), nrow=(dim(dataOB_afterincreases)[1]), ncol=2, byrow = F)
dimnames(A) <- list(times,c("bid size","ask size"))
# 'A' is thus a matrix with several thousand rows and 3 columns. First col gives position in dataOB_part, second col gives bid volume, third col gives ask volume. (For just after price increase and the spread closing to 0.01.)


#-------------------------
# Fitting pdf to data
#-------------------------
# we want to find the joint pdf that best fits the data in the matrix A
# for data overview, plot(A[,1],A[,2], xlab= "bid sizes", ylab = "ask sizes")
# we can estimate pdf by counting number of observations within a given distance of each point
# we do this count at each point in a grid up to the largest observations, could then extrapolate linearly between these points. 
# (For computational speed reasons we dont extrapolate)

# Create a function which rounds up numbers to the nearest hundred
round_up <- function(x){
  round(x+50,-2)
}

# Create two vectors which will act as the grid
xgrid <- seq(from = 100, to = round_up(max(A[,1])), by = 100) #these are the bid sizes
ygrid <- seq(from = 100, to = round_up(max(A[,2])), by = 100) #ask sizes
B <- matrix(0, nrow = length(xgrid), ncol = length(ygrid))
# The following for loop runs over each point in the grid
for (x in xgrid){
  for (y in ygrid){
    # Count the number of points in a 'box' of x+-250, y+-250
    u <- ((abs(A[,1]-x)<250) & (abs(A[,2]-y)))
    density <- length(which(u)) #number of points in the 'box'
    B[x/100,y/100] <- density #assigns densities to matrix
  }
}

B <- B/sum(B) # rescales so elements sum to 1, so is a pmf
# B[i,j] is density for (bid size)=100i and (ask size)=100j

# we calculate a simplified D(f) using only the points from the grid
# a full calculation would require approximately 100*100=10,000 more calculations
# note also simplified D(f) considers only 1 in 10,000 points, but each has value approx 10,000x greater than true D(f), so simplified D(f) is approx same as D(f)

time <- proc.time() # start time
Df_simp <- 0 # will be simplified D(f)
for (i in xgrid){
  for (j in ygrid){
    Df_simp <- Df_simp + i*j*B[i/100,j/100]
  }
}
proc.time()-time 
# end_time-start_time gives elapsed time of a couple seconds, so full calculation isn't feasible

# The following passes the values of sqrt(lambda/D(f)) to the global variable sqrts
ratio_sqrt <- sqrt(lambda/Df_simp)
sqrts <- c(sqrts, ratio_sqrt)