# This file loads and prepares the message and order book files from the ~Code/Data file
# Based on a modified version of the file loading and preparation sections of the LOBSTER demo file, available through their website at lobsterdata.com

# We modify then return the working directory to original
originalwd <- getwd() 
setwd("C:/Users/jamie/Dropbox/Maths Y3/Extended Essay/Data")

# Name of Orderbook & Messagebook
ORDERBOOK <- paste(paste(tickers[i] , demodate ,starttime,endtime,"orderbook" ,nlevels ,sep = "_"),"csv",sep = ".")
MSGBOOK <- paste(paste(tickers[i] , demodate ,starttime,endtime,"message" ,nlevels ,sep = "_"),"csv",sep = ".")

#-------------------------------
# Data Preparation - Message File
# Remove observations outside trading hours
#-------------------------------
         
# Load data
dataM <- read.csv ( MSGBOOK )
# Remove unwanted 7th column
dataM <- dataM[,-7]
# Name the columns 
columns <- c ( "Time" , "Type" , "OrderID" , "Size" , "Price" , "TradeDirection" )
colnames(dataM) <- columns

# Trading hours (start & end)
startTrad   = 9.5*60*60;       # 9:30 in seconds after midnight
endTrad     = 16*60*60;        # 16:00 
# Create data frame with messages from the continuous trading period
dataM_part = dataM[dataM$Time>=startTrad & dataM$Time<=endTrad,]

# Check for trading halts
# When trading halts, there will be type=7 in message file, price & direction = -1, all other properties = 0
# When quoting resumes, type = 7, price changed to 0
# When trading resumes, type = 7, price to 1
tradehaltIdx = which(dataM[,2] == 7 & dataM[,5] == -1 )
tradequoteIdx = which(dataM[,2] == 7 & dataM[,5] == 0 )
traderesumeIdx = which(dataM[,2] == 7 & dataM[,5] == 1 )

# In the case of no trading halts
if(length(tradehaltIdx)==0 & length(tradequoteIdx)==0  & length(traderesumeIdx)==0 )
  print("No trading halts detected.")
# In the case of trading halts
if(length(tradehaltIdx) !=0)
  cat("Data contains trading halt! at time stamp(s)", dataM[tradehaltIdx,1],"\n" ) #\n creates new line
if(length(tradequoteIdx) !=0)
  cat(" Data contains quoting message! at time stamp(s)", dataM[tradequoteIdx,1], "\n")
if(length(traderesumeIdx) !=0)
  cat(" Data resumes trading! at time stamp(s) ", dataM[traderesumeIdx,1],"\n")

#------------------------------------
# Data Preparation - Order Book File
#------------------------------------

# Load data
dataOB <- read.csv(ORDERBOOK)   
# Naming data frame columns
columns2 <- c("ASKp1","ASKs1","BIDp1","BIDs1")
# Naming data frame columns, only used for levels>1
if (nlevels > 1) {
  for (j in 2:nlevels) {
    columns2 <- c (columns2,paste("ASKp",j,sep=""), paste("ASKs",j,sep=""),paste("BIDp",j,sep=""),paste("BIDs",j,sep="")) 
  }
}
colnames(dataOB) <- columns2

# Create a vector of logical values where TRUE=in trading period
timeindex <-dataM$Time>=startTrad & dataM$Time<=endTrad
# Take dataOB entries only in the trading period
dataOB_part = dataOB[timeindex,]

# Convert prices into dollars, LOBSTER stores prices in dollar price times 10000
for (j in c(seq(from = 1, length=2*nlevels, by = 2))){ 
  dataOB_part[,j ] = dataOB_part[ ,j]/10000 
}

print("load and prep of files is complete")

setwd(originalwd) # return wd to original wd