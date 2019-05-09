# Rebuilds

# Need to identify which OB events impact desired queues, so rebuild to only level-1 data
datalevel1 <- vector("list", len)
for (i in 1:len){
  source("rebuild_to_level1.R")
  # Now dataM_part1 and dataOB_part1 are our msg book and order book files for 1 level
  datalevel1[[i]] <- list(dataM_part1, dataOB_part1)
}
names(datalevel1) <- tickers


# Similarly for other levels
datalevel2 <- vector("list", len)
for (i in 1:len){
  source("rebuild_to_level2.R")
  # Now dataM_part2 and dataOB_part2 are our msg book and order book files for level 2
  datalevel2[[i]] <- list(dataM_part2, dataOB_part2)
}
names(datalevel2) <- tickers


datalevel3 <- vector("list", len)
for (i in 1:len){
  source("rebuild_to_level3.R")
  # Now dataM_part3 and dataOB_part3 are our msg book and order book files for level 3
  datalevel3[[i]] <- list(dataM_part3, dataOB_part3)
}
names(datalevel3) <- tickers


datalevel10 <- vector("list", len)
for (i in 1:len){
  source("rebuild_to_level10.R")
  # Now dataM_part10 and dataOB_part10 are our msg book and order book files for level 10
  datalevel10[[i]] <- list(dataM_part10, dataOB_part10)
}
names(datalevel10) <- tickers

datalevels <- list(datalevel1, datalevel2, datalevel3, datalevel10, datalist)