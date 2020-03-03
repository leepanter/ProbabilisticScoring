# ImportAllData.R

# Imports all data from original data set

WD="/Users/lee/Documents/GitHub/ProbabilisticScoring/Data"
setwd(WD)

FullData=read.csv(file = "/Users/lee/Documents/GitHub/ProbabilisticScoring/Data/FullData.csv")

save(FullData, file = "FullData.Rdata")
