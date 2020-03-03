# ImportSubsetVariableList.R

## Imports the subset variable list

WD="/Users/lee/Documents/GitHub/ProbabilisticScoring/Data"
setwd(WD)

SubsetVariableList <- read_excel("SubsetVariableList.xlsx")

save(SubsetVariableList, file = "SubsetVariableList.Rdata")
