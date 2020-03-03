# ImportVariableList.R

## Imports the full variable list

WD="/Users/lee/Documents/GitHub/ProbabilisticScoring/Data"
setwd(WD)

FullVariableList <- read_excel("~/Documents/GitHub/ProbabilisticScoring/Data/FullVariableList.xlsx")

FullVariableList = FullVariableList[4:289, 1:2]

colnames(FullVariableList)=c("OrVarNo", "VarName")

save(FullVariableList, file = "FullVariableList.Rdata")


