# functionReformatWeights.R

####  Description: This script defines a function that will take the output created by the PCVeval_overQnum() function and transform the output list into a single data frame for easier computation.

####  Arguments:
# list.in: list argument 9X4X3 dimensional array produced by PCVeval_overQnum()

####  Returns:
# out.list: List of 3 data frames, one list with three 9X4 data frames

# Set Working Directory
WD="/Users/lee/Documents/GitHub/ProbabilisticScoring/Scripts/cvAnalysis"
setwd(WD)

# Data Dependencies:
# PHQ9 Data
source(file = "/Users/lee/Documents/GitHub/ProbabilisticScoring/Scripts/DataManagement/PHQ9/phq9DataSubsetImport.R")

# Variable Dependencies:
set.seed(123)

# File Dependencies

## function: CVsplit(mydat, n.sets) ##
source(file = "/Users/lee/Documents/GitHub/ProbabilisticScoring/Scripts/DataManagement/PHQ9/functionCVsplit.R")

## function: Peval(dat.in, qNum, respNum, qName) ##
source(file="/Users/lee/Documents/GitHub/ProbabilisticScoring/Scripts/cvAnalysis/functionPeval.R")

## function: PCVeval(dat.in, N.PCV.obs)
source(file="/Users/lee/Documents/GitHub/ProbabilisticScoring/Scripts/cvAnalysis/functionPCVeval.R")

## function: PCeval_overQnum(dat.in, Qnum, Qstring) ##
source(file = "/Users/lee/Documents/GitHub/ProbabilisticScoring/Scripts/cvAnalysis/functionPCVeval_overQnum.R")


ReformatWeights=function(list.in){
  init.list.in=list.in
  C1=data.frame()
  C2=data.frame()
  C3=data.frame()

  for(i in 1:9){
    for(j in 1:4){
      C1[i, j]=init.list.in[[i]][[j]][1]
      C2[i, j]=init.list.in[[i]][[j]][2]
      C3[i, j]=init.list.in[[i]][[j]][3]
    }
  }
  colnames(C1)=c("A0","A1","A2","A3")
  colnames(C2)=c("A0","A1","A2","A3")
  colnames(C3)=c("A0","A1","A2","A3")

  out.list=list(C1, C2, C3)
  return(out.list)
}





