# functionPCVeval.R

####  Description: This script defines a function that evaluates Peval for a input training data size.  This function is specific to PHQ9 data that is subsetted and reformatted for the specific use of this, and further functions.

####  Arguments:
# data.in: A way to pass in PHQ9 data
# N.PCV.obs: Number of observations contained in training data sets, passed into CVsplit() function for CV data partitioning.

####  Returns: (Type=list())
# format: list(C1.PCV.out, C2.PCV.out, C3.PCV.out)


####  Libraries and Preliminaries
set.seed(123)
source(file = "/Users/lee/Documents/GitHub/ProbabilisticScoring/Scripts/DataManagement/PHQ9/phq9DataSubsetImport.R")
source(file = "/Users/lee/Documents/GitHub/ProbabilisticScoring/Scripts/DataManagement/PHQ9/functionCVsplit.R")
source(file="/Users/lee/Documents/GitHub/ProbabilisticScoring/Scripts/cvAnalysis/functionPeval.R")



PCVeval=function(dat.in, qNum, qName){
  init.dat.in=dat.in
  init.qNum=qNum
  init.qName=qName
  out.list=list()

  for(i in 1:4){
    out.list[[i]]=Peval(init.dat.in, init.qNum, i-1, init.qName)
  }

  return(out.list)
}


