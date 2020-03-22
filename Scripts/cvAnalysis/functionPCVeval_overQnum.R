# functionPCVeval_overQnum.R

####  Description: This script defines a function that evaluates PCVeval over an input data set over all questions

####  Arguments:
# data.in: A way to pass in PHQ9 data

####  Returns: (Type=list())
# a list of length rowdim(dat.in)
#   each list is an evaluation of Peval corresponding to answers {0,1,2,3} for the provided input arguments

####  Libraries and Preliminaries
set.seed(123)
source(file = "/Users/lee/Documents/GitHub/ProbabilisticScoring/Scripts/DataManagement/PHQ9/phq9DataSubsetImport.R")
source(file = "/Users/lee/Documents/GitHub/ProbabilisticScoring/Scripts/DataManagement/PHQ9/functionCVsplit.R")
source(file="/Users/lee/Documents/GitHub/ProbabilisticScoring/Scripts/cvAnalysis/functionPeval.R")
source(file="/Users/lee/Documents/GitHub/ProbabilisticScoring/Scripts/cvAnalysis/functionPCVeval.R")



PCVeval_overQnum=function(dat.in){
  init.dat.in=dat.in
  out.q=list()

  for(i in 1:9){
    out.q[[i]]=PCVeval(init.dat.in, i, Qstring[i])
  }
  return(out.q)
}