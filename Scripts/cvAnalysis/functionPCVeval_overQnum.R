# functionPCVeval_overQnum.R

####  Description: This script defines a function that evaluates Peval over an input data set over a defined question number

####  Arguments:
# data.in: A way to pass in PHQ9 data
# Qnum: numeric argument 1,2,3,4,5...9
# Qstring: one of Q1, Q2,...,Q9 as strings

####  Returns: (Type=list())
# a list of length rowdim(dat.in)
#   each list is an evaluation of Peval corresponding to answers {0,1,2,3} for the provided input arguments

####  Libraries and Preliminaries
set.seed(123)
source(file = "/Users/lee/Documents/GitHub/ProbabilisticScoring/Scripts/DataManagement/PHQ9/phq9DataSubsetImport.R")
source(file = "/Users/lee/Documents/GitHub/ProbabilisticScoring/Scripts/DataManagement/PHQ9/functionCVsplit.R")
source(file="/Users/lee/Documents/GitHub/ProbabilisticScoring/Scripts/cvAnalysis/functionPeval.R")



PCeval_overQnum=function(dat.in, Qnum, Qstring){
  init.dat.in=dat.in
  init.Qnum=Qnum
  init.Qstring=Qstring
  subject.length=dim(init.dat.in)[1]

  out.Sub_i=list()
  for(i in 1:subject.length){
    out.Resp_k=list()
    for(k in 1:4){
      out.Resp_k[[k]]=Peval(init.dat.in, init.Qnum, k-1, init.Qstring)
    }
    out.Sub_i[[i]]=list(out.Resp_k[[1]],
                        out.Resp_k[[2]],
                        out.Resp_k[[3]],
                        out.Resp_k[[4]])
  }
  out=out.Sub_i
  return(out)
}