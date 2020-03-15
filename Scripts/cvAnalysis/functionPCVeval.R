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
source(file = "/Users/lee/Documents/GitHub/ProbabilisticScoring/Scripts/cvAnalysis/functionPCVeval_overQnum.R")




PCVeval=function(dat.in, N.PCV.obs){
  init.dat.in=dat.in
  init.N.PCV.obs=N.PCV.obs
  init.N.set=floor(N.row/in.N.PCV.obs)

  N.row=dim(init.dat.in)[1]

  dat.out=CVsplit(init.dat.in, init.N.set)


}



dat.in=phq9Subset
N.PCV.obs.full=2496
N.PCV.ob=1000

init.dat.in=dat.in
in.N.PCV.obs.full=N.PCV.obs.full
in.N.PCV.obs=N.PCV.obs
# N.row=dim(init.dat.in)[1]
# init.N.set=floor(N.row/in.N.PCV.obs)

dat.out.full=CVsplit(init.dat.in, N.PCV.obs.full)
dat.out=CVsplit(init.dat.in, N.PCV.ob)

dat.out.OverQnum.full=PCeval_overQnum(dat.out.full[[1]], 1, Qstring[1])
dat.out.OverQnum=PCeval_overQnum(dat.out[[1]][[1]], 1, Qstring[1])