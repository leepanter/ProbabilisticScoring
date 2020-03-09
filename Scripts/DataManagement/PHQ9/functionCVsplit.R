# functionCVsplit.R

# Description: This script defines the function CVsplit, which takes as arguments:

####	Arguments
# dat.in: Data set to partition
# N.set: Number of sets (total, including test set) into which to partition data

####  Return (Type=list())
# format: list(dat.train.out, dat.test.out, N.obs)

# out.list <-- Contains the elements below
# dat.train.out: a list of training data sets (in order of selection)
# dat.test.out: a list of testing data sets (in order of selection)
# N.obs: a numerical value indicating the number of observations in a training/test set

####  Call:
#  obj = CVsplit(mydat, N)


####	Libraries and Prelims	 ####
set.seed(123)
source(file = "/Users/lee/Documents/GitHub/ProbabilisticScoring/Scripts/DataManagement/PHQ9/phq9DataSubsetImport.R")



####	Begin Script	 ####
CVsplit=function(dat.in, N.set){

  # Test for N.set >=2
  if(N.set<2){return(0)}
  else{
    N.dat.in.rows=dim(dat.in)[1]
    N.dat.in.cols=dim(dat.in)[2]
    N.obs=floor(N.dat.in.rows/N.set)
    i=1
    index.sample.set=1:N.dat.in.rows
    index.fixed.set=1:N.dat.in.rows

    index.vec=c()
    index.list=list()
    dat.train.out=list()
    dat.test.out=list()

    while(i<=N.set){
      index.list[[i]]=sample(index.sample.set, N.obs, replace = FALSE)
      index.vec=append(index.vec, unique(index.list[[i]]))
      dat.test.out[[i]]=dat.in[index.list[[i]],]
      dat.train.out[[i]]=dat.in[-index.list[[i]],]
      index.sample.set=index.fixed.set[-index.vec[i]]
      i=i+1
    }

    out.list=list(dat.train.out,
                  dat.test.out,
                  N.obs)

  }
}



####	End Script	 ####