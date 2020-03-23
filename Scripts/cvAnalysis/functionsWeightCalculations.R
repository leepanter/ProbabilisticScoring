################################################################################
## functionsWeightCalculations.R
################################################################################


# functionPeval.R
####  Description:  This script calculated the probabilities of being classified into one of three different outcome categories based upon training data.

####  Arguments:
# dat.in: training data file on which to perform calculations
# qNum: question number of inquiry
# respNum: given response of inquiry
# qName: Column String name for response of interest Q1, Q2, Q3...


####  Retuns:
# a three-item vector of probabilites corresponding to the chances of eventually being characterized within a certain classification outcome C1, C2, C3

####	Libraries and Prelims	 ####
set.seed(123)
source(file = "/Users/lee/Documents/GitHub/ProbabilisticScoring/Scripts/DataManagement/PHQ9/functionsDataImportProcessing.R")

####	Begin Script	 ####

Peval=function(dat.in, qNum, respNum, qName){

  low.thresh=7
  high.thresh=10

  # Figure out which columns correspond to answers to qNum
  index.qNum=which(colnames(dat.in)==qName)
  qDat=dat.in[,index.qNum]

  ### Calculate Prior Proababilities

  # Calculate P(E_j) for j=0,1,2,3
  PErespNum=length(which(qDat==respNum))/2495

  # Indices in dat.in classified as C1
  PC1=length(which(dat.in$qTot<low.thresh & qDat==respNum))

  # Calculate Number of people in C1
  C1.total=length(which(dat.in$qTot<low.thresh))

  PrPc1=PC1/C1.total


  # Indices in dat.in classified as C2
  PC2=length(which(low.thresh<=dat.in$qTot & dat.in$qTot<high.thresh & qDat==respNum))

  # Calculate Number of people in C2
  C2.total=length(which(low.thresh<=dat.in$qTot & dat.in$qTot<high.thresh))

  PrPc2=PC2/C2.total


  # Indices in dat.in classified as C3
  PC3=length(which(dat.in$qTot >= high.thresh & qDat==respNum))

  # Calculate Number of people in C3
  C3.total=length(which(dat.in$qTot >= high.thresh ))

  PrPc3=PC3/C3.total


  ret1=PrPc1/PErespNum
  ret2=PrPc2/PErespNum
  ret3=PrPc3/PErespNum
  out=c(ret1, ret2, ret3)
  return(out)
}


# functionPCVeval.R

####  Description: This script defines a function that evaluates Peval for a input training data size.  This function is specific to PHQ9 data that is subsetted and reformatted for the specific use of this, and further functions.

####  Arguments:
# data.in: A way to pass in PHQ9 data
# N.PCV.obs: Number of observations contained in training data sets, passed into CVsplit() function for CV data partitioning.

####  Returns: (Type=list())
# format: list(C1.PCV.out, C2.PCV.out, C3.PCV.out)

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


# functionPCVeval_overQnum.R

####  Description: This script defines a function that evaluates PCVeval over an input data set over all questions

####  Arguments:
# data.in: A way to pass in PHQ9 data

####  Returns: (Type=list())
# a list of length rowdim(dat.in)
#   each list is an evaluation of Peval corresponding to answers {0,1,2,3} for the provided input arguments

PCVeval_overQnum=function(dat.in){
  init.dat.in=dat.in
  out.q=list()

  for(i in 1:9){
    out.q[[i]]=PCVeval(init.dat.in, i, Qstring[i])
  }
  return(out.q)
}



# functionReformatWeights.R

####  Description: This script defines a function that will take the output created by the PCVeval_overQnum() function and transform the output list into a single data frame for easier computation.

####  Arguments:
# list.in: list argument 9X4X3 dimensional array produced by PCVeval_overQnum()

####  Returns:
# out.list: List of 3 data frames, one list with three 9X4 data frames

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


####	End Script	 ####



