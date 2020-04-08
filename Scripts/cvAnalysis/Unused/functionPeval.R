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
source(file = "/Users/lee/Documents/GitHub/ProbabilisticScoring/Scripts/DataManagement/PHQ9/phq9DataSubsetImport.R")
source(file = "/Users/lee/Documents/GitHub/ProbabilisticScoring/Scripts/DataManagement/PHQ9/functionCVsplit.R")


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

####	End Script	 ####









