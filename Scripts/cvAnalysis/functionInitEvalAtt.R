# functionInitEvalAtt.R

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

# Deine categorical separation points
InitEvalAtt=function(dat.in, qNum, respNum, qName){
# InitEvalAtt(DataName,
#  QuestionNumber (numeric),
#  ResponseNumber (numeric),
#  "qName" (String))

  low.thresh=7
  high.thresh=10

  # Figure out which columns correspond to answers to qNum
  index.qNum=which(colnames(dat.in)==qName)
  qDat=dat.in[,index.qNum]

  # Calculate Prior Proababilities
  PC1.index=which(dat.in$qTot<low.thresh & qDat==respNum)
  PrPc1=length(PC1.index)

  PC2.index=which(low.thresh<=dat.in$qTot & dat.in$qTot<high.thresh & qDat==respNum)
  PrPc2=length(PC2.index)

  PC3.index=which(dat.in$qTot >= high.thresh & qDat==respNum)
  PrPc3=length(PC3.index)

  PqRespNum=length(which(qDat==respNum))

  ret1=PrPc1/PqRespNum
  ret2=PrPc2/PqRespNum
  ret3=PrPc3/PqRespNum
  out=c(ret1, ret2, ret3)
  return(out)
}

####	End Script	 ####









