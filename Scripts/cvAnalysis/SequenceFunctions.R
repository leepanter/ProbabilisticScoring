####	SequenceFunctions.R	 ####
####	Description:	 ####

####	Description:	 ####
# This script will define the following functions that will evaluate a subjects answer for a probabilistic fit.

#-------------------------------------------------------------------------#
####   Generalizes over training/test data pairings
## EvalSeqData=function(?)


####  Generalizes over multiple subjects in a single test/train pairing
## EvalSeqSubjec=function(train.weights.in, test.data.in, set.no)



####	Evaluates sequence for classification convergence for one subject in a single train/test pairing
## EvalSeq=function(train.weights.in, test.data.in, set.no, subject.no)


#-------------------------------------------------------------------------#
####	Script Dependencies	 ####

# Package Dependencies:


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

## function: ReformatWeights(in.list)
source(file="/Users/lee/Documents/GitHub/ProbabilisticScoring/Scripts/cvAnalysis/functionReformatWeights.R")

#-------------------------------------------------------------------------#
#-------------------------------------------------------------------------#


####	Evaluates sequence for classification convergence for one subject in a single train/test pairing
## EvalSeq=function(train.weights.in, test.data.in, set.no.in, subject.no.in)


EvalSeq=function(train.weights.in, test.data.in, set.no.in, subject.no.in){
  init.train.weights.in=train.weights.in
  init.test.data.in=test.data.in
  init.set.no.in=set.no.in
  init.subject.no.in=subject.no.in

  sub.Prob.sequence=list()
  sub.Prob.sequence[[1]]=c(1/3,1/3,1/3)
  sub.answer.sequence=init.test.data.in[init.subject.no.in, 4:12]
  weight.col.index=c()
  for(i in 1:9){
    weight.col.index[i]=which(Anum==sub.answer.sequence[1,i])
  }

  for(i in 2:9){
    sub.Prob.sequence[[i]]=c(
      sub.Prob.sequence[[i-1]][1]*init.train.weights.in[[1]][1,weight.col.index[i-1]],
      sub.Prob.sequence[[i-1]][2]*init.train.weights.in[[2]][1,weight.col.index[i-1]],
      sub.Prob.sequence[[i-1]][3]*init.train.weights.in[[3]][1,weight.col.index[i-1]])
  }

  return(sub.Prob.sequence)
}




####  Generalizes over multiple subjects in a single test/train pairing
## EvalSeqSubject=function(train.weights.in, test.data.in, set.no.in)

EvalSeqSubject=function(train.weights.in, test.data.in, set.no.in){
  init.train.weights.in.EvalSeqSubject=train.weights.in
  init.test.data.in.EvalSeqSubject=test.data.in
  init.set.no.in.EvalSeqSubject=set.no.in

  row.dim=dim(init.test.data.in.EvalSeqSubject)[1]
  Eval.Subjects.Prob.sequence=list()

  for(i in 1:row.dim){
    Eval.Subjects.Prob.sequence[[i]]=EvalSeq(init.train.weights.in.EvalSeqSubject,
                                             init.test.data.in.EvalSeqSubject,
                                             init.set.no.in.EvalSeqSubject,
                                             i)
  }

  return(Eval.Subjects.Prob.sequence)
}













CVdat=CVsplit(phq9Subset, N.set = 50)
train.weights.in=list()
for(i in 1:3){
  train.weights.in[[i]]=round(ReformatWeights(PCVeval_overQnum(CVdat[[1]][[5]]))[[i]], digits = 4)
}
test.data.in = CVdat[[2]][[5]]
set.no.in = 5


  # init.train.weights.in.EvalSeqSubject=train.weights.in
  # init.test.data.in.EvalSeqSubject=test.data.in
  # init.set.no.in.EvalSeqSubject=set.no.in
  #
  # row.dim=dim(init.test.data.in.EvalSeqSubject)[1]
  # Eval.Subjects.Prob.sequence=list()
  #
  # for(i in 1:row.dim){
  #   Eval.Subjects.Prob.sequence[[i]]=EvalSeq(init.train.weights.in.EvalSeqSubject,
  #                                            init.test.data.in.EvalSeqSubject,
  #                                            init.set.no.in.EvalSeqSubject,
  #                                            i)
  # }


#
# subject.no.in = 20
#
#   init.train.weights.in=train.weights.in
#   init.test.data.in=test.data.in
#   init.set.no.in=set.no.in
#   init.subject.no.in=subject.no.in
#
#   sub.Prob.sequence=list()
#   sub.Prob.sequence[[1]]=c(1/3,1/3,1/3)
#   sub.answer.sequence=init.test.data.in[init.subject.no.in, 4:12]
#   weight.col.index=c()
#   for(i in 1:9){
#     weight.col.index[i]=which(Anum==sub.answer.sequence[1,i])
#   }
#
#
#   for(i in 2:9){
#     sub.Prob.sequence[[i]]=c(
#       sub.Prob.sequence[[i-1]][1]*init.train.weights.in[[1]][1,weight.col.index[i-1]],
#       sub.Prob.sequence[[i-1]][1]*init.train.weights.in[[2]][1,weight.col.index[i-1]],
#       sub.Prob.sequence[[i-1]][1]*init.train.weights.in[[3]][1,weight.col.index[i-1]])
#   }






