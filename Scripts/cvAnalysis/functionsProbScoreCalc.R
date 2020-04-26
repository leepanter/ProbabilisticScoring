####	functionsProbScoreCalc.R	 ####
####	Description:	 ####
# This script will define the following functions that will evaluate a subjects answer for a probabilistic fit.

#-------------------------------------------------------------------------#
####   Generalizes over training/test data pairings
## EvalSeqData=function(cvData.train.in, cvData.test.in, cvData.N.set.in)


####  Generalizes over multiple subjects in a single test/train pairing
## EvalSeqSubjec=function(train.weights.in, test.data.in, set.no)



####	Evaluates sequence for classification convergence for one subject in a single train/test pairing
## EvalSeq=function(train.weights.in, test.data.in, set.no, subject.no)


#-------------------------------------------------------------------------#
####	Script Dependencies	 ####

# Package Dependencies:


# Set Working Directory
WD="/Users/lee/Documents/GitHub/ProbabilisticScoring/Scripts/cvAnalysis"
# WD="/home/lpanter/PScoreUpdate1/GCloudUpdate"
setwd(WD)

# Data Dependencies:
# PHQ9 Data
source(file = "/Users/lee/Documents/GitHub/ProbabilisticScoring/Scripts/DataManagement/PHQ9/functionsDataImportProcessing.R")

# Variable Dependencies:
set.seed(123)

# File Dependencies

## functions:
source(file = "/Users/lee/Documents/GitHub/ProbabilisticScoring/Scripts/cvAnalysis/functionsWeightCalculations.R")

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

  sub.Prob.sequence.new=list()
  sub.Prob.sequence.sum=c()

  for(i in 1:9){
    sub.Prob.sequence.sum[i]=sum(sub.Prob.sequence[[i]])
    sub.Prob.sequence.new[[i]]=sub.Prob.sequence[[i]]/sub.Prob.sequence.sum[i]
  }
  return(sub.Prob.sequence.new)
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




EvalSeqData=function(cvData.train.in, cvData.test.in, cvData.N.set.in){
  init.cvData.train.in=cvData.train.in
  init.cvData.test.in=cvData.test.in
  init.cvData.N.set.in=cvData.N.set.in

  train.weights.overSet=list()
  for(i in 1:init.cvData.N.set.in){
    train.weights.overSubject=list()
    for(j in 1:3){
      train.weights.overSubject[[j]]=round(ReformatWeights(PCVeval_overQnum(cvData.train.in[[i]]))[[j]], digits = 4)
    }
    train.weights.overSet[[i]]=list(train.weights.overSubject[[1]],
                                    train.weights.overSubject[[2]],
                                    train.weights.overSubject[[3]])
  }

  out.seq.subj.probs.overSet=list()
  for(i in 1:init.cvData.N.set.in){
    out.seq.subj.probs.overSet[[i]]=EvalSeqSubject(train.weights.overSet[[i]],
                                                   init.cvData.test.in[[i]], i)
  }

  return(out.seq.subj.probs.overSet)
}

