####	CVInitialSetups.R	 ####

#-------------------------------------------------------------------------#
####	Description:	 ####
# This script
# - performs a Meta-analysis over the CV data partitions to determine a proper subset of K-values to use, and what possible N.Obs.train exist
# - performs an accuracy analysis to determine the optimal convergence threshold for the Pscore classification algorithm
# - Calculates the full P-score algorithm weights for the full data set, and the associate outcomes.


####	Script Dependencies	 ####

# Package Dependencies:
library(ggplot2)
library(reshape)

# Set Working Directory
WD="/Users/lee/Documents/GitHub/ProbabilisticScoring/Scripts/cvAnalysis"
setwd(WD)

# Data Dependencies:
# PHQ9 Data
source(file = "/Users/lee/Documents/GitHub/ProbabilisticScoring/Scripts/DataManagement/PHQ9/phq9DataSubsetImport.R")
# source(file="phq9DataSubsetImport.R")

# Variable Dependencies:
set.seed(123)
options(warn = -1)

# File Dependencies

## functions:
# Weight Calculations
source(file = "/Users/lee/Documents/GitHub/ProbabilisticScoring/Scripts/cvAnalysis/functionsWeightCalculations.R")
# source(file="functionsWeightCalculations.R")

# Probabilistic Score Calculations
source(file = "/Users/lee/Documents/GitHub/ProbabilisticScoring/Scripts/cvAnalysis/functionsProbScoreCalc.R")
# source(file="functionsProbScoreCalc.R")

# Scoring Analysis
source(file="/Users/lee/Documents/GitHub/ProbabilisticScoring/Scripts/cvAnalysis/functionsScoringAnalysis.R")
# source(file="functionsScoringAnalysis.R")

#-------------------------------------------------------------------------#
#-------------------------------------------------------------------------#
####	Begin Script	 ####
#-------------------------------------------------------------------------#
#-------------------------------------------------------------------------#

####	Sample-Length Analysis  ####
len.train=c()
N=2495
k=5:2490
len.k=length(k)
for(i in 1:len.k){
  len.train[i]=floor(N/(k[i]))*(k[i]-1)
}

# plot(len.train~k)

min.train=min(len.train)
min.index.train=which(len.train==min.train)

max.train=max(len.train)
max.index.train=which(len.train==max.train)

uniq.len.train=unique(len.train)

index.len.train=list()

len.uniq.k=length(uniq.len.train)

for(i in 1:len.uniq.k){
  index.len.train[[i]]=which(len.train==sort(uniq.len.train)[i])
}

set.info=list()
k.sets=list()
N.obs.training.set=c()


for(i in 1:len.uniq.k){
  N.obs.training.set[[i]]=sort(uniq.len.train)[i]
  k.sets[[i]]=k[index.len.train[[i]]]
}
set.info=list(N.obs.training.set, k.sets)

even.probs.vector=c()
for(i in 1:len.uniq.k){
  even.probs.vector[i]=set.info[[2]][[i]][1]
}

df.set.info=data.frame(as.numeric(even.probs.vector), N.obs.training.set)
colnames(df.set.info)=c("df.k.sets", "N.obs.training.set")
 #plot(df.set.info$N.obs.training.set~df.set.info$df.k.sets)


#-------------------------------------------------------------------------#
#### full data weight calculations	 ####
#-------------------------------------------------------------------------#
full.data=phq9
full.data.weights=ReformatWeights(PCVeval_overQnum(full.data))
for(i in 1:3){
  full.data.weights[[i]]=round(full.data.weights[[i]], digits = 4)
}

####  Calculate full data-weight probabilistic outcomes
full.data.probSequences=EvalSeqSubject(full.data.weights, full.data, 1)
full.data.probClasses=c()
full.data.probClasses.Convg=list()

for(i in 1:2495){
  full.data.probClasses.Convg[[i]]=convg(full.data.probSequences[[i]], 0.75)
}

for(i in 1:2495){
  full.data.probClasses[i]=full.data.probClasses.Convg[[i]][[3]]
}

phq9$FullDatProbClass.num=full.data.probClasses



#-------------------------------------------------------------------------#
#### Accuracy as a function of threshold specification, for full data set	 ####
#-------------------------------------------------------------------------#
thresholdAccuracy=function(accuracy.in, data.weights.in, data.set.in){
  init.accuracy.in=accuracy.in
  init.data.weights.in=data.weights.in
  init.data.set.in=data.set.in

  data.probClasses=c()
  data.probClasses.Convg=list()

  rowdim.data.set.in=dim(init.data.set.in)[1]
  data.probSequences=EvalSeqSubject(init.data.weights.in, init.data.set.in, 1)

  for(i in 1:rowdim.data.set.in){
    data.probClasses.Convg[[i]]=convg(data.probSequences[[i]], init.accuracy.in)
  }

  for(i in 1:rowdim.data.set.in){
    data.probClasses[i]=data.probClasses.Convg[[i]][[3]]
  }

  out.data.accuracy=length(which(data.probClasses==init.data.set.in$SupOutNum))/rowdim.data.set.in
  return(out.data.accuracy)
}

thresholdAccuracy.vector_fulldata=c()
accuracy.threshold.argument=seq(from=0.35, to = 0.95, by = 0.025)

for(i in 1:25){
  thresholdAccuracy.vector_fulldata[i]=thresholdAccuracy(accuracy.threshold.argument[i],
                                                         full.data.weights,
                                                         full.data)
}
# plot(thresholdAccuracy.vector_fulldata~accuracy.threshold.argument)
thresholdAccuracy.vector_fulldata[17]
accuracy.threshold.argument[17]

# 0.75 is the highest classification accuracy argument


#-------------------------------------------------------------------------#
####	Remove Unused Variables	 ####
#-------------------------------------------------------------------------#
rm(index.len.train); rm(k.sets); rm(set.info);
rm(even.probs.vector); rm(i); rm(k);
rm(len.k); rm(len.train); rm(len.uniq.k);
rm(max.index.train); rm(max.train); rm(min.index.train);
rm(min.train); rm(N); rm(N.obs.training.set);
rm(uniq.len.train);

rm(full.data); rm(full.data.probClasses.Convg)
rm(full.data.probSequences); rm(full.data.probClasses)

rm(accuracy.threshold.argument); rm(thresholdAccuracy.vector_fulldata)


#-------------------------------------------------------------------------#
#-------------------------------------------------------------------------#
####	End Script	 ####
#-------------------------------------------------------------------------#
#-------------------------------------------------------------------------#