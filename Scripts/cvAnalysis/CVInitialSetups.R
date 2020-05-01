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

####	Sample-length Analysis Train only ####
len.train=c()
N=2495
k=5:2490
len.k=length(k)

for(i in 1:len.k){
  len.train[i]=floor(N/(k[i]))*(k[i]-1)
}

min.train=min(len.train)
min.index.train=which(len.train==min.train)

max.train=max(len.train)
max.index.train=which(len.train==max.train)

uniq.len.train=unique(len.train)

index.len.train=list()

len.train.uniq.k=length(uniq.len.train)



for(i in 1:len.train.uniq.k){
  index.len.train[[i]]=which(len.train==sort(uniq.len.train)[i])
}

set.train.info=list()
k.sets.train=list()
N.obs.training.set=c()

for(i in 1:len.train.uniq.k){
  N.obs.training.set[[i]]=sort(uniq.len.train)[i]
  k.sets.train[[i]]=k[index.len.train[[i]]]
}

set.train.info=list(N.obs.training.set, k.sets.train)

even.probs.vector.train=c()

for(i in 1:len.train.uniq.k){
  even.probs.vector.train[i]=set.train.info[[2]][[i]][1]
}

df.train.set.info=data.frame(as.numeric(even.probs.vector.train),
                             N.obs.training.set)
colnames(df.train.set.info)=c("df.k.sets.train", "N.obs.training.set")


####	Sample-Length Analysis  test/train####
len.train=c()
len.test=c()
N=2495
k=5:2490
len.k=length(k)
for(i in 1:len.k){
  len.train[i]=floor(N/(k[i]))*(k[i]-1)
}
for(i in 1:len.k){
  len.test[i]=floor(N/(k[i]))
}

min.train=min(len.train)
min.index.train=which(len.train==min.train)

min.test=min(len.test)
min.index.test=which(len.test==min.test)

max.train=max(len.train)
max.index.train=which(len.train==max.train)

max.test=max(len.test)
max.index.test=which(len.test==max.test)

uniq.len.train=unique(len.train)
uniq.len.test=unique(len.test)

index.len.train=list()
index.len.test=list()

len.train.uniq.k=length(uniq.len.train)
len.test.uniq.k=length(uniq.len.test)


for(i in 1:len.train.uniq.k){
  index.len.train[[i]]=which(len.train==sort(uniq.len.train)[i])
}

for(i in 1:len.test.uniq.k){
  index.len.test[[i]]=which(len.test==sort(uniq.len.test)[i])
}

set.train.info=list()
k.sets.train=list()
N.obs.training.set=c()

set.test.info=list()
k.sets.test=list()
N.obs.testing.set=c()

for(i in 1:len.train.uniq.k){
  N.obs.training.set[[i]]=sort(uniq.len.train)[i]
  k.sets.train[[i]]=k[index.len.train[[i]]]
}

for(i in 1:len.test.uniq.k){
  N.obs.testing.set[[i]]=sort(uniq.len.test)[i]
  k.sets.test[[i]]=k[index.len.test[[i]]]
}


set.train.info=list(N.obs.training.set, k.sets.train)
set.test.info=list(N.obs.testing.set, k.sets.test)


even.probs.vector.train=c()
even.probs.vector.test=c()

for(i in 1:len.train.uniq.k){
  even.probs.vector.train[i]=set.train.info[[2]][[i]][1]
}

for(i in 1:len.test.uniq.k){
  even.probs.vector.test[i]=set.test.info[[2]][[i]][1]
}


df.train.set.info=data.frame(as.numeric(even.probs.vector.train),
                             N.obs.training.set)
colnames(df.train.set.info)=c("df.k.sets.train", "N.obs.training.set")

df.test.set.info=data.frame(as.numeric(even.probs.vector.test),
                             N.obs.testing.set)
colnames(df.test.set.info)=c("df.k.sets.test", "N.obs.testing.set")

k.training.values=sort(df.train.set.info$df.k.sets.train)[1:10]

df.set.info=data.frame(k, len.train, len.test)

# p=ggplot(df.set.info, aes(x=k))+
#   geom_point(aes(y=len.train), color="tan4")+
#   geom_point(aes(y=len.test), color="tomato")
# p
#
# p=ggplot(df.train.set.info, aes(x=df.k.sets.train))+
#   geom_point(aes(y=N.obs.training.set))+
#   xlim(0,2500)+
#   geom_hline(yintercept = 1996, color="blue")+
#   geom_vline(xintercept = 5, color="blue")+
#   geom_hline(yintercept = 2320, color="red")+
#   geom_vline(xintercept=233, color="red")
# p
#
#
# p=ggplot(df.test.set.info, aes(x=df.k.sets.test))+
#   geom_point(aes(y=N.obs.testing.set))+
#   xlim(0,2500)+
#   geom_hline(yintercept = 499, color="blue")+
#   geom_vline(xintercept = 5, color="blue")+
#   geom_hline(yintercept = 10, color="red")+
#   geom_vline(xintercept=233, color="red")
# p

k.final=sort(df.train.set.info$df.k.sets.train)[1:100]
df.train.set.info=df.train.set.info[order(df.train.set.info$df.k.sets.train),]
N.obs.train.final=df.train.set.info$N.obs.training.set[1:100]
N.obs.test.final=c()
for(i in 1:100){
  N.obs.test.final[i]=floor(N/k.final[i])
}

df.k.final=data.frame(k.final, N.obs.train.final, N.obs.test.final)
colnames(df.k.final)=c("k", "N.obs.train", "N.obs.test")


# p=ggplot(df.k.final, aes(x=k))+
#   geom_point(aes(y=N.obs.train), color="tan4")+
#   geom_point(aes(y=N.obs.test), color="tomato")
# p


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

rm(df.set.info); rm(df.test.set.info);
rm(index.len.test); rm(k.sets.test);
rm(k.sets.train); rm(p); rm(set.test.info);
rm(set.train.info); rm(even.probs.vector.test); rm(even.probs.vector.train);
rm(k.final); rm(k.training.values); rm(len.test);
rm(len.test.uniq.k); rm(len.train.uniq.k); rm(max.index.test);
rm(max.test); rm(min.index.test); rm(min.test);
rm(N.obs.test.final); rm(N.obs.testing.set); rm(N.obs.train.final);
rm(uniq.len.train); rm(uniq.len.test)



#-------------------------------------------------------------------------#
#-------------------------------------------------------------------------#
####	End Script	 ####
#-------------------------------------------------------------------------#
#-------------------------------------------------------------------------#