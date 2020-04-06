####	MasterCVanalysis.R	 ####

#-------------------------------------------------------------------------#
####	Description:	 ####
# This script will combine the constituent functions, data, and variables needed in order to perform a cross-validation examination of Probabilistic Scoring.


####	Script Dependencies	 ####

# Package Dependencies:
library(ggplot2)

# Set Working Directory
WD="/Users/lee/Documents/GitHub/ProbabilisticScoring/Scripts/cvAnalysis"
setwd(WD)

# Data Dependencies:
# PHQ9 Data
source(file = "/Users/lee/Documents/GitHub/ProbabilisticScoring/Scripts/DataManagement/PHQ9/phq9DataSubsetImport.R")

# Variable Dependencies:
set.seed(123)
options(warn = -1)

# File Dependencies

## functions:

# Weight Calculations
source(file = "/Users/lee/Documents/GitHub/ProbabilisticScoring/Scripts/cvAnalysis/functionsWeightCalculations.R")

# Probabilistic Score Calculations
source(file = "/Users/lee/Documents/GitHub/ProbabilisticScoring/Scripts/cvAnalysis/functionsProbScoreCalc.R")

# Scoring Analysis
source(file="/Users/lee/Documents/GitHub/ProbabilisticScoring/Scripts/cvAnalysis/functionsScoringAnalysis.R")

#-------------------------------------------------------------------------#
####	Begin Script	 ####
#-------------------------------------------------------------------------#

####	CV-meta analysis	 ####

####  Determine the number of training observations for each K-set selection
number.of.training.obs=c()
number.of.sets=3:2493
for(i in 3:2493){
  number.of.training.obs[i-2]=(floor(2495/number.of.sets[i-2]))*(number.of.sets[i-2]-1)
}

####  Determine min and max number of training observations possible
min.train.obs=min(number.of.training.obs)
max.train.obs=max(number.of.training.obs)

####  Determine which k-value correspond to the extemas
number.of.sets.min=number.of.sets[which.min(number.of.training.obs)]
number.of.sets.max=number.of.sets[which.max(number.of.training.obs)]


#### full data weight calculations	 ####
full.data=phq9

#-------------------------------------------------------------------------#

####	CVk	 ####
####  This Portion will now generalize to arbitrary k values
accuracy.ksets=c()
N.obs.k=c()
k.set.values=seq(from=5, to = 2490, by=25)


for(k.in in 1:100){
  k=k.set.values[k.in]
  k.index=1+k-750

  ####	Divide Data into K CV data sets
  CVk.dat=CVsplit(full.data, k)
  Number.k.obs=CVk.dat[[3]]

  ####  Initialized Empty Variables
  CVk_j.train=list()
  CVk_j.test=list()
  CVk_j.train.weights=list()
  CVk_j.data.accuracy=c()

  for(j in 1:k){
    CVk_j.train[[j]]=CVk.dat[[1]][[j]]
    CVk_j.test[[j]]=CVk.dat[[2]][[j]]

    CVk_j.train.weights[[j]]=ReformatWeights(PCVeval_overQnum(CVk_j.train[[j]]))

    for(i in 1:3){
      CVk_j.train.weights[[j]][[i]]=round(CVk_j.train.weights[[j]][[i]], digits = 4)
    }

    ###  Calculate CVk-j probabilistic outcomes
    CVk_j.probSequences=EvalSeqSubject(CVk_j.train.weights[[j]], CVk_j.test[[j]], 1)
    CVk_j.probClasses=c()
    CVk_j.probClasses.Convg=list()

    for(i in 1:Number.k.obs){
      CVk_j.probClasses.Convg[[i]]=convg(CVk_j.probSequences[[i]], 0.95)
    }

    for(i in 1:Number.k.obs){
      CVk_j.probClasses[i]=CVk_j.probClasses.Convg[[i]][[3]]
    }

    ####  Calculate Accuracy of probablistic classes
    CVk_j.data.accuracy[j]=length(which(CVk_j.probClasses==CVk_j.test[[j]]$SupOutNum))/Number.k.obs

  }

  ####  Output Minimum Accuracy Values
  CVk.accuracy=mean(CVk_j.data.accuracy)
  accuracy.ksets[k.index]=CVk.accuracy
  N.obs.k[k.index]=Number.k.obs*(k-1)
}

x=data.frame(N.obs.k, accuracy.ksets)

p=ggplot(x, aes(x=N.obs.k, y=accuracy.ksets))+
  geom_line()+
  geom_point()

p

#-------------------------------------------------------------------------#
####	End Script	 ####
#-------------------------------------------------------------------------#



#-------------------------------------------------------------------------#
#####	Post-Script	#####

####  Notes:

####  Compilation Errors:

####  Execution Errors:

####  Next Scripts to Consider:

#-------------------------------------------------------------------------#
