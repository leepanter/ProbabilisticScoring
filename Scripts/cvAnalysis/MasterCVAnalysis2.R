####	MasterCVanalysis.R	 ####

#-------------------------------------------------------------------------#
####	Description:	 ####
# This script will combine the constituent functions, data, and variables needed in order to perform a cross-validation examination of Probabilistic Scoring.


####	Script Dependencies	 ####

# Package Dependencies:
library(ggplot2)
library(boot)
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


####	Create Bootsrap Sample Distributions	 ####
## This portion of code will subsample the given data.  Each subsample will include all colums of data, and the number of rows in each sub-sample will range from one to 2495.  Each subsample will be stored in its own list item so that a subsample distribution of weights may be generated in the following step.

boot.index.sample.i=list()

for(i in 1:2495){
  index=1:2495
  loop.sample.max.times=sample.max.times[i]
  init.i=i
  j=1
  boot.index.sample.j=list()
  while((j <= 500)){
    boot.index.sample.j[[j]]=sample(index, init.i, replace = TRUE)
    j=j+1
  }
  boot.index.sample.i[[i]]=boot.index.sample.j
}


boot.sample.i=list()

for(i in 1:2495){
  boot.sample.i.j=list()
  for(j in 1:500){
    boot.sample.i.j[[j]]=phq9[boot.index.sample.i[[1]][[1]],]
  }
  boot.sample.i[[i]]=boot.sample.i.j
}


####	Weight Bootstrap Distributions	 ####
## We will now perform the weight calculations for each of the bootstrap subsamples.

boot.weights.i=list()

for(i in 1:2495){
  boot.weights.i.j=list()
  for(j in 1:500){
    boot.weights.i.j[[j]]=ReformatWeights(PCVeval_overQnum(boot.sample.i[[i]][[j]]))
    for(k in 1:3){
      boot.weights.i.j[[j]][[k]]=round(boot.weights.i.j[[j]][[k]], digits = 4)
    }
  }
  boot.weights.i[[i]]=boot.weights.i.j
}


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
