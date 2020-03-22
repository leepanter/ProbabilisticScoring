####	MasterCVanalysis.R	 ####

#-------------------------------------------------------------------------#
####	Description:	 ####
# This script will combine the constituent functions, data, and variables needed in order to perform a cross-validation examination of Probabilistic Scoring.


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
####	Begin Script	 ####
#-------------------------------------------------------------------------#

#### full data weight calculations	 ####

full.data=phq9Subset
full.data.weights=ReformatWeights(PCVeval_overQnum(full.data))
for(i in 1:3){
  full.data.weights[[i]]=round(full.data.weights[[i]], digits = 4)
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
