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

#### full data weight calculations	 ####
full.data=phq9
full.data.weights=ReformatWeights(PCVeval_overQnum(full.data))
for(i in 1:3){
  full.data.weights[[i]]=round(full.data.weights[[i]], digits = 4)
}


####	Example of calculation of Probabilistic Weights	 ####
CV4.dat=CVsplit(phq9, 4)
CV4.sequences=EvalSeqData(CV4.dat[[1]], CV4.dat[[2]], 4)

CV4.convg.90=list()

for(i in 1:623){
  CV4.convg.90[[i]]=convg(CV4.sequences[[1]][[i]], 0.90)
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
