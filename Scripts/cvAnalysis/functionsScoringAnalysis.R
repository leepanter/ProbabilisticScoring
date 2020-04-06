####	functionsScoringAnalysis	 ####
####	Description:	 ####
# This script will define the following functions that will evaluate data CVsplit data against the supervised learning data.

#-------------------------------------------------------------------------#
####   convgData()
## Generalizes the convgSub() functiong over training/test data pairings

####   convgSub()
## Generalizes the convg() function over multiple subjects in a single test/train pairing

####   convg(sub.seq.in, thresh.in)
## Determines the convergence of a single subject's probabilisticly defined class

####  ConvgCompare(out.Convgfunction.in, sub.sup.data.in)
## Compares the supervised outcome in the data to the Probabilistic outcome determined by convg

#-------------------------------------------------------------------------#
####	Script Dependencies	 ####

# Package Dependencies:


# Set Working Directory
WD="/Users/lee/Documents/GitHub/ProbabilisticScoring/Scripts/cvAnalysis"
setwd(WD)

# Data Dependencies:
# PHQ9 Data
source(file = "/Users/lee/Documents/GitHub/ProbabilisticScoring/Scripts/DataManagement/PHQ9/functionsDataImportProcessing.R")

# Variable Dependencies:
set.seed(123)

# File Dependencies

## functions:
source(file = "/Users/lee/Documents/GitHub/ProbabilisticScoring/Scripts/cvAnalysis/functionsProbScoreCalc.R")

#-------------------------------------------------------------------------#
#-------------------------------------------------------------------------#

####   convg(sub.seq.in, thresh.in)
# Description: Defines a function that determines if an subject specific input sequence of probabilities (3x9 probabilistic) converges to a class (C1, C2, C3) with a provided probability threshold.

# Arguments:
#   - sub.seq.in: 3x9 sequence of probabilistic scores specific to an individual.
#   - thresh.in: probabilistic convergence threshold (e.g. 90%, 95%, 99.5%)

# Returns:
#   - index.j: how many iterations of the probabilistic scoring algorithm were required for the sequence to converge. (note that $\text{thresh.in}>\frac{1}{3} \ \Rightarrow \ \text{index.j}\in\left \{  2,\ldots,9   \right \}$)
#   - max.seq.val: Probability at convergence
#   - index.max: Class index at maximum probability (which class eventually converged to)

convg=function(sub.seq.in, thresh.in){
  init.sub.seq.in=sub.seq.in
  init.thresh.in=thresh.in


  max.seq.val=c()
  max.seq.val[1]=1/3

  for(i in 2:9){
    max.seq.val[i]=max(init.sub.seq.in[[i]])
  }

  index.j=min(which(max.seq.val >= init.thresh.in))

  if (index.j<9 | max.seq.val[9] >= init.thresh.in){
    index.max=which(init.sub.seq.in[[index.j]]==max.seq.val[index.j])
    out.result=list(index.j, max.seq.val[index.j], index.max)
    return(out.result)
  } else return(list(NA, NA, NA))
}


####   ConvgCompare(out.Convgfunction.in, sub.sup.data.in)
# Description: Compares the resulting classification of the convg() function to the supervised outcome

# Arguments:
#   - out.Convgfunction.in: Numerical classification resulting from convg function
#   - sub.sup.data.in: Numerical supervised classification fro PHQ9 data

# Returns:
#   - out.result: 1 if classifications are the same, 0 if not


ConvgCompare=function(out.Convgfunction.in, sub.sup.data.in){
  init.out.Convgfunction.in=out.Convgfunction.in
  init.sub.sup.data.in=sub.sup.data.in
  out.result=NA_integer_

  if(init.out.Convgfunction.in==init.sub.sup.data.in){
    out.result=1
  } else {
    out.result=0
  }
  return(out.result)
}



