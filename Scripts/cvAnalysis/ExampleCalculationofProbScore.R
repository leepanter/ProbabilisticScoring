# ExampleCalculationofProbScore.R

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


####	Example CV calculation with n.set=50 ####
n.set.CVsplit=CVsplit(phq9Subset, 50)
train.set.list=n.set.CVsplit[[1]]
test.set.list=n.set.CVsplit[[2]]

train.set.weights=list()
for(i in 1:3){
  train.set.weights[[i]]=round(ReformatWeights(PCVeval_overQnum(train.set.list[[1]]))[[i]], digits = 4)
}

test.set.subject1.sequence=test.set.list[[1]][1,4:12]

subject.1.class.prob.t0=c(1/3,1/3,1/3)

weight.col.index.t0=which(Anum==test.set.subject1.sequence[1,1])

subject.1.class.prob.t1 =
  c(subject.1.class.prob.t0[1]*train.set.weights[[1]][1,weight.col.index.t0],
    subject.1.class.prob.t0[2]*train.set.weights[[2]][1,weight.col.index.t0],
    subject.1.class.prob.t0[3]*train.set.weights[[3]][1,weight.col.index.t0])


weight.col.index.t1=which(Anum==test.set.subject1.sequence[1,2])

subject.1.class.prob.t2 =
  c(subject.1.class.prob.t1[1]*train.set.weights[[1]][2,weight.col.index.t1],
    subject.1.class.prob.t1[2]*train.set.weights[[2]][2,weight.col.index.t1],
    subject.1.class.prob.t1[3]*train.set.weights[[3]][2,weight.col.index.t1])

weight.col.index.t2=which(Anum==test.set.subject1.sequence[1,3])

subject.1.class.prob.t3 =
  c(subject.1.class.prob.t2[1]*train.set.weights[[1]][3,weight.col.index.t2],
    subject.1.class.prob.t2[2]*train.set.weights[[2]][3,weight.col.index.t2],
    subject.1.class.prob.t2[3]*train.set.weights[[3]][3,weight.col.index.t2])


weight.col.index.t3=which(Anum==test.set.subject1.sequence[1,4])

(subject.1.class.prob.t4 =
    c(subject.1.class.prob.t3[1]*train.set.weights[[1]][4,weight.col.index.t3],
      subject.1.class.prob.t3[2]*train.set.weights[[2]][4,weight.col.index.t3],
      subject.1.class.prob.t3[3]*train.set.weights[[3]][4,weight.col.index.t3]))

weight.col.index.t4=which(Anum==test.set.subject1.sequence[1,5])

(subject.1.class.prob.t5 =
    c(subject.1.class.prob.t4[1]*train.set.weights[[1]][5,weight.col.index.t4],
      subject.1.class.prob.t4[2]*train.set.weights[[2]][5,weight.col.index.t4],
      subject.1.class.prob.t4[3]*train.set.weights[[3]][5,weight.col.index.t4]))

weight.col.index.t5=which(Anum==test.set.subject1.sequence[1,6])

(subject.1.class.prob.t6 =
    c(subject.1.class.prob.t5[1]*train.set.weights[[1]][6,weight.col.index.t5],
      subject.1.class.prob.t5[2]*train.set.weights[[2]][6,weight.col.index.t5],
      subject.1.class.prob.t5[3]*train.set.weights[[3]][6,weight.col.index.t5]))

weight.col.index.t6=which(Anum==test.set.subject1.sequence[1,7])

(subject.1.class.prob.t7 =
    c(subject.1.class.prob.t6[1]*train.set.weights[[1]][7,weight.col.index.t6],
      subject.1.class.prob.t6[2]*train.set.weights[[2]][7,weight.col.index.t6],
      subject.1.class.prob.t6[3]*train.set.weights[[3]][7,weight.col.index.t6]))

weight.col.index.t7=which(Anum==test.set.subject1.sequence[1,8])

(subject.1.class.prob.t8 =
    c(subject.1.class.prob.t7[1]*train.set.weights[[1]][8,weight.col.index.t7],
      subject.1.class.prob.t7[2]*train.set.weights[[2]][8,weight.col.index.t7],
      subject.1.class.prob.t7[3]*train.set.weights[[3]][8,weight.col.index.t7]))

weight.col.index.t8=which(Anum==test.set.subject1.sequence[1,9])

(subject.1.class.prob.t9 =
    c(subject.1.class.prob.t8[1]*train.set.weights[[1]][9,weight.col.index.t8],
      subject.1.class.prob.t8[2]*train.set.weights[[2]][9,weight.col.index.t8],
      subject.1.class.prob.t8[3]*train.set.weights[[3]][9,weight.col.index.t8]))


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
