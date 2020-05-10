####	MasterCVanalysis.R	 ####

#-------------------------------------------------------------------------#
####	Description:	 ####
# This script will combine the constituent functions, data, and variables needed in order to perform a cross-validation examination of Probabilistic Scoring.


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

## File Dependencies
# CVInitialSetup.R
source(file = "/Users/lee/Documents/GitHub/ProbabilisticScoring/Scripts/cvAnalysis/CVInitialSetups.R")
# source(file="CVInitialSetups.R")

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
####	Begin Script	 ####
#-------------------------------------------------------------------------#

# Initialize Empty Variables in Global Scope
accuracy.ksets=c()
traditional.accuracy.ksets=c()
N.obs.k=c()
boot.sample.i=list()


# number.samples=100
# sample.length=number.samples+2
# df.set.info=df.train.set.info
# colnames(df.set.info)=c("df.k.sets", "N.obs.train.set")
# sample.vec.k.sets=df.set.info$df.k.sets
# sample.vec.k.sets=sample.vec.k.sets[-c(1,1245)]
# N.set.arg=sort(sample(sample.vec.k.sets, number.samples, replace = FALSE))
# N.set.arg=sort(c(N.set.arg,1247, 1248))
#

df.set.info=df.k.final
colnames(df.set.info)=c("df.k.sets", "N.obs.train", "N.obs.test" )
sample.length=25
N.set.arg=sample(df.set.info$df.k.sets, sample.length, replace = FALSE)


for(i in 1:sample.length){
  boot.sample.i[[i]]=CVsplit(phq9, N.set.arg[i])
}


for(k in 1:sample.length){
  k.setVal=N.set.arg[k]
  k.index=k

  ####	Divide Data into K CV data sets
  CVk.dat=boot.sample.i[[k.index]]
  Number.k.obs=CVk.dat[[3]]


  ####  Initialized Empty Variables
  CVk_j.train=list()
  CVk_j.test=list()
  CVk_j.train.weights=list()
  CVk_j.data.accuracy=c()
  CVk_j.data.accuracy.traditional=c()

  for(j in 1:k.setVal){
    CVk_j.train[[j]]=CVk.dat[[1]][[j]]
    CVk_j.test[[j]]=CVk.dat[[2]][[j]]
    CVk_j.probClasses=c()
    CVk_j.probClasses.Convg=list()

    ### Calculate data weights
    CVk_j.train.weights[[j]]=ReformatWeights(PCVeval_overQnum(CVk_j.train[[j]]))

    for(i in 1:3){
      CVk_j.train.weights[[j]][[i]]=round(CVk_j.train.weights[[j]][[i]], digits = 4)
    }

    ###  Calculate CVk-j probabilistic outcomes
    CVk_j.probSequences=EvalSeqSubject(CVk_j.train.weights[[j]], CVk_j.test[[j]], 1)

    for(i in 1:Number.k.obs){
      CVk_j.probClasses.Convg[[i]]=convg(CVk_j.probSequences[[i]], 0.75)
    }

    for(i in 1:Number.k.obs){
      CVk_j.probClasses[i]=CVk_j.probClasses.Convg[[i]][[3]]
    }

    ####  Calculate Accuracy of probablistic classes
    CVk_j.data.accuracy[j]=length(which(CVk_j.probClasses==CVk_j.test[[j]]$SupOutNum))/Number.k.obs

    ####  Calculate Accuracy of traditional Classes
    CVk_j.data.accuracy.traditional[j]=length(which(CVk_j.test[[j]]$sumClassNum==CVk_j.test[[j]]$SupOutNum))/Number.k.obs
  }

  ####  Output Accuracy Values
  CVk.accuracy=mean(CVk_j.data.accuracy)
  accuracy.ksets[k.index]=CVk.accuracy

  CVk.accuracy.traditional=mean(CVk_j.data.accuracy.traditional)
  traditional.accuracy.ksets[k.index]=CVk.accuracy.traditional
}


for(k in 1:sample.length){
  N.obs.k[k]=dim(boot.sample.i[[k]][[1]][[1]])[1]
}

accuracy.df=data.frame(N.obs.k, accuracy.ksets)


accuracy.lm=lm(accuracy.ksets~N.obs.k, data = accuracy.df)
(accuracy.lms=summary(accuracy.lm))

accuracy.plot=ggplot(accuracy.df, aes(x=N.obs.k, y=accuracy.ksets))+
  xlab("Observations in Traing Set")+
  ylab("Accuracy of Prob.Scoring")+
  geom_abline(intercept = as.numeric(coef(accuracy.lm))[[1]],
              slope=as.numeric(coef(accuracy.lm))[[2]])+
  geom_point()
accuracy.plot

zeros=rep(0, times=sample.length)
ones=rep(1, times=sample.length)
ID=as.factor(c(ones, zeros))
N.obs.train.k.lmFE=rep(N.obs.k, times=2)
accuracy.out.lmFE=c(accuracy.ksets,traditional.accuracy.ksets)
accuracy.df.lmFE=data.frame(N.obs.train.k.lmFE, ID, accuracy.out.lmFE)

accuracy.lmFE=lm(accuracy.out.lmFE~ID+N.obs.train.k.lmFE+ID:N.obs.train.k.lmFE,
                 data = accuracy.df.lmFE)
(accuracy.lmFEs=summary(accuracy.lmFE))

accuracy.df.lmFE.re=reshape::melt(accuracy.df.lmFE, id.vars=c("N.obs.train.k.lmFE","ID"))

trad.intercept=as.numeric(coef(accuracy.lmFE))[1]+as.numeric(coef(accuracy.lmFE))[2]
trad.slope=as.numeric(coef(accuracy.lmFE))[3]+as.numeric(coef(accuracy.lmFE))[4]

prob.intercept=as.numeric(coef(accuracy.lmFE))[1]
prob.slope=as.numeric(coef(accuracy.lmFE))[3]


p=ggplot2::ggplot(accuracy.df.lmFE.re, aes(x = N.obs.train.k.lmFE, y = accuracy.out.lmFE,
                                           group = ID))+
  geom_point()+
  geom_abline(intercept = trad.intercept, slope = trad.slope)+
  geom_abline(intercept = prob.intercept, slope = prob.slope)


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
