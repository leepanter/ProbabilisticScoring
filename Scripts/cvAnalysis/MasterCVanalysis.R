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

plot(number.of.training.obs~number.of.sets)

####  Determine min and max number of training observations possible
min.train.obs=min(number.of.training.obs)
max.train.obs=max(number.of.training.obs)

####  Determine which k-value correspond to the extemas
number.of.sets.min=number.of.sets[which.min(number.of.training.obs)]
number.of.sets.max=number.of.sets[which.max(number.of.training.obs)]

number.of.training.obs.subset=number.of.training.obs[1247:2493]
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
  full.data.probClasses.Convg[[i]]=convg(full.data.probSequences[[i]], 0.66)
}

for(i in 1:2495){
  full.data.probClasses[i]=full.data.probClasses.Convg[[i]][[3]]
}

full.data.accuracy=length(which(full.data.probClasses==full.data$SupOutNum))/2495


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
plot(thresholdAccuracy.vector_fulldata~accuracy.threshold.argument)
thresholdAccuracy.vector_fulldata[17]
accuracy.threshold.argument[17]

# 0.75 is the highest classification accuracy argument


#-------------------------------------------------------------------------#
####	CV 4	 ####
#-------------------------------------------------------------------------#

####	Divide Data into 4 CV data sets
CV4.dat=CVsplit(full.data, 4)

####  Initialized Empty Variables
CV4_j.train=list()
CV4_j.test=list()
CV4_j.train.weights=list()
CV4_j.data.accuracy=c()
CV4_j.data.accuracy.traditional=c()
CV4_j.probClasses=c()
CV4_j.probClasses.Convg=list()


for(j in 1:4){
  CV4_j.train[[j]]=CV4.dat[[1]][[j]]
  CV4_j.test[[j]]=CV4.dat[[2]][[j]]

  CV4_j.train.weights[[j]]=ReformatWeights(PCVeval_overQnum(CV4_j.train[[j]]))

  for(i in 1:3){
    CV4_j.train.weights[[j]][[i]]=round(CV4_j.train.weights[[j]][[i]], digits = 4)
  }

  CV4_j.probSequences=EvalSeqSubject(CV4_j.train.weights[[j]], CV4_j.test[[j]], 1)

  for(i in 1:623){
    CV4_j.probClasses.Convg[[i]]=convg(CV4_j.probSequences[[i]], 0.75)
  }

  for(i in 1:623){
    CV4_j.probClasses[i]=CV4_j.probClasses.Convg[[i]][[3]]
  }

  CV4_j.data.accuracy[j]=length(which(CV4_j.probClasses==CV4_j.test[[j]]$SupOutNum))/623

  CV4_j.data.accuracy.traditional[j]=length(which(CV4_j.test[[j]]$sumClassNum==CV4_j.test[[j]]$SupOutNum))/623
}

####  Accuracy Values
CV4.accuracy=mean(CV4_j.data.accuracy)
CV4.accuracy.traditional=mean(CV4_j.data.accuracy.traditional)
#-------------------------------------------------------------------------#
####	CVk	 ####
#-------------------------------------------------------------------------#

####  This Portion will now generalize to arbitrary k values
accuracy.ksets=c()
traditional.accuracy.ksets=c()
N.obs.k=c()
#lower.loop.limit=1225
#upper.loop.limit=1275
n.minus.one=24

N.set.arg=seq(from=1248, to=2490, length.out = n.minus.one)
N.set.arg=c(N.set.arg,1247)

for(i in 1:n.minus.one+1){
  N.set.arg[i]=floor(N.set.arg[i])
}

boot.sample.i=list()

for(i in 1:n.minus.one+1){
  boot.sample.i[[i]]=CVsplit(phq9, N.set.arg[i])
}


for(k in 1:n.minus.one+1){
  k.setVal=N.set.arg[k]
  k.index=k

  ####	Divide Data into K CV data sets
  #CVk.dat=CVsplit(phq9, k.setVal)
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

  N.obs.k[k.index]=Number.k.obs*(k.setVal-1)
}

N.obs.k=c()
for(k in 1:n.minus.one+1){
  N.obs.k[k]=length(boot.sample.i[[k]][[1]])
}

accuracy.df=data.frame(N.obs.k, accuracy.ksets)

zeros=rep(0, times=n.minus.one+1)
ones=rep(1, times=n.minus.one+1)
ID=as.factor(c(ones, zeros))
N.obs.train.k.lmFE=rep(N.obs.k, times=2)
accuracy.out.lmFE=c(accuracy.ksets,traditional.accuracy.ksets)
accuracy.df.lmFE=data.frame(N.obs.train.k, ID, accuracy.out)

accuracy.lm=lm(accuracy.ksets~N.obs.k, data = accuracy.df)
(accuracy.lms=summary(accuracy.lm))
accuracy.lmFE=lm(accuracy.out.lmFE~ID*N.obs.train.k.lmFE,
                 data = accuracy.df.lmFE)
(accuracy.lmFEs=summary(accuracy.lmFE))



accuracy.plot=ggplot(accuracy.df, aes(x=N.obs.k, y=accuracy.ksets))+
  xlab("Observations in Traing Set")+
  ylab("Accuracy of Prob.Scoring")+
  geom_abline(intercept = as.numeric(coef(accuracy.lm))[[1]],
              slope=as.numeric(coef(accuracy.lm))[[2]])+
  geom_point()

accuracy.plot

plot(accuracy.lm)


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
