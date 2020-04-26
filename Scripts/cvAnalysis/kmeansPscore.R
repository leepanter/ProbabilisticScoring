# kmeansPscore.R
#-------------------------------------------------------------------------#
####	Description:	 ####
# This script will generate a clustering of the PHQ9 data using a K-means approach.  It will produce three clusters that will then be ordered according to the depression classification ranking based on empirical properties of the clusters.


####	Script Dependencies	 ####

# Package Dependencies:
library(factoextra)
library(tidyverse)
library(cluster)
library(dendextend)
library(ggplot2)

# Set Working Directory
WD="/Users/lee/Documents/GitHub/ProbabilisticScoring/Scripts/cvAnalysis"
# WD="/home/lpanter/PScoreUpdate1/GCloudUpdate"
setwd(WD)

# Data Dependencies:
# PHQ9 Data
source(file = "/Users/lee/Documents/GitHub/ProbabilisticScoring/Scripts/DataManagement/PHQ9/phq9DataSubsetImport.R")
# source(file = "phq9DataSubsetImport.R")

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
# source(file = "functionsWeightCalculations.R")

# Probabilistic Score Calculations
source(file = "/Users/lee/Documents/GitHub/ProbabilisticScoring/Scripts/cvAnalysis/functionsProbScoreCalc.R")
# source(file = "functionsProbScoreCalc.R")

# Scoring Analysis
source(file="/Users/lee/Documents/GitHub/ProbabilisticScoring/Scripts/cvAnalysis/functionsScoringAnalysis.R")
# source(file="functionsScoringAnalysis.R")
#-------------------------------------------------------------------------#
####	Begin Script	 ####
#-------------------------------------------------------------------------#

# load data
dat=phq9
dat.kmeans=dat[,4:12]

# K Means  ----------------------------------------------------------------

k.means.out=kmeans(dat.kmeans, centers=3, iter.max = 50, nstart = 100)

dat$kmeans=k.means.out$cluster
kmeans.factor=c()
for(i in 1:2495){
  if(dat$kmeans[i]==1){
    kmeans.factor[i]="C2"
  } else if(dat$kmeans[i]==2){
    kmeans.factor[i]="C1"
  } else{kmeans.factor[i]="C3"}
}

dat$kmeans.factor=kmeans.factor

accuracy.traditional.comp.kmeans=length(which(dat$kmeans.factor==dat$sumClassString))/2495

phq9$kmeans=dat$kmeans
phq9$kmeans.factor=dat$kmeans.factor

# fviz_cluster(k.means.out, data = dat.kmeans)
#
# pQ1=ggplot(dat, aes(x=Q1, y=qTot, color=kmeans.factor))+
#   geom_point()
# pQ1
# pQ2=ggplot(dat, aes(x=Q2, y=qTot, color=kmeans.factor))+
#   geom_point()
# pQ2
# pQ3=ggplot(dat, aes(x=Q3, y=qTot, color=kmeans.factor))+
#   geom_point()
# pQ3
# pQ4=ggplot(dat, aes(x=Q4, y=qTot, color=kmeans.factor))+
#   geom_point()
# pQ4
# pQ5=ggplot(dat, aes(x=Q5, y=qTot, color=kmeans.factor))+
#   geom_point()
# pQ5
# pQ6=ggplot(dat, aes(x=Q6, y=qTot, color=kmeans.factor))+
#   geom_point()
# pQ6
# pQ7=ggplot(dat, aes(x=Q7, y=qTot, color=kmeans.factor))+
#   geom_point()
# pQ7
# pQ8=ggplot(dat, aes(x=Q8, y=qTot, color=kmeans.factor))+
#   geom_point()
# pQ8
# pQ9=ggplot(dat, aes(x=Q9, y=qTot, color=kmeans.factor))+
#   geom_point()
# pQ9
#
# gridExtra::grid.arrange(pQ1, pQ2, pQ3,
#                         pQ4, pQ5, pQ6,
#                         pQ7, pQ8, pQ9, nrow=3)



#-------------------------------------------------------------------------#
# Kmeans CVk analysis -----------------------------------------------------
#-------------------------------------------------------------------------#
number.samples=25
sample.length=number.samples+2
sample.vec.k.sets=df.set.info$df.k.sets
sample.vec.k.sets=sample.vec.k.sets[-c(1,1245)]
N.set.arg=sort(sample(sample.vec.k.sets, sample.length, replace = FALSE))
N.set.arg=sort(c(N.set.arg,1247, 1248))

accuracy.ksets=c()
traditional.accuracy.ksets=c()
N.obs.k=c()
boot.sample.i=list()

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
    CVk_j.data.accuracy[j]=length(which(CVk_j.probClasses==CVk_j.test[[j]]$kmeans))/Number.k.obs

    ####  Calculate Accuracy of traditional Classes
    CVk_j.data.accuracy.traditional[j]=length(which(CVk_j.test[[j]]$sumClassNum==CVk_j.test[[j]]$kmeans))/Number.k.obs
  }

  ####  Output Accuracy Values
  CVk.accuracy=mean(CVk_j.data.accuracy)
  accuracy.ksets[k.index]=CVk.accuracy

  CVk.accuracy.traditional=mean(CVk_j.data.accuracy.traditional)
  traditional.accuracy.ksets[k.index]=CVk.accuracy.traditional

  N.obs.k[k.index]=Number.k.obs*(k.setVal-1)
}


#-------------------------------------------------------------------------#
####	REMOVE	 ####
#-------------------------------------------------------------------------#

accuracy.ksets=c()
traditional.accuracy.ksets=c()
N.obs.k=c()

k=1

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

N.obs.k[k.index]=Number.k.obs*(k.setVal-1)

#-------------------------------------------------------------------------#
####	REMOVE	 ####
#-------------------------------------------------------------------------#





N.obs.k=c()
for(k in 1:n.minus.one+1){
  N.obs.k[k]=length(boot.sample.i[[k]][[1]])
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

zeros=rep(0, times=n.minus.one+1)
ones=rep(1, times=n.minus.one+1)
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



# Heirarchical Clustering -------------------------------------------------

d=dist(dat.kmeans)
hc1=hclust(d, method = "complete")
plot(hc1)
sub_grp=cutree(hc1, k=3)
table(sub_grp)

dat=dat %>%
    mutate(hcluster = sub_grp)

hcluster.factor=c()
for(i in 1:2495){
  if(dat$hcluster[i]==1){
    hcluster.factor[i]="C3"
  } else if(dat$hcluster[i]==2){
    hcluster.factor[i]="C1"
  } else{hcluster.factor[i]="C2"}
}

dat$hcluster.factor=hcluster.factor

length(which(dat$hcluster.factor==dat$SupOutString))/2495


