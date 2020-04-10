####	MasterCVanalysis.R	 ####

#-------------------------------------------------------------------------#
####	Description:	 ####
# This script will combine the constituent functions, data, and variables needed in order to perform a cross-validation examination of Probabilistic Scoring.


####	Script Dependencies	 ####

# Package Dependencies:
library(ggplot2)
library(boot)
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


####	Create Bootsrap Sample Distributions	 ####
## This portion of code will subsample the given data.  Each subsample will include all colums of data, and the number of rows in each sub-sample will range from one to 2495.  Each subsample will be stored in its own list item so that a subsample distribution of weights may be generated in the following step.

n.minus.one=99
N.set.arg=seq(from=1248, to=2490, length.out = n.minus.one)
N.set.arg=c(N.set.arg,1247)

for(i in 1:n.minus.one+1){
  N.set.arg[i]=floor(N.set.arg[i])
}

boot.sample.i=list()

for(i in 1:n.minus.one+1){
  boot.sample.i[[i]]=CVsplit(phq9, N.set.arg[i])
}

# save(boot.sample.i, file = "/Users/lee/Desktop/BootSamplei.RData")
# load(file = "/Users/lee/Desktop/BootSamplei.RData")

boot.sample.train.i=list()
boot.sample.test.i=list()
boot.sample.Nset.i=list()

for(i in 1:n.minus.one+1){
  boot.sample.train.i[[i]]=boot.sample.i[[i]][[1]]
  boot.sample.test.i[[i]] =boot.sample.i[[i]][[2]]
  boot.sample.Nset.i[[i]] =boot.sample.i[[i]][[3]]
}


####	Weight Bootstrap Distributions	 ####
## We will now perform the weight calculations for each of the bootstrap subsample.

# i in 1:5
boot.weights.i=list()

for(i in 1:n.minus.one+1){
  # j in 1:N.set.arg[i]
  boot.weights.i.j=list()
  for(j in 1:N.set.arg[i]){
    boot.weights.i.j[[j]]=ReformatWeights(PCVeval_overQnum(boot.sample.train.i[[i]][[j]]))
    for(k in 1:3){
      boot.weights.i.j[[j]][[k]]=round(boot.weights.i.j[[j]][[k]], digits = 4)
    }
  }
  boot.weights.i[[i]]=boot.weights.i.j
}

save(boot.weights.i, file = "/Users/lee/Desktop/BootWeights15.Rdata")

# we now re-format these weights so that a distribution for each weight-variable may be obtained.

weight.out.i=list()
for(i in 1:n.minus.one+1){
  weight.out.i.k=list()
  for(k in 1:3){
    weight.out.i.k.j=list()
    for(j in 1:4){
      weight.out.i.k.j.l=list()
      for(l in 1:9){
        weight.out.i.k.j.l.h=c()
        for(h in 1:N.set.arg[i]){
          weight.out.i.k.j.l.h=append(weight.out.i.k.j.l.h,
                                      boot.weights.i[[i]][[h]][[k]][l,j])
        }
        weight.out.i.k.j.l[[l]]=weight.out.i.k.j.l.h
      }
      weight.out.i.k.j[[j]]=weight.out.i.k.j.l
    }
    weight.out.i.k[[k]]=weight.out.i.k.j
  }
  weight.out.i[[i]]=weight.out.i.k
}

# save(weight.out.i, file = "/Users/lee/Desktop/CoefWeightsi.Rdata")
# load(file="/Users/lee/Desktop/CoefWeightsi.Rdata")
# weights.out.k=list()
# for(k in 1:3){
#   weights.out.k.j=list()
#   for(j in 1:4){
#     weights.out.k.j.l=list()
#     for(l in 1:9){
#       weights.out.k.j.l.i=list()
#       for(i in 1:50){
#         weights.out.k.j.l.i.h=c()
#         for(h in 1:N.set.arg[i]){
#           weights.out.k.j.l.i.h=append(weights.out.k.j.l.i.h,
#                                             boot.weights.i[[i]][[h]][[k]][l,j])
#         }
#         weights.out.k.j.l.i[[i]]=weights.out.k.j.l.i.h
#       }
#       weights.out.k.j.l[[l]]=unlist(weights.out.k.j.l.i)
#     }
#     weights.out.k.j[[j]]=weights.out.k.j.l
#   }
#   weights.out.k[[k]]=weights.out.k.j
# }

# We will now calculate the variation of each value weight value at each level of training observation number

var.weight.i=list()
for(i in 1:n.minus.one+1){
  var.weight.i.j=list()
  for(j in 1:3){
    var.weight.i.j.k=list()
    for(k in 1:4){
      var.weight.i.j.k.l=c()
      for(l in 1:9){
        var.weight.i.j.k.l[l]=var(weight.out.i[[i]][[j]][[k]][[l]])
      }
      var.weight.i.j.k[[k]]=var.weight.i.j.k.l
    }
    var.weight.i.j[[j]]=var.weight.i.j.k
  }
  var.weight.i[[i]]=var.weight.i.j
}

## Choose a couple of Variance sequences to plot:
### in all three classes
###  Responses 2 and  4,
### for questions 3, 6, and 9 in

# X.Class.Response.Qnum
X.1.2.3=c()
X.1.2.6=c()
X.1.2.9=c()
X.1.4.3=c()
X.1.4.6=c()
X.1.4.9=c()
X.2.2.3=c()
X.2.2.6=c()
X.2.2.9=c()
X.2.4.3=c()
X.2.4.6=c()
X.2.4.9=c()
X.3.2.3=c()
X.3.2.6=c()
X.3.2.9=c()
X.3.4.3=c()
X.3.4.6=c()
X.3.4.9=c()

# var.weight.i[[Nsetval]][[class]][[response]][[qnumber]]

for(i in 1:n.minus.one+1){
  X.1.2.3[i]=var.weight.i[[i]][[1]][[2]][3]
  X.1.2.6[i]=var.weight.i[[i]][[1]][[2]][6]
  X.1.2.9[i]=var.weight.i[[i]][[1]][[2]][9]
  X.1.4.3[i]=var.weight.i[[i]][[1]][[4]][3]
  X.1.4.6[i]=var.weight.i[[i]][[1]][[4]][6]
  X.1.4.9[i]=var.weight.i[[i]][[1]][[4]][9]
  X.2.2.3[i]=var.weight.i[[i]][[2]][[2]][3]
  X.2.2.6[i]=var.weight.i[[i]][[2]][[2]][6]
  X.2.2.9[i]=var.weight.i[[i]][[2]][[2]][9]
  X.2.4.3[i]=var.weight.i[[i]][[2]][[4]][3]
  X.2.4.6[i]=var.weight.i[[i]][[2]][[4]][6]
  X.2.4.9[i]=var.weight.i[[i]][[2]][[4]][9]
  X.3.2.3[i]=var.weight.i[[i]][[3]][[2]][3]
  X.3.2.6[i]=var.weight.i[[i]][[3]][[2]][6]
  X.3.2.9[i]=var.weight.i[[i]][[3]][[2]][9]
  X.3.4.3[i]=var.weight.i[[i]][[3]][[4]][3]
  X.3.4.6[i]=var.weight.i[[i]][[3]][[4]][6]
  X.3.4.9[i]=var.weight.i[[i]][[3]][[4]][9]
}
df.var.X=data.frame(rbind(X.1.2.3,
                          X.1.2.6,
                          X.1.2.9,
                          X.1.4.3,
                          X.1.4.6,
                          X.1.4.9,
                          X.2.2.3,
                          X.2.2.6,
                          X.2.2.9,
                          X.2.4.3,
                          X.2.4.6,
                          X.2.4.9,
                          X.3.2.3,
                          X.3.2.6,
                          X.3.2.9,
                          X.3.4.3,
                          X.3.4.6,
                          X.3.4.9))

colnames(df.var.X)=N.set.arg
class.id=c(rep("1", times=6),
           rep("2", times=6),
           rep("3", times=6))
response=rep( c(rep("2", times=3) , rep("4", times=3)), times=3)
qnum=rep(c("3", "6", "9"), times=6)
df.var.X$class.id=class.id
df.var.X$response=response
df.var.X$qnum=qnum
df.var.re=melt(df.var.X, id=c("class.id","response","qnum"))
colnames(df.var.re)=c("class.id", "response", "qnum", "train.count", "variation")
p=ggplot(df.var.re, aes(x=train.count, y=variation))+
  geom_point(aes(shape=class.id, color=qnum))+
  geom_line()
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
